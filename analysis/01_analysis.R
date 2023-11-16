# ------------------------------------------------------------------------------
# Benchmarking analyses
#
# By:        Aasiyah Rashan  <aasiyah@nicslk.com>
# Edited by: Daniel Puttmann <d.p.puttmann@amstedamumc.nl>
#
# Date of last edit: 2023-10-31
# ------------------------------------------------------------------------------
# Connecting to database with APACHE variables ----------------------------
# The connection will be saved for further analysis.
conn <- omop_connect(driver = driver,
                     host = host,
                     dbname = dbname,
                     port = port,
                     user = user,
                     password = password)


# Getting physiology variables data set -----------------------------------
data <-  get_score_variables(conn, driver, schema,
                             start_date, end_date,
                             0, 1,
                             concepts_path,
                             "APACHE II")

###### Getting care site and death data and merging with main data set.
raw_sql <- read_file("analysis/get_care_site_outcome.sql") %>%
  glue(schema     = schema,
       start_date = start_date,
       end_date   = end_date)

outcome_data <- dbGetQuery(conn, raw_sql)
data <- left_join(data,
                  outcome_data,
                  by = c("person_id",
                         "visit_occurrence_id",
                         "visit_detail_id",
                         "icu_admission_datetime"))

# Getting diagnosis data --------------------------------------------------
# Query is saved in SQL file.
if (grep("SQL Server", driver, ignore.case = TRUE)) {
  sql_query_file <-
    "analysis/get_diagnoses_sql_server.sql"
} else {
  sql_query_file <-
    "analysis/get_diagnoses.sql"
}

raw_sql <- read_file (sql_query_file) %>%
  glue(dbname     = dbname,
       schema     = schema,
       start_date = start_date,
       end_date   = end_date)

#### Running the query
diag_data <- dbGetQuery(conn, raw_sql)

if (dataset_name == "NICE"){
  data <- apply_nice_specific_exclusions(conn, data)
  diag_data <- apply_nice_specific_exclusions(conn, diag_data)
  diag_data <- diag_data %>%
    filter(between(diagnosis_concept_id, 2000000074, 2000000127))
}

dbDisconnect(conn)

#### Making sure everyone has a diagnosis recorded in the OMOP data set.
missing_diag <- anti_join(data,
                          diag_data,
                          by = c("person_id",
                                 "visit_occurrence_id",
                                 "visit_detail_id"))

if (nrow(missing_diag) > 1) {
  warning("There are patients in the dataset who have no recorded diagoses",
          " in the condition occurrence or procedure occurrence tables.",
          "\nPlease check the 'missing_diag' dataset for more information")

  data <- anti_join(data,
                    missing_diag,
                    by = c("person_id",
                           "visit_occurrence_id",
                           "visit_detail_id"))
}

### This is a CCAA specific calculation of APACHE II coefficients.
if (dataset_name == "CCAA") {
  download_mapping_files(freetext_mapping_path,
                         snomed_mapping_path,
                         ap2_path,
                         ap2_coefs_path,
                         implementation_asia_path,
                         implementation_africa_path,
                         output_path)
}

### This data set needs the variables:
### 'primary_diagnosis_name' & 'ap_diag_coef'.
coef_data <- get_apache_ii_coefficents(diag_data, output_path)
data <- left_join(data,
                  coef_data,
                  by = c("person_id",
                         "visit_occurrence_id",
                         "visit_detail_id"))

save(data,      file = "data/01_orig_data.RData")
save(diag_data, file = "data/02_diag_data.RData")

# Starting data analysis.--------------------------------------------------
load("data/01_orig_data.RData")
load("data/02_diag_data.RData")

###### Applying exclusion criteria.
if (dataset_name == "CCAA") {
  data <- apply_ccaa_specific_exclusions(data, output_path)
}

data <- data %>%
  filter(age >=18) %>%
  ###### For other data sets, replace with diagnosis name.
  ### Ensure this works for visit detail number 60186, person 59355.
  filter(!grepl("*burn*",
                if (dataset_name == "CCAA") {
                  extracted_apache_diag
                } else {
                  primary_diagnosis_name
                },
                ignore.case = TRUE)) %>%
  # Calculating a few variables I need.
  mutate(icu_outcome = if_else(!is.na(death_datetime)
                               & death_datetime > icu_admission_datetime
                               & death_datetime <= icu_discharge_datetime,
                               "Dead",
                               "Alive"),
         icu_los = as.numeric(difftime(icu_discharge_datetime,
                                       icu_admission_datetime,
                                       units = "days")),
         admission_year = as.factor(year(icu_admission_datetime)))

##### Calculating apache II score.
data <- fix_apache_ii_units(data)
data <- fix_implausible_values_apache_ii(data)

#### FOR CCA ONLY. We don't collect paco2, so imputing it as 40 mmHg before we
#### start the apache calculation. This allows the pao2 and fio2 to contribute
#### to the score.
if (dataset_name == "CCAA") {
  data <- data %>%
    mutate(min_paco2orig = min_paco2,
           max_paco2orig = max_paco2,
           min_paco2 = if_else(is.na(min_paco2orig), 40, min_paco2),
           max_paco2 = if_else(is.na(max_paco2orig), 40, max_paco2))
}

### Calculating apache II probabilities.
data <- calculate_apache_ii_score(data)

### Calculating apache II prob.
### The 'coef_dataset' for the APACHE II prob calculation needs to contain
### patient ID and the corresponding apache II diagnosis coefficent.
# 39, 269 missing diagnoses.
data <- calculate_apache_ii_prob(data, coef_data)

##### For SMRs, summarising the data by care site ID
by_care_site <- data %>%
  group_by(care_site_id) %>%
  summarise(total        = sum(!is.na(person_id)),
            # SMRs
            expected_ap2 = median(apache_ii_prob,     na.rm = TRUE) * total,
            n_dead       = sum(icu_outcome == "Dead", na.rm = TRUE),
            smr_ap2      = n_dead / expected_ap2) %>%
  ungroup()

######### Creating table one. Dividing by gender because I have to divide by
######### something. Not planning to use it.
######### data should include male or female only
if (!is.factor(data$gender)){
  data <- data %>%
    filter(gender != "No matching concept")
  data$gender <- factor(data$gender, labels = c("MALE", "FEMALE"))
}
output <- make_output_df(data, "gender")

output <- get_count(data, "gender", "person_id", "Number of patients",
                    output)

output <- get_unique_count(data, "gender", "care_site_id", "Number of sites",
                           output)

output <- get_median_iqr(data, "gender", "age", "Age",
                         output, round = 2)

output <- get_n_percent_value(data, "gender", "gender", "MALE", "Male",
                              output, round = 2)

### Scores
output <- get_median_iqr(data, "gender", "apache_ii_score", "APACHE II score",
                         output, round = 2)
output <- get_median_iqr(data, "gender", "apache_ii_prob",
                         "APACHE II probability of mortality",
                         output, round = 2)

#### SMR for APACHE II. Using the care site dataset.
#### Have to create the row separately and paste it to the output dataset.
smr_row <- paste0(round(median(by_care_site$smr_ap2, na.rm = TRUE), 2),
                  " (",
                  round(quantile(by_care_site$smr_ap2, 0.25, na.rm = TRUE), 2),
                  " - ",
                  round(quantile(by_care_site$smr_ap2, 0.75, na.rm = TRUE), 2),
                  ")")

smr_row <- c("APACHE II SMR Median (IQR)", smr_row, "", "")

output <- rbind(output, smr_row)

### Outcomes
output <- get_n_percent_value(data, "gender", "icu_outcome", "Dead",
                              "ICU mortality",
                              output, round = 2)
output <-get_median_iqr(data, "gender", "icu_los", "ICU length of stay (Days)",
                        output, round = 2)

# writing the output data frame to an excel file
openxlsx::write.xlsx(output,
                     file = "output/01_output.xlsx",
                     borders = c("all"),
                     colWidths = c("auto"),
                     na.string = "-")

############ Getting availability and range of the physiology components of the
############ APACHE II score. Min and max will have same availability
########## Doing a lot of messing around to get things into the format I want.
availability <- data %>%
  select(starts_with("max_")) %>%
  rename_all(~ stringr::str_replace(., "^max_", "")) %>%
  summarise_all(list(
    availability = ~ round(100 * sum(!is.na(.)) / nrow(data), 2),
    min = ~ round(min(., na.rm = TRUE), 2),
    max = ~ round(max(., na.rm = TRUE), 2))) %>%
  pivot_longer(cols = names(.),
               names_to = c("variable", "summary"),
               names_sep = "_") %>%
  pivot_wider(names_from = "summary", values_from = "value") %>%
  arrange(desc(availability))

# Writing the output out.
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "2_availability_apache")
writeData(wb,
          sheet   = "2_availability_apache",
          x       = availability,
          borders = "columns")
setColWidths(wb,
             "2_availability_apache",
             cols   = 1:6,
             widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

########## The datetime sometimes gets mistranslated to character values
########## We change them back to datetimes here
if (is.character(data$icu_admission_datetime)){
  data$icu_admission_datetime <- ymd_hms(data$icu_admission_datetime,
                                         tz=Sys.timezone())
}

############ Graph of admission dates, just to make sure things look consistent.
admissions <- data %>%
  ggplot(aes(x = lubridate::floor_date(icu_admission_datetime, "month"))) +
  geom_line(stat = "count") +
  ylab("Patients per month") +
  xlab("Date") +
  theme_classic()

ggsave("output/02_number_of_patients.png")

##### Also doing a plot of histograms of values.
hist <- data %>%
  select(visit_detail_id, starts_with("min")) %>%
  pivot_longer(cols = !visit_detail_id) %>%
  filter(name != "min_paco2",
         name != "min_sbp",
         name != "min_dbp") %>%
  ### Should really join this to the units in the dataset instead of hardcoding.
  mutate(name = case_when(name == "min_hr" ~ "Heart rate",
                          name == "min_bicarbonate" ~ "Bicarbonate mmol/L",
                          name == "min_creatinine" ~ "Creatinine mg/dL",
                          name == "min_map" ~ "Mean arterial pressure",
                          name == "min_fio2" ~ "FiO2",
                          name == "min_gcs" ~ "GCS",
                          name == "min_hematocrit" ~ "Hematocrit %",
                          name == "min_hr" ~ "Heart rate",
                          name == "min_paco2orig" ~ "PaCO2 mmHg",
                          name == "min_pao2" ~ "PaO2 mmHg",
                          name == "min_ph" ~ "pH",
                          name == "min_potassium" ~ "Potassium mmol/L",
                          name == "min_rr" ~ "Respiratory rate",
                          name == "min_sodium" ~ "Sodium mmol/L",
                          name == "min_temp" ~ "Temperature C",
                          name == "min_wcc" ~ "White cell count 10^9/L")) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free") +
  theme_classic()

ggsave("output/03_variable_distributions.png")

########## Funnel plot of SMRs.
by_care_site <- by_care_site %>%
  #### There are 15 sites with extremely high SMRs.
  #### Removing them so the rest of the graph is visible.
  filter(smr_ap2 < 5)

## Drawing the funnel plot
smr_graph(by_care_site)
ggsave("output/04_funnel_plot.png")
