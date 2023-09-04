####### First getting database containing apache variables. Will save it and use for further analysis.
postgres_conn <- postgres_connect(host = host,
                                  dbname = dbname,
                                  port = 5432,
                                  user = user,
                                  password = password)

### Getting dataset of physiology variables
data <- get_score_variables(postgres_conn, schema, start_date, end_date,
                            0, 1, "CCAA", "APACHE II")

###### Getting care site and death data and merging with main dataset.
raw_sql <- read_file("analysis/get_care_site_outcome.sql") %>%
  glue(schema = schema,
       start_date = start_date,
       end_date = end_date)
outcome_data <- dbGetQuery(postgres_conn, raw_sql)

data <- left_join(data, outcome_data, by = c("person_id", "visit_occurrence_id",
                                             "visit_detail_id", "icu_admission_datetime"))
save(data, file = "data/01_orig_data.RData")

#### Getting diagnosis data. Query is saved in sql file.
raw_sql <- read_file("analysis/get_diagnoses.sql") %>%
  glue(schema = schema,
       start_date = start_date,
       end_date = end_date)
#### Running the query
###### A bunch of patients appear to be missing diagnoses. Remember to look into why.
diag_data <- dbGetQuery(postgres_conn, raw_sql)
save(diag_data, file = "data/02_diag_data.RData")

dbDisconnect(postgres_conn)


####### Start with 254,766
###### Excluding everyone <18. 227,506
load("data/01_orig_data.RData")
load("data/02_diag_data.RData")
data <- data %>%
  filter(age >=18) %>%
  # Calculating a few variables I need.
  mutate(icu_outcome = if_else(!is.na(death_datetime) &
                                 death_datetime > icu_admission_datetime &
                                 death_datetime <= icu_discharge_datetime, "Dead", "Alive"),
         icu_los = as.numeric(difftime(icu_discharge_datetime, icu_admission_datetime,
                                       units = "days")),
         # This is just for the table one functions.
         gender = as.factor(gender))

##### Calculating apache II score.
## Note - I'm not sure what to do about WCC 1000/mcgl.
data <- fix_apache_ii_units(data)

#### FOR CCA ONLY. We don't collect paco2, so imputing it as 40 mmHg before we start the apache calculation. This allows the pao2 and fio2 to contribute to the score.
data <- data %>%
  mutate(min_paco2orig = min_paco2,
         max_paco2orig = max_paco2,
         min_paco2 = if_else(is.na(min_paco2orig), 40, min_paco2),
         max_paco2 = if_else(is.na(max_paco2orig), 40, max_paco2))

data <- calculate_apache_ii_score(data)

### Calculating apache II prob. Functions in apache_ii_prob file.
# download_mapping_files(snomed_mapping_path, ap2_path, ap2_coefs_path, output_path)
coef_data <- get_apache_ii_coefficents(diag_data, output_path)
# 39, 269 missing diagnoses.
data <- calculate_apache_ii_prob(data, coef_data)

##### For SMRs, summarising the data by care site ID
by_care_site <- summarise_by_unit(data)

######### Creating table one. Dividing by gender because I have to divide by something. Not planning to use it.
output <- make_output_df(data, "gender")
output <- get_count(data, "gender", "person_id", "Number of patients", output)
output <- get_unique_count(data, "gender", "care_site_id", "Number of sites", output)
output <- get_median_iqr(data, "gender", 'age', "Age", output, round =1)
output <- get_n_percent_value(data, 'gender', 'gender', "MALE", "Male", output, round =1)

### Scores
output <- get_median_iqr(data, "gender", 'apache_ii_score', "APACHE II score", output, round =1)
output <- get_median_iqr(data, "gender", 'apache_ii_prob',
                         "APACHE II probability of mortality", output, round =1)

#### SMR for APACHE II. Using the care site dataset.
#### Have to create it separately and paste it to the output dataset.
smr_row <-
  paste0(
  round(median(by_care_site$smr_ap2, na.rm = TRUE), 2), " (",
  round(quantile(by_care_site$smr_ap2, 0.25, na.rm = TRUE), 2), " - ",
  round(quantile(by_care_site$smr_ap2, 0.75, na.rm = TRUE), 2), ")")

smr_row <- c("APACHE II SMR Median (IQR)", smr_row, "", "")

output <- rbind(output, smr_row)

### Outcomes
output <- get_n_percent_value(data, 'gender', 'icu_outcome', "Dead", "ICU mortality",
                              output, round =1)
output <- get_median_iqr(data, "gender", 'icu_los', "ICU length of stay (Days)", output, round =1)


# writing the output data frame to an excel file
write.xlsx(output, file = "output/01_output.xlsx", borders = c("all"), colWidths = c("auto"),
           na.string = "-")

############ Getting availability and range of the physiology components of the APACHE II score.
########### min and max will have same availability
########## Doing a lot of messing around to get things into the format I want.
availability <-
  data %>%
  select(starts_with("max_")) %>%
  rename_all(~stringr::str_replace(.,"^max_","")) %>%
  summarise_all(list(availability = ~round(100*sum(!is.na(.))/nrow(data), 2),
                    min = ~round(min(., na.rm = TRUE), 2),
                    max = ~round(max(., na.rm = TRUE), 2)
                    )) %>%
  pivot_longer(
    cols = names(.),
    names_to = c("variable", "summary"),
    names_sep = "_") %>%
  pivot_wider(names_from = "summary", values_from = "value") %>%
  arrange(desc(availability))

# Writing the output out.
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "2_availability_apache")
writeData(wb, sheet = "2_availability_apache", x = availability, borders = "columns")
setColWidths(wb, "2_availability_apache", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

############ Graph of admission dates, just to make sure things look consistent.
admissions <-
  data %>%
  ggplot(aes(x = lubridate::floor_date(icu_admission_datetime, "month"))) +
  geom_line(stat = "count") +
  ylab("Patients per month") +
  xlab("Date")
theme_classic(admissions)
ggsave("output/02_number_of_patients.png")

##### Also doing a plot of histograms of values.
hist <-
  data %>%
  select(visit_detail_id, starts_with("min")) %>%
  pivot_longer(cols = !visit_detail_id) %>%
  filter(name != "min_paco2",
         name != "min_sbp",
         name != "min_dbp") %>%
  ### Should really join this to the units in the dataset instead of hardcoding.
  mutate(name = case_when(
    name == "min_hr" ~ "Heart rate",
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
  facet_wrap(~name, scales = "free")
custom_theme(hist)
ggsave("output/03_variable_distributions.png")

########## Funnel plot of SMRs.
by_care_site <- by_care_site %>%
  #### There are 15 sites with extremely high SMRs. Removing them so the rest of the graph is visible.
  filter(smr_ap2 < 5)
## Drawing the funnel plot
smr_graph(by_care_site)
ggsave("output/04_funnel_plot.png")
