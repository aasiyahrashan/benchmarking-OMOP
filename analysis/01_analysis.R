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

#### Getting diagnosis data. Query is saved in sql file.
raw_sql <- read_file("analysis/get_diagnoses.sql") %>%
  glue(schema = schema,
       start_date = start_date,
       end_date = end_date)
#### Running the query
diag_data <- dbGetQuery(postgres_conn, raw_sql)

dbDisconnect(postgres_conn)

### This is a CCAA specific calculation of APACHE II coefficients.
### It also get the primary diagnosis for a patient.
download_mapping_files(freetext_mapping_path, snomed_mapping_path, ap2_path, ap2_coefs_path,
                       implementation_asia_path, implementation_africa_path,
                       output_path)

### This dataset needs variables called primary_diagnosis_name and ap_diag_coef.
coef_data <- get_apache_ii_coefficents(diag_data, output_path)
data <- left_join(data, coef_data, by = c("person_id", "visit_occurrence_id",
                                          "visit_detail_id"))
save(data, file = "data/01_orig_data.RData")
save(diag_data, file = "data/02_diag_data.RData")

################ Starting data analysis.
load("data/01_orig_data.RData")
load("data/02_diag_data.RData")

# ###### Getting free text diagnoses unmapped to SNOMED for Usagi.
# free_text <- diag_data %>%
#   filter(grepl("^disorder1,*", diagnosis_name) |
#          grepl("^operation1,*", diagnosis_name),
#          diagnosis_concept_id == 0) %>%
#   mutate(`Source term` = sub("^[^,]*,", "", diagnosis_name)) %>%
#   mutate(`Source term` = str_trim(`Source term`),
#          `Source term` = str_squish(`Source term`),
#          `Source term` = tolower(`Source term`),
#          `Source term` = str_replace_all(`Source term`, "[[:punct:]]", "")) %>%
#   group_by(`Source term`) %>%
#   summarise(frequency = n()) %>%
#   filter(frequency ==1)
#
# write_csv(free_text, file ="data/03_unmapped_freetext.csv")

###### Applying exclusion critera.
data <- apply_ccaa_specific_exclusions(data, output_path)

data <- data %>%
  filter(age >=18) %>%
  filter(!grepl("*burn*", primary_diagnosis_name, ignore.case = TRUE)) %>%
  # Calculating a few variables I need.
  mutate(icu_outcome = if_else(!is.na(death_datetime) &
                                 death_datetime > icu_admission_datetime &
                                 death_datetime <= icu_discharge_datetime, "Dead", "Alive"),
         icu_los = as.numeric(difftime(icu_discharge_datetime, icu_admission_datetime,
                                       units = "days")),
         admission_year = as.factor(year(icu_admission_datetime)))

##### Calculating apache II score.
data <- fix_apache_ii_units(data)
data <- fix_implausible_values_apache_ii(data)

#### FOR CCA ONLY. We don't collect paco2, so imputing it as 40 mmHg before we start the apache calculation. This allows the pao2 and fio2 to contribute to the score.
data <- data %>%
  mutate(min_paco2orig = min_paco2,
         max_paco2orig = max_paco2,
         min_paco2 = if_else(is.na(min_paco2orig), 40, min_paco2),
         max_paco2 = if_else(is.na(max_paco2orig), 40, max_paco2))

data <- calculate_apache_ii_score(data)
### Calculating apache II prob.
### The 'coef_dataset' for the APACHE II prob calculation needs to contain patient ID and the corresponding apache II diagnosis coefficent.
# 39, 269 missing diagnoses.
data <- calculate_apache_ii_prob(data, coef_data)

######################### Multiple imputation APACHE II score
######################### Using predictive mean matching to impute missing physiology values.
### Technique taken from this book. https://stefvanbuuren.name/fimd/sec-nutshell.html
### As per 6.4, imputing physiology values instead of AP2 score or subscores so we don't lose the more granular information.
### As per chapter 6.3, deliberately including outcome variables as predictors for the imputation.

#### TODO. CHECK the real data to see if min and max are always the same value.
#### This will mean there was only one variable collected, so only one variable needs to be imputed.
#### If not, both min and max need to be imputed.
#### TODO check other variable names and see if any of them can be included as predictor. Eg, comorbidities.

# Defining APACHE II variables.
apache <- c("max_temp", "min_temp", "min_wcc", "max_wcc",
            "max_fio2", "min_paco2", "min_pao2", "min_hematocrit", "max_hematocrit",
            "min_hr", "max_hr", "min_rr", "max_rr", "min_ph", "max_ph",
            "min_bicarbonate", "max_bicarbonate", "min_sodium", "max_sodium",
            "min_potassium", "max_potassium", "min_gcs", "min_creatinine",
            "max_creatinine", "sbp_max", "sbp_min", "dbp_max", "dbp_min")

mice_data <- data %>%
  select(person_id, age, gender, icu_outcome, icu_los, !!apache)

### Creating a predictor matrix and deliberately excluding the person_id variable as a predictor.
pred <- make.predictorMatrix(mice_data)
pred['person_id'] <- 0
mice_data <- mice(mice_data, pred=pred, m = 5, maxit = 1000,
                  method = "pmm", seed = 100)

### Converting to long format containing all imputed datasets stacked on top of each other, then
### calculating APACHE II score, then converting back to wide for further analysis/pooling.
### Instructions followed from section 6.4
### Using the long format is acceptable, since this is just a calclation of dervied variables per row, and there is no aggregation of results across rows.
mice_long <- complete(mice_data, "long", include = TRUE)
mice_long <- calculate_apache_ii_score(mice_long, imputation = "none")
mice_long <- calculate_apache_ii_prob(mice_long, coef_data)
mice_data <- as.mids(mice_long)

################# Calculating SMRs
##### For SMRs, summarising the data by care site ID
by_care_site <- data %>%
  group_by(care_site_id) %>%
  summarise(total = sum(!is.na(person_id)),
            # SMRs
            expected_ap2 = median(apache_ii_prob, na.rm = TRUE)*total,
            n_dead = sum(icu_outcome == "Dead" , na.rm = TRUE),
            smr_ap2 = n_dead/expected_ap2) %>%
  ungroup()

######### Creating table one. Dividing by gender because I have to divide by something. Not planning to use it.
output <- make_output_df(data, "admission_year")
output <- get_count(data, "admission_year", "person_id", "Number of patients", output)
output <- get_unique_count(data, "admission_year", "care_site_id", "Number of sites", output)
output <- get_median_iqr(data, "admission_year", 'age', "Age", output, round =1)
output <- get_n_percent_value(data, 'admission_year', 'gender', "MALE", "Male", output, round =1)

### Scores
output <- get_median_iqr(data, "admission_year",
                         'apache_ii_score', "APACHE II score", output, round =1)
output <- get_median_iqr(data, "admission_year", 'apache_ii_prob',
                         "APACHE II probability of mortality", output, round =1)

#### SMR for APACHE II. Using the care site dataset.
#### Have to create the row separately and paste it to the output dataset.
smr_row <-
  paste0(
  round(median(by_care_site$smr_ap2, na.rm = TRUE), 2), " (",
  round(quantile(by_care_site$smr_ap2, 0.25, na.rm = TRUE), 2), " - ",
  round(quantile(by_care_site$smr_ap2, 0.75, na.rm = TRUE), 2), ")")

smr_row <- c("APACHE II SMR Median (IQR)", smr_row, "", "")

output <- rbind(output, smr_row)

### Outcomes
output <- get_n_percent_value(data, 'admission_year', 'icu_outcome', "Dead", "ICU mortality",
                              output, round =1)
output <- get_median_iqr(data, "admission_year", 'icu_los',
                         "ICU length of stay (Days)", output, round =1)


# writing the output data frame to an excel file
write.xlsx(output, file = "output/01_output.xlsx", borders = c("all"), colWidths = c("auto"),
           na.string = "-")

############ Getting availability and range of the physiology components of the APACHE II score.
availability <- get_physiology_variable_availability(data)

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
hist <- get_physiology_variable_distributions(data)
custom_theme(hist)
ggsave("output/03_variable_distributions.png")

########## Funnel plot of SMRs.
by_care_site <- by_care_site %>%
  #### There are 15 sites with extremely high SMRs. Removing them so the rest of the graph is visible.
  filter(smr_ap2 < 5)
## Drawing the funnel plot
smr_graph(by_care_site)
ggsave("output/04_funnel_plot.png")
