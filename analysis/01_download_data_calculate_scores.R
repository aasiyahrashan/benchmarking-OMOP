####### First getting database containing apache variables. Will save it and use for further analysis.
postgres_conn <- postgres_connect(host = host,
                                  dbname = dbname,
                                  port = 5432,
                                  user = user,
                                  password = password)

### Getting dataset of physiology variables
data <- get_score_variables(postgres_conn, schema, start_date, end_date,
                            0, 1, "./data/ccaa_concepts.csv", "APACHE II")

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

#### Making sure everyone has a diagnosis recorded in the OMOP dataset.
missing_diag <- anti_join(data, diag_data,
                          by = c("person_id", "visit_occurrence_id", "visit_detail_id"))
if(nrow(missing_diag) > 1){
  warning("There are patients in the OMOP dataset who do not have a diangosis recorded in the condition occurrence or procedure occurrence tables. Please check the 'missing_diag' dataset for more information")
}

### This is a CCAA specific calculation of APACHE II coefficients.
###
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

###### Applying exclusion critera.
data <- apply_ccaa_specific_exclusions(data, output_path)

data <- data %>%
  filter(age >=18) %>%
  ###### For other datasets, replace with diagnosis name.
  ### Ensure this works for visit detail number 60186, person 59355.
  filter(!grepl("*burn*", extracted_apache_diag, ignore.case = TRUE)) %>%
  filter(!grepl("*burn*", extracted_snomed_diag, ignore.case = TRUE)) %>%
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
data <- calculate_apache_ii_prob(data)

######################### Multiple imputation APACHE II score
######################### Using predictive mean matching to impute missing physiology values.
### Technique taken from this book. https://stefvanbuuren.name/fimd/sec-nutshell.html
### As per 6.4, imputing physiology values instead of AP2 score or subscores so we don't lose the more granular information.
### As per chapter 6.3, deliberately including outcome variables as predictors for the imputation.

#### I've checked to make sure the min and max are sometimes different, and should therefore be imputed separately.

# Defining APACHE II physiology variables.
apache_vars <- c("max_temp", "min_temp", "min_wcc", "max_wcc",
                  "max_fio2", "min_paco2", "min_pao2", "min_hematocrit", "max_hematocrit",
                  "min_hr", "max_hr", "min_rr", "max_rr", "min_ph", "max_ph",
                  "min_bicarbonate", "max_bicarbonate", "min_sodium", "max_sodium",
                  "min_potassium", "max_potassium", "min_gcs", "min_creatinine",
                  "max_creatinine", "max_sbp", "min_sbp", "max_dbp", "min_dbp",
                  "count_comorbidity", "count_renal_failure", "count_emergency_admission")

mice_data <- data %>%
  select(person_id, visit_occurrence_id, visit_detail_id, care_site_id, admission_year,
         age, gender, icu_outcome, icu_los, !!apache_vars, ap2_diag_coef)

### Removing the following variables from either being predicted, or being predictors.
pred <- make.predictorMatrix(mice_data)

#### Removing the coef variable completely, since we don't want it imputed at all.
pred <- pred[, -which(colnames(pred) %in% c('person_id', 'visit_occurrence_id',
                                          'visit_detail_id', 'care_site_id', 'admission_year',
                                          'ap2_diag_coef'))]

pred <- pred[-which(rownames(pred) %in% c('person_id', 'visit_occurrence_id',
                                            'visit_detail_id', 'care_site_id', 'admission_year',
                                            'ap2_diag_coef')), ]

mice_data <- mice(mice_data, pred=pred, m = 30, maxit = 100,
                  method = "pmm", seed = 100)

### Converting to long format containing all imputed datasets stacked on top of each other, then
### calculating APACHE II score, then converting back to wide for further analysis/pooling.
### Instructions followed from section 6.4
### Using the long format is acceptable, since this is just a calclation of dervied variables per row, and there is no aggregation of results across rows.
mice_long <- complete(mice_data, "long", include = TRUE)
mice_long <- calculate_apache_ii_score(mice_long, imputation = "none")
mice_long <- calculate_apache_ii_prob(mice_long, imputation = 'none')
mice_data <- as.mids(mice_long)

save(mice_data, file = "data/03_mice_data.RData")
