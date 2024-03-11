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
conn <- omop_connect(
  driver = driver,
  host = host,
  dbname = dbname,
  port = port,
  user = user,
  password = password
)

# Getting physiology variables data set -----------------------------------
data <- get_score_variables(
  conn, dialect, schema, start_date, end_date,
  0, 1, concepts_path, "APACHE II",
  age_method = "year_only"
)

##### Getting care site and death data and merging with main data set.
# Editing date arugments to add single quotes for SQL
 start_date_sql <- single_quote(start_date)
 end_date_sql <- single_quote(end_date)

 raw_sql <- read_file("analysis/get_care_site_outcome.sql") %>%
   SqlRender::translate(tolower(dialect)) %>%
   SqlRender::render(
     schema = schema,
     start_date = start_date_sql,
     end_date = end_date_sql
   )

 outcome_data <- dbGetQuery(conn, raw_sql)

 data <- left_join(data,
   outcome_data,
   by = c(
     "person_id",
     "visit_occurrence_id",
     "visit_detail_id",
     "icu_admission_datetime"
   )
 )

 # Getting diagnosis data --------------------------------------------------
 # Query is saved in SQL file.
 raw_sql <- read_file("analysis/get_diagnoses.sql") %>%
   SqlRender::translate(tolower(dialect)) %>%
   SqlRender::render(
     dbname = dbname,
     schema = schema,
     start_date = start_date_sql,
     end_date = end_date_sql
   )

 #### Running the query
 diag_data <- dbGetQuery(conn, raw_sql)

 #### NICE exclusions need the postgres connection, so running before disconnect.
 if (dataset_name == "NICE") {
   data <- apply_nice_specific_exclusions(conn, data)
   diag_data <- apply_nice_specific_exclusions(conn, diag_data)
   diag_data <- diag_data %>%
     filter(between(diagnosis_concept_id, 2000000074, 2000000127))
 }

 #### CCAA exclusions also need a separate SQL query
if (dataset_name == "CCAA") {

   # Finding out if patients were readmitted to ICU
   raw_sql <- glue("SELECT DISTINCT person_id
                                    ,visit_occurrence_id
                                    ,visit_detail_id
                                    , observation_datetime as readmission_datetime
                                    , value_as_string as readmission
                      FROM cca_omop.observation
                     WHERE observation_concept_id = 2000000042")

   readmission_dataset <- dbGetQuery(conn, raw_sql)

   # Removing readmissions
   data <- data %>%
     left_join(readmission_dataset,
               by = c("person_id",
                      "visit_occurrence_id",
                      "visit_detail_id")) %>%
     filter(is.na(readmission) |
              readmission == "No" |
              (readmission == "Yes" &
                 readmission_datetime != icu_admission_datetime))
   # The same CCAA patient ID can be joined to more than one other for readmissions.
   # Since this study keeps the first only, I'm removing the duplicates.
   data <- data %>%
     mutate(patient_id = str_extract(person_source_value, "^[^ ]+")) %>%
     arrange(patient_id, icu_admission_datetime, icu_discharge_datetime) %>%
     distinct(patient_id, .keep_all = TRUE)

   download_mapping_files(
     freetext_mapping_path,
     snomed_mapping_path,
     ap2_path,
     ap2_coefs_path,
     implementation_asia_path,
     implementation_africa_path,
     units_of_measure_path,
     output_path
   )
   data <- apply_ccaa_specific_exclusions(data, output_path)
   }

dbDisconnect(conn)

#### Making sure everyone has a diagnosis recorded in the OMOP data set.
missing_diag <- anti_join(data,
  diag_data,
  by = c(
    "person_id",
    "visit_occurrence_id",
    "visit_detail_id"
  )
)

if (nrow(missing_diag) > 1) {
  warning(
    "There are patients in the dataset who have no recorded diagoses",
    " in the condition occurrence or procedure occurrence tables.",
    "\nPlease check the 'missing_diag' dataset for more information"
  )
}

# This needs to include variables called primary_diagnosis_name and ap_diag_coef.
coef_data <- get_apache_ii_coefficents(diag_data, dataset_name,
                                       output_path = output_path)
data <- left_join(data,
  coef_data,
  by = c(
    "person_id",
    "visit_occurrence_id",
    "visit_detail_id"
  )
)
save(data, file = "data/01_orig_data.RData")
save(diag_data, file = "data/02_diag_data.RData")


# Data analysis -----------------------------------------------------------
load("data/01_orig_data.RData")


# Exclusion criteria ------------------------------------------------------
# Calculating variables needed for later
data <- data %>%
  mutate(
    icu_outcome = if_else(!is.na(death_datetime) &
      death_datetime > icu_admission_datetime &
      death_datetime <= icu_discharge_datetime,
    "Dead",
    "Alive"
    ),
    icu_los = as.numeric(difftime(icu_discharge_datetime,
      icu_admission_datetime,
      units = "days"
    )),
    hospital_outcome = if_else((!is.na(death_datetime) &
      death_datetime > hospital_admission_datetime &
      death_datetime <= hospital_discharge_datetime) |
      icu_outcome == "Dead",
    "Dead",
    "Alive"
    ),
    admission_year = as.factor(year(icu_admission_datetime))
  )

# Getting first ICU admission per patient.
data <- data %>%
  arrange(person_id, visit_occurrence_id, icu_admission_datetime) %>%
  distinct(person_id, visit_occurrence_id, .keep_all = TRUE)

# Excluding patients from countries with insufficent contributions.
# # This is based on the ICNARC report. https://www.google.com/url?q=https://onlinereports.icnarc.org/Reports/2019/12/annual-quality-report-201819-for-adult-critical-care&sa=D&source=docs&ust=1698589035706375&usg=AOvVaw3Zu-zA_qy5M02R9HGsMLZP
# First calculating contributions over time.
patients_per_month_country <-
  data %>%
  mutate(
    country_fac = factor(country),
    admission_month = factor(lubridate::month(icu_admission_datetime))
  ) %>%
  group_by(country_fac, country, admission_year, admission_month,
    .drop = FALSE
  ) %>%
  summarize(n_admissions = n()) %>%
  arrange(country, admission_year, admission_month) %>%
  group_by(country) %>%
  mutate(
    percent_change_last_month =
      ((n_admissions - lag(n_admissions)) / lag(n_admissions)) * 100,
    percent_change_next_month =
      ((n_admissions - lead(n_admissions)) / lead(n_admissions)) * 100
  ) %>%
  # Only allowing months with admissions which haven't
  # decreased more than 60% compared to previous or next month to
  # count as contributions.
  mutate(
    contributed =
      case_when(
        n_admissions < 5 ~ FALSE,
        percent_change_last_month < -60 ~ FALSE,
        percent_change_next_month < -60 ~ FALSE,
        TRUE ~ TRUE
      ),
    date = as.Date(paste0(
      as.character(admission_year), "-",
      as.character(admission_month), "-", "01"
    ))
  ) %>%
  group_by(country, admission_year) %>%
  summarise(months_contributed_in_year = sum(contributed))

# Excluding patients with insufficient contributions
data <- data %>%
  left_join(patients_per_month_country,
    by = c("country", "admission_year")
  ) %>%
  filter(months_contributed_in_year >= 6 | (months_contributed_in_year >= 3 &
    admission_year == 2019))

# Applying patient specific exclusion critera
data <- data %>%
  filter(age >= 17) %>%
  # Removing diagsnoses which can't have APACHE II calculated on them.
  filter(
    !grepl("*burn*", primary_diagnosis_name, ignore.case = TRUE)
  ) %>%
  # Excluding patients without APACHE II diagnoses
  filter(!is.na(ap2_diag_coef))

# APACHE II score and prob ------------------------------------------------
data <- fix_apache_ii_units(data)

# Getting availability before removing implausible values
availability_orig <- get_physiology_variable_availability(data)
data <- fix_implausible_values_apache_ii(data)
save(availability_orig, file = "data/03_original_physiology_availability.RData")

#### FOR CCA ONLY. We don't collect paco2, so imputing it as 40 mmHg before we
#### start the apache calculation. This allows the pao2 and fio2 to contribute
#### to the score.
if (dataset_name == "CCAA") {
  data <- data %>%
    mutate(
      min_paco2orig = min_paco2,
      max_paco2orig = max_paco2,
      min_paco2 = if_else(is.na(min_paco2orig), 40, min_paco2),
      max_paco2 = if_else(is.na(max_paco2orig), 40, max_paco2)
    )
}

### Calculating apache II prob.
data <- calculate_apache_ii_score(data)
data <- calculate_apache_ii_prob(data)

save(data, file = "data/04_cleaned_filtered_data.RData")

# Multiple imputation APACHE II score -------------------------------------
# Using predictive mean matching to impute missing physiology values.
# Technique taken from this book. https://stefvanbuuren.name/fimd/sec-nutshell.html
# As per 6.4, imputing physiology values instead of AP2 score or subscores so we
# don't lose the more granular information.
# As per chapter 6.3, deliberately including outcome variables as predictors for
# the imputation.
# I've checked to make sure the min and max are sometimes different, and should
# therefore be imputed separately.

# Defining APACHE II physiology variables.
apache_vars <- c(
  "max_temp", "min_temp", "min_wcc", "max_wcc",
  "max_fio2", "min_paco2", "min_pao2", "min_hematocrit",
  "max_hematocrit", "min_hr", "max_hr", "min_rr", "max_rr", "min_ph",
  "max_ph", "min_bicarbonate", "max_bicarbonate", "min_sodium",
  "max_sodium", "min_potassium", "max_potassium", "min_gcs",
  "min_creatinine", "max_creatinine", "count_comorbidity", "count_renal_failure",
  "count_emergency_admission"
)

if ("max_sbp" %in% colnames(data)) {
  apache_vars <- append(apache_vars, c("max_sbp", "min_sbp", "max_dbp", "min_dbp"))
} else {
  apache_vars <- append(apache_vars, c("max_map", "min_map"))
}

mice_data <- data %>%
  select(
    person_id, visit_occurrence_id, visit_detail_id, country,
    admission_year, age, gender, icu_outcome, icu_los, !!apache_vars,
    ap2_diag_coef
  )

### Removing the following variables from either being predicted, or being predictors.
pred <- make.predictorMatrix(mice_data)

#### Removing variables from the prediction.
excl_vars <-
  c("person_id", "visit_occurrence_id", "visit_detail_id",
    "admission_year", "ap2_diag_coef")
if(!length(unique(data$country)) > 1) excl_vars <- append(excl_vars, "country")

pred[, excl_vars] <- 0
pred[excl_vars, ] <- 0

mice_data <- mice(mice_data,
  pred = pred, m = 30, maxit = 100,
  method = "pmm", seed = 100
)

### Converting to long format containing all imputed datasets stacked on top of
### each other, then calculating APACHE II score, then converting back to wide
### for further analysis/pooling.
### Instructions followed from section 6.4
### Using the long format is acceptable, since this is just a calculation of
### derived variables per row, and there is no aggregation of results across rows.
mice_long <- complete(mice_data, "long", include = TRUE)
mice_long <- calculate_apache_ii_score(mice_long, imputation = "none")
mice_long <- calculate_apache_ii_prob(mice_long, imputation = "none")
mice_data <- as.mids(mice_long)

save(mice_data, file = "data/05_mice_data.RData")
