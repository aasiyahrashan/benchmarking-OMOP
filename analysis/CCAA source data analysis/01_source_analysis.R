# Reading data in ---------------------------------------------------------
data <- read_csv("data/CoreForms.csv")
units_of_measure <- read_csv(glue("{output_path}/data/measures.csv"))

# Getting diagnosis data and diagnosis coefficents ------------------------
coef_data <- get_apache_ii_coefficents(data,
  dataset_name = dataset_name,
  source = "source",
  output_path = output_path
)
data <- left_join(data, coef_data, by = "patient_id")

# Applying exclusion criteria ---------------------------------------------
# First, calculating variables I need
data <- data %>%
  mutate(care_site_name = unitId,
    icu_los = as.integer(difftime(Discharge.date_of_discharge, date_of_admission,
                                  units = "days")),
    hospital_outcome = if_else(Discharge.discharge_status_hos == "Dead" |
                                 Discharge.discharge_status == "Dead",
                               "Dead", "Alive", "Alive"),
    admission_year = as.factor(year(date_of_admission))
  )

# Removing patients from ineligible sites.
data <- apply_ccaa_specific_exclusions(data, output_path)

# Removing readmissions and patients who wouldn't be in OMOP
data <- data %>%
  filter(is.na(Admission.readmission) | Admission.readmission != "Yes") %>%
  # Excluding patients who wouldn't be in OMOP.
  filter(Admission.gender %in% c("Male", "Female")) %>%
  filter(!is.na(Discharge.discharge_status))

# Excluding patients from countries with insufficent contributions.
patients_per_month_country <-
  data %>%
  mutate(
    country_fac = factor(country),
    admission_month = factor(lubridate::month(date_of_admission))
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

# Excluding patients with insufficent contributions
data <- data %>%
  left_join(patients_per_month_country,
            by = c("country", "admission_year")
  ) %>%
  filter(months_contributed_in_year >= 6 | (months_contributed_in_year >= 3 &
                                              admission_year == 2019))
# Exclusions based on age and diagnosis
data <- data %>%
  filter(Admission.age >= 18) %>%
  filter(
    !grepl("*burn*", primary_diagnosis_name, ignore.case = TRUE),
    !grepl("*cesarean section*", primary_diagnosis_name, ignore.case = TRUE),
    !grepl("*ectopic pregnancy*", primary_diagnosis_name, ignore.case = TRUE)
  ) %>%
  # Excluding patients without APACHE II diagnoses
  filter(!is.na(ap2_diag_coef))

# Calculating APACHE II --------------------------------
# Measure units file already downloaded with mapping data. Reading it in and joining
data <- left_join(data, units_of_measure, by = c("unitId" = "unit_id"))
data <- unit_conversion_source(admission = data)
data <- calculate_apache_ii_score_source(data)

# Output tables -----------------------------------------------------------
# Saving to the same output file for easy reference
