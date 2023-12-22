# Reading data in ---------------------------------------------------------
data <- read_csv("data/CoreForms.csv")

# Getting diagnosis data and diagnosis coefficents ------------------------
coef_data <- get_apache_ii_coefficents(data,
  dataset_name = dataset_name,
  source = "source",
  output_path = output_path
)
data <- left_join(data, coef_data, by = "patient_id")

# Applying exclusion criteria ---------------------------------------------
# Renaming unit ID variable to care site so I can use existing exclusion
# critera function
data <- data %>%
  mutate(care_site_name = unitId)

data <- apply_ccaa_specific_exclusions(data, output_path)

# Removing readmissions
data <- data %>%
  filter(is.na(Admission.readmission) | Admission.readmission != "Yes")

# Exclusions based on age and diagnosis
data <- data %>%
  filter(Admission.age >= 18) %>%
  filter(Admission.gender %in% c("Male", "Female")) %>%
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
