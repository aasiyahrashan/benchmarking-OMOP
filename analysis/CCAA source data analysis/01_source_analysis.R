# Reading data in ---------------------------------------------------------
# Specifying variable ID as a character.
column_types <- cols(
  starts_with("Admission.snomed_diagnosis_concept_id") = col_character(),
  .default = col_guess()
)
data <- read_csv("data/CoreForms.csv", col_types = column_types)
sari_data <- read_csv("data/SariAdmissionAssessment.csv")
units_of_measure <- read_csv(glue("{output_path}/data/measures.csv"))

data <- left_join(data, sari_data,
                  by = join_by(patient_id, unitId, hospitalId,
                               date_of_admission, date_of_admission_hospital,
                               registry))

# Getting diagnosis data and diagnosis coefficents ------------------------
coef_data <- get_apache_ii_coefficents(data,
  dataset_name = dataset_name,
  source = "source",
  output_path = output_path
)
data <- left_join(data, coef_data, by = "patient_id")

# Applying exclusion criteria ---------------------------------------------
# First, calculating variables I need. Some just renames to work with OMOP functions
data <- data %>%
  filter(date_of_admission >= start_date & date_of_admission <= end_date) %>%
  mutate(
    care_site_name = unitId,
    # NOTE - This needs fixing - but leaving it for the moment
    emergency_admission = if_else(Admission.emergency_surgery == "Yes", 1, 0),
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
  filter(!is.na(Discharge.date_of_discharge))

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
data <- calculate_apache_ii_prob(data)


# Calculating SMRs --------------------------------------------------------
smrs_ni <- data %>%
  group_by(country, admission_year) %>%
  summarise(
    total = sum(!is.na(patient_id)),
    # SMRs
    median_ap2_score = median(apache_ii_score, na.rm = TRUE),
    median_ap2_prob = median(apache_ii_prob * 100, na.rm = TRUE),
    expected_ap2 = median(apache_ii_prob, na.rm = TRUE) * total,
    n_dead = sum(Discharge.discharge_status == "Dead", na.rm = TRUE),
    percent_dead = 100 * sum(Discharge.discharge_status == "Dead", na.rm = TRUE) / total,
    smr_ap2 = n_dead / expected_ap2
  ) %>%
  ungroup()

# Output tables -----------------------------------------------------------
output <- make_output_df(data, "admission_year")
output <- get_count(
  data, "admission_year", "patient_id",
  "Number of patients", output
)
output <- get_unique_count(
  data, "admission_year", "country",
  "Number of countries", output
)
output <- get_median_iqr(data, "admission_year", "Admission.age",
                         "Age", output,
                         round = 2
)
output <- get_n_percent_value(data, "admission_year", "Admission.gender", "Male",
                              "Male", output,
                              round = 2
)

output <- get_median_iqr(data, "admission_year",
                         "apache_ii_score", "APACHE II score", output,
                         round = 2
)
output <- get_median_iqr(data, "admission_year", "apache_ii_prob",
                         "APACHE II probability of mortality", output,
                         round = 2
)

#### SMR. Using the country dataset.
#### Have to create the row separately and paste it to the output dataset.
smr_ni_output <- make_output_df(smrs_ni, "admission_year")
smr_ni_output <- get_median_iqr(smrs_ni, "admission_year",
                                "smr_ap2", "APACHE II SMR Median (IQR)", smr_ni_output,
                                round = 2
)
names(smr_ni_output) <- names(output)
output <- rbind(output, smr_ni_output[1, ])

### Outcomes
output <- get_n_percent_value(data, "admission_year",
                              "Discharge.discharge_status",
                              "Dead", "ICU mortality",
                              output,
                              round = 2
)
output <- get_median_iqr(data, "admission_year", "icu_los",
                         "ICU length of stay (Days)", output,
                         round = 2
)
output <- get_n_percent_value(data, "admission_year", "hospital_outcome",
                              "Dead", "Hospital mortality",
                              output,
                              round = 2
)
# writing the output data frame to an excel file
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "3_source_data_tableone")
writeData(wb, sheet = "3_source_data_tableone", x = output, borders = "columns")
setColWidths(wb, "3_source_data_tableone", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# Getting availability and range of the physiology components of the APACHE II score.
availability <- get_physiology_variable_availability_source(data)

# Writing the output out.
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "4_availability_apache_source")
writeData(wb, sheet = "4_availability_apache_source", x = availability,
          borders = "columns")
setColWidths(wb, "4_availability_apache_source", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# Funnel plot of SMRs NI.
smr_graph(smrs_ni, "expected_ap2", "APACHE II normal imputation")
ggsave("output/06_funnel_plot_source_data.png")

