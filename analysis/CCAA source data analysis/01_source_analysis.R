# Reading data in ---------------------------------------------------------
# Specifying variable ID as a character.
column_types <- cols(
  "Admission.snomed_diagnosis_concept_id" = col_character(),
  "Admission.snomed_diagnosis_concept_id1" = col_character(),
  "Admission.snomed_diagnosis_concept_id2" = col_character(),
  "Admission.snomed_diagnosis_concept_id3" = col_character(),
  "Admission.snomed_diagnosis_concept_id4" = col_character(),
  "Admission.snomed_diagnosis_concept_id5" = col_character(),
  "Admission.snomed_diagnosis_concept_id6" = col_character(),
  "Admission.snomed_diagnosis_concept_id7" = col_character(),
  .default = col_guess()
)
source_data <- read_csv("data/CoreForms.csv", col_types = column_types) %>%
  distinct(patient_id, .keep_all = TRUE)
sari_data <- read_csv("data/SariAdmissionAssessment.csv") %>%
  distinct(patient_id, .keep_all = TRUE)
daily <- read_csv("data/DailyAssessment.csv")
daily <- daily %>%
  filter(DailyAssessment.date_of_daily_assessment == date_of_admission) %>%
  distinct(patient_id, .keep_all = TRUE)
sari_daily <- read_csv("data/SariDailyAssessment.csv")
sari_daily <- sari_daily %>%
  filter(SariDailyAssessment.date_of_sari_daily_assessment == date_of_admission) %>%
  distinct(patient_id, .keep_all = TRUE)

units_of_measure <- read_csv(glue("{output_path}/data/measures.csv"))

source_data <- left_join(source_data, sari_data,
                  by = join_by(patient_id, unitId, hospitalId,
                               date_of_admission, date_of_admission_hospital,
                               registry))
source_data <- left_join(source_data, daily,
                  by = join_by(patient_id, unitId, hospitalId,
                               date_of_admission, date_of_admission_hospital,
                               registry))

source_data <- left_join(source_data, sari_daily,
                  by = join_by(patient_id, unitId, hospitalId,
                               date_of_admission, date_of_admission_hospital,
                               registry))

# Getting diagnosis data and diagnosis coefficents ------------------------
coef_data <- get_apache_ii_coefficents(source_data,
  dataset_name = dataset_name,
  source = "source",
  output_path = output_path
)
source_data <- left_join(source_data, coef_data, by = "patient_id")

# Applying exclusion criteria ---------------------------------------------
# First, calculating variables I need. Some just renames to work with OMOP functions
source_data <- source_data %>%
  # For some reason, there are 4 ICU patients without ICU admision date filled.
  filter(coalesce(date_of_admission,
                  date_of_admission_hospital) >= start_date &
           coalesce(date_of_admission,
                    date_of_admission_hospital) < end_date) %>%
  mutate(
    care_site_name = unitId,
    icu_los = as.integer(difftime(Discharge.date_of_discharge, date_of_admission,
                                  units = "days")),
    hospital_outcome = if_else(Discharge.discharge_status_hos == "Dead" |
                                 Discharge.discharge_status == "Dead",
                               "Dead", "Alive", "Alive"),
    admission_year = as.factor(year(coalesce(date_of_admission,
                                             date_of_admission_hospital)))
    )

# Removing patients from ineligible sites.
source_data <- apply_ccaa_specific_exclusions(source_data, output_path)

# Removing readmissions and patients who wouldn't be in OMOP
source_data <- source_data %>%
  filter(is.na(Admission.readmission) | Admission.readmission != "Yes") %>%
  # Excluding patients who wouldn't be in OMOP.
  filter(Admission.gender %in% c("Male", "Female")) %>%
  # One patient date is missing here, even though it's in OMOP.
  filter(!is.na(Discharge.date_of_discharge)) %>%
  mutate(country = as.factor(country))


# Exclusions based on age and diagnosis
source_data <- source_data %>%
  # 5 here, not sure why
  filter(Admission.age >= 17) %>%
  filter(
    !grepl("*burn*", primary_diagnosis_name, ignore.case = TRUE)) %>%
  # Excluding patients without APACHE II diagnoses
  filter(!is.na(ap2_diag_coef))

# Calculating APACHE II --------------------------------
# Measure units file already downloaded with mapping data. Reading it in and joining
source_data <- left_join(source_data, units_of_measure, by = c("unitId" = "unit_id"))
source_data <- calculate_min_max_variables(source_data)
source_data <- unit_conversion_source(admission = source_data)
source_data <- data.table(source_data)
source_data <- fix_implausible_values_apache_ii(data = source_data)
source_data <- calculate_apache_ii_score(source_data)
source_data <- calculate_apache_ii_prob(source_data)

# Calculating SMRs --------------------------------------------------------
smrs_ni <- source_data %>%
  group_by(country) %>%
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
output <- make_output_df(source_data, "country")
output <- get_count(
  source_data, "country", "patient_id",
  "Number of patients", output
)
output <- get_unique_count(
  source_data, "country", "country",
  "Number of countries", output
)
output <- get_median_iqr(source_data, "country", "Admission.age",
                         "Age", output,
                         round = 2
)
output <- get_n_percent_value(source_data, "country", "Admission.gender", "Male",
                              "Male", output,
                              round = 2
)

output <- get_median_iqr(source_data, "country",
                         "apache_ii_score", "APACHE II score", output,
                         round = 2
)
output <- get_median_iqr(source_data, "country", "apache_ii_prob",
                         "APACHE II probability of mortality", output,
                         round = 2
)

#### SMR. Using the country dataset.
#### Have to create the row separately and paste it to the output dataset.
smr_ni_output <- make_output_df(smrs_ni, "country")
smr_ni_output <- get_median_iqr(smrs_ni, "country",
                                "smr_ap2", "APACHE II SMR Median (IQR)", smr_ni_output,
                                round = 2
)
names(smr_ni_output) <- names(output)
output <- rbind(output, smr_ni_output[1, ])

### Outcomes
output <- get_n_percent_value(source_data, "country",
                              "Discharge.discharge_status",
                              "Dead", "ICU mortality",
                              output,
                              round = 2
)
output <- get_median_iqr(source_data, "country", "icu_los",
                         "ICU length of stay (Days)", output,
                         round = 2
)
output <- get_n_percent_value(source_data, "country", "hospital_outcome",
                              "Dead", "Hospital mortality",
                              output,
                              round = 2
)
# writing the output data frame to an excel file
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "4_source_data_tableone")
writeData(wb, sheet = "4_source_data_tableone", x = output, borders = "columns")
setColWidths(wb, "4_source_data_tableone", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# Getting availability and range of the physiology components of the APACHE II score.
availability <- get_physiology_variable_availability(source_data)

# Writing the output out.
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "5_availability_apache_source")
writeData(wb, sheet = "5_availability_apache_source", x = availability,
          borders = "columns")
setColWidths(wb, "5_availability_apache_source", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# Funnel plot of SMRs NI.
smr_graph(smrs_ni, "expected_ap2", "APACHE II normal imputation")
ggsave("output/06_funnel_plot_source_data.png")

