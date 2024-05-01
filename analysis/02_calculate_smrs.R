load("data/03_original_physiology_availability.RData")
load("data/04_cleaned_filtered_data.RData")
load("data/05_mice_data.RData")
mice_long <- complete(mice_data, "long", include = TRUE) %>%
  mutate(country = as.factor(country))

# SMRS normal imputation --------------------------------------------------
smrs_ni <- data %>%
  group_by(country) %>%
  summarise(
    total = sum(!is.na(person_id)),
    # SMRs
    median_ap2_score = median(apache_ii_score, na.rm = TRUE),
    median_ap2_prob = median(apache_ii_prob * 100, na.rm = TRUE),
    expected_ap2 = median(apache_ii_prob, na.rm = TRUE) * total,
    n_dead = sum(icu_outcome == "Dead", na.rm = TRUE),
    percent_dead = 100 * sum(icu_outcome == "Dead", na.rm = TRUE) / total,
    smr_ap2 = n_dead / expected_ap2
  ) %>%
  ungroup()

# SMRS multiple imputation --------------------------------------------------
# To report median and IQR of AP2 score,
# getting score and mortality probability per patient.
# Summarising using mean to get a point estimate. We won't report variability.
mice_summary <- mice_long %>%
  #### Don't want to include the original data set
  filter(.imp != 0) %>%
  group_by(
    person_id, visit_occurrence_id, visit_detail_id, country
  ) %>%
  summarise(
    apache_ii_score_no_imputation = mean(apache_ii_score_no_imputation),
    apache_ii_prob_no_imputation = mean(apache_ii_prob_no_imputation,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# To report SMRs for the funnel plot,
# First calculating expected deaths per ICU and imputed dataset.
# Then using Rubin's rules to calculate point estimate and 95%CI for expected deaths.
# Used this tutorial. https://www.bookdown.org/rwnahhas/RMPH/mi-descriptives.html
# Then calculating SMR for mean, and 95% CI limits.
# Then plotting funnel plot for mean SMR and expected deaths.
# Then as a sensitivity analysis, plotting upper and lower 95% CIs of the SMRs
smrs_mi <- mice_long %>%
  filter(.imp != 0) %>%
  group_by(.imp, country) %>%
  summarise(
    total = sum(!is.na(person_id)),
    # Getting mean and variance of expected deaths to use for Rubin's rule pooling.
    expected_ap2 = mean(apache_ii_prob_no_imputation) * total,
    var_mean_expected = var(apache_ii_prob_no_imputation) / total,
    n_dead = sum(icu_outcome == "Dead"),
  ) %>%
  group_by(country, n_dead) %>%
  summarise(
    pooled_mean = mean(expected_ap2),
    within_imp_var = mean(var_mean_expected),
    between_imp_var = var(expected_ap2),
    pooled_variance = within_imp_var + (1 + 1 / max(.imp)) * between_imp_var,
    pooled_se = sqrt(pooled_variance),
    lower_ci_expected_deaths = pooled_mean - 1.96 * pooled_se,
    upper_ci_expected_deaths = pooled_mean + 1.96 * pooled_se
  ) %>%
  # Getting SMRs based on pooled expected deaths.
  mutate(
    smr_ap2 = n_dead / pooled_mean,
    lower_ci_smr_ap2 = n_dead / lower_ci_expected_deaths,
    upper_ci_smr_ap2 = n_dead / upper_ci_expected_deaths
  ) %>%
  ungroup()

# Create tables and graphs ------------------------------------------------

######### Creating table one.
output <- make_output_df(data, "country")
output <- get_count(
  data, "country", "person_id",
  "Number of patients", output
)
output <- get_unique_count(
  data, "country", "country",
  "Number of countries", output
)
# NICE does not include care site in OMOP, so number of ICUs in NICE has to come from NICE metadata.
if (dataset_name == "CCAA") {
  output <- get_unique_count(
    data, "country", "care_site_id",
    "Number of ICUs", output
  )
}
output <- get_median_iqr(data, "country", "age",
  "Age", output,
  round = 2
)
output <- get_n_percent_value(data, "country", "gender", "MALE",
  "Male", output,
  round = 2
)

############## Normal imputation
output <- get_median_iqr(data, "country",
  "apache_ii_score", "APACHE II score", output,
  round = 2
)
output <- get_median_iqr(data, "country", "apache_ii_prob",
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

### Scores multiple imputation
output <- get_median_iqr(mice_summary, "country",
  "apache_ii_score_no_imputation", "APACHE II score MI", output,
  round = 2
)
output <- get_median_iqr(mice_summary, "country",
  "apache_ii_prob_no_imputation",
  "APACHE II probability of mortality MI", output,
  round = 2
)

#### SMR for APACHE II.
smr_mi_output <- make_output_df(smrs_mi, "country")
smr_mi_output <- get_median_iqr(smrs_mi, "country",
  "smr_ap2", "APACHE II SMR Median (IQR)", smr_mi_output,
  round = 2
)
names(smr_mi_output) <- names(output)
output <- rbind(output, smr_mi_output[1, ])

### Outcomes
output <- get_n_percent_value(data, "country", "icu_outcome",
  "Dead", "ICU mortality",
  output,
  round = 2
)
output <- get_median_iqr(data, "country", "icu_los",
  "ICU length of stay (Days)", output,
  round = 2
)
output <- get_n_percent_value(data, "country", "hospital_outcome",
  "Dead", "Hospital mortality",
  output,
  round = 2
)
# writing the output data frame to an excel file
write.xlsx(output,
  file = "output/01_output.xlsx", borders = c("all"), colWidths = c("auto"),
  na.string = "-"
)


# Availability ------------------------------------------------------------
# Writing out original availability
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "2_availability_orig")
writeData(wb, sheet = "2_availability_orig", x = availability_orig, borders = "columns")
setColWidths(wb, "2_availability_orig", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# Getting availability and range of the physiology components of the APACHE II score.
availability <- get_physiology_variable_availability(data)

# Writing the output out.
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "3_availability_after_delete")
writeData(wb, sheet = "3_availability_after_delete", x = availability, borders = "columns")
setColWidths(wb, "3_availability_after_delete", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# Variable distribution. - max and min are the same for CCAA, since most extreme value is collected.
output_2 <- make_output_df(data, "country")

output_2 <- get_median_iqr(data, "country", "age",
                         "Age", output_2,
                         round = 1
)
output_2 <- get_n_percent_value(data, "country", "gender", "MALE",
                              "Male", output_2,
                              round = 1
)
output_2 <- get_n_percent_value(data, "country", "comorbidity", 1,
                              "Organ failure/immunocompromised", output_2,
                              round = 1
)
output_2 <- get_n_percent_value(data, "country", "renal_failure", 1,
                              "Renal failure", output_2,
                              round = 1
)
output_2 <- get_n_percent_value(data, "country", "emergency_admission", 1,
                              "Emergency admission", output_2,
                              round = 1
)
output_2 <- get_median_iqr(data, "country", "max_bicarbonate",
                         "Bicarbonate", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_creatinine",
                         "Creatinine", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_fio2",
                         "FiO2", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "min_gcs",
                         "GCS", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_hematocrit",
                         "Hematocrit", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_hr",
                         "Heart rate", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_map",
                         "Mean arterial pressure", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_pao2",
                         "PaO2", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_ph",
                         "pH", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_potassium",
                         "Potassium", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_rr",
                         "Respiratory rate", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_sodium",
                         "Sodium", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_temp",
                         "Temperature", output_2,
                         round = 1
)
output_2 <- get_median_iqr(data, "country", "max_wcc",
                         "White cell count", output_2,
                         round = 1
)
# Writing the output out.
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "4_variable_distributions")
writeData(wb, sheet = "4_variable_distributions", x = output_2, borders = "columns")
setColWidths(wb, "4_variable_distributions", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# The NICE graph has size limits for publication
if(dataset_name == "NICE"){
  smr_graph(smrs_ni, "expected_ap2", "",
            max = 35000, automatic_y_lim = TRUE)
} else if (dataset_name == "CCAA"){
  smr_graph(smrs_ni, "expected_ap2", "APACHE II normal imputation")
}

ggsave("output/02_funnel_plot_ni.png", width = 7, height =7,
       bg='transparent')

# Funnel plot of SMRs MI.
# Here, the CCAA graph has size limits for publication
if(dataset_name == "NICE"){
  smr_graph(smrs_mi, "pooled_mean", "APACHE II multiple imputation")
} else if (dataset_name == "CCAA"){
  smr_graph(smrs_mi, "pooled_mean", "",
            max = 35000, automatic_y_lim = TRUE)
}
ggsave("output/03_funnel_plot_mi.png", width = 7, height =7,
       bg='transparent')

# Sensitivity analysis funnel plot for upper and lower CIs.
smr_graph(smrs_mi, "lower_ci_expected_deaths", "APACHE II multiple imputation lower 95% CI")
ggsave("output/04_funnel_plot_mi_lower_ci.png", width = 7, height =7,
       bg='transparent')
smr_graph(smrs_mi, "upper_ci_expected_deaths", "APACHE II multiple imputation upper 95% CI")
ggsave("output/05_funnel_plot_mi_upper_ci.png", width = 7, height =7,
       bg='transparent')
