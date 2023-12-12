load("data/03_cleaned_filtered_data.RData")
load("data/04_mice_data.RData")

# SMRS normal imputation --------------------------------------------------
smrs_ni <- data %>%
  group_by(country, admission_year) %>%
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
##### Getting score and mortality probablity per patient. Just getting mean.
##### Not completely sure if correct.
mice_summary <- mice_long %>%
  #### Don't want to include the original dataset
  filter(.imp != 0) %>%
  group_by(
    person_id, visit_occurrence_id, visit_detail_id, country,
    admission_year
  ) %>%
  summarise(
    apache_ii_score_no_imputation = mean(apache_ii_score_no_imputation),
    apache_ii_prob_no_imputation = mean(apache_ii_prob_no_imputation,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

######### SMRs multiple imputation
smrs_mi <- mice_long %>%
  filter(.imp != 0) %>%
  group_by(.imp, country, admission_year) %>%
  summarise(
    total = sum(!is.na(person_id)),
    # SMRs
    expected_ap2 = median(apache_ii_prob_no_imputation, na.rm = TRUE) * total,
    n_dead = sum(icu_outcome == "Dead", na.rm = TRUE),
    percent_dead = 100 * sum(icu_outcome == "Dead", na.rm = TRUE) / total,
    smr_ap2 = n_dead / expected_ap2
  ) %>%
  ####### Now applying Rubin's rules to get CIs.
  ####### Not done yet. Just getting the mean for the moment.
  #### Getting the mean over all imputed datasets as the point estimate.
  group_by(country, admission_year) %>%
  summarise(
    total = mean(total),
    expected_ap2 = mean(expected_ap2),
    n_dead = mean(n_dead),
    smr_ap2 = mean(smr_ap2)
  ) %>%
  ungroup()


### Deciding if countries contributed data each month or not month, or not by
### comparing number of admissions over time.
### This decides whether or not a country should contribute to the annual SMRs
### on the funnel plots, and in the graphs.
patients_per_month_country_non_summarised <-
  data %>%
  mutate(
    country_fac = factor(country),
    admission_year = factor(lubridate::year(icu_admission_datetime)),
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
  #### Only allowing months with admissions which haven't
  #### decreased more than 80 % compared to previous or next month to
  #### count as contributions.
  mutate(
    contributed =
      case_when(
        n_admissions < 5 ~ FALSE,
        percent_change_last_month < -80 ~ FALSE,
        percent_change_next_month < -80 ~ FALSE,
        TRUE ~ TRUE
      ),
    date = as.Date(paste0(
      as.character(admission_year), "-",
      as.character(admission_month), "-", "01"
    ))
  )

# #### Plotting and saving graph of percentage increase and decrease in c
# ontributions per site.
p <- patients_per_month_country_non_summarised %>%
  filter(!is.na(percent_change_last_month) &
    percent_change_last_month != -Inf &
    percent_change_last_month != Inf) %>%
  filter(percent_change_last_month < 0)

patients_per_month_country <-
  patients_per_month_country_non_summarised %>%
  group_by(country, admission_year) %>%
  summarise(months_contributed_in_year = sum(contributed))

#### Filtering out SMRs with fewer than 6 months of contribution.
### This is based on the ICNARC report. https://www.google.com/url?q=https://onlinereports.icnarc.org/Reports/2019/12/annual-quality-report-201819-for-adult-critical-care&sa=D&source=docs&ust=1698589035706375&usg=AOvVaw3Zu-zA_qy5M02R9HGsMLZP
smrs_ni <- left_join(smrs_ni,
  patients_per_month_country,
  by = c("country", "admission_year")
) %>%
  filter(months_contributed_in_year >= 6 | (months_contributed_in_year >= 3 &
    admission_year == 2019))

smrs_mi <- left_join(smrs_mi,
  patients_per_month_country,
  by = c("country", "admission_year")
) %>%
  filter(months_contributed_in_year >= 6 |
    (months_contributed_in_year >= 3 &
      admission_year == 2019))


# Create tables and graphs ------------------------------------------------

######### Creating table one.
output <- make_output_df(data, "admission_year")
output <- get_count(
  data, "admission_year", "person_id",
  "Number of patients", output
)
output <- get_unique_count(
  data, "admission_year", "country",
  "Number of countries", output
)
output <- get_median_iqr(data, "admission_year", "age",
  "Age", output,
  round = 2
)
output <- get_n_percent_value(data, "admission_year", "gender", "MALE",
  "Male", output,
  round = 2
)

############## Normal imputation
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

### Scores multiple imputation
output <- get_median_iqr(mice_summary, "admission_year",
  "apache_ii_score_no_imputation", "APACHE II score MI", output,
  round = 2
)
output <- get_median_iqr(mice_summary, "admission_year",
  "apache_ii_prob_no_imputation",
  "APACHE II probability of mortality MI", output,
  round = 2
)

#### SMR for APACHE II.
smr_mi_output <- make_output_df(smrs_mi, "admission_year")
smr_mi_output <- get_median_iqr(smrs_mi, "admission_year",
  "smr_ap2", "APACHE II SMR Median (IQR)", smr_mi_output,
  round = 2
)
names(smr_mi_output) <- names(output)
output <- rbind(output, smr_mi_output[1, ])

### Outcomes
output <- get_n_percent_value(data, "admission_year", "icu_outcome",
  "Dead", "ICU mortality",
  output,
  round = 2
)
output <- get_median_iqr(data, "admission_year", "icu_los",
  "ICU length of stay (Days)", output,
  round = 2
)

# writing the output data frame to an excel file
write.xlsx(output,
  file = "output/01_output.xlsx", borders = c("all"), colWidths = c("auto"),
  na.string = "-"
)

# Getting availability and range of the physiology components of the APACHE II score.
availability <- get_physiology_variable_availability(data)

# Writing the output out.
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "2_availability_apache")
writeData(wb, sheet = "2_availability_apache", x = availability, borders = "columns")
setColWidths(wb, "2_availability_apache", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

# Funnel plot of SMRs NI.
smr_graph(smrs_ni, "APACHE II normal imputation")
ggsave("output/02_funnel_plot_ni.png")

# Funnel plot of SMRs MI.
smr_graph(smrs_mi, "APACHE II multiple imputation")
ggsave("output/03_funnel_plot_mi.png")
