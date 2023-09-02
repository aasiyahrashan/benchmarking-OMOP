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


####### 373,166.
###### Excluding everyone <18. 218, 821
data <- data %>%
  filter(age >=18) %>%
  # Calculating a few variables I need.
  mutate(icu_outcome = if_else(!is.na(death_datetime) &
                                 death_datetime > icu_admission_datetime &
                                 death_datetime < icu_discharge_datetime, "Dead", "Alive"),
         icu_los = as.numeric(difftime(icu_discharge_datetime, icu_admission_datetime,
                                       units = "days")))

##### Calculating apache II score.
## Note - I'm not sure what to do about WCC 1000/mcgl.
data <- fix_apache_ii_units(data)
data <- calculate_apache_ii_score(data)

### Calculating apache II prob. Functions in apache_ii_prob file.
download_mapping_files(snomed_mapping_path, ap2_path, ap2_coefs_path, output_path)
coef_data <- calculate_apache_ii(get_apache_ii_coefficents, output_path)
data <- calculate_apache_ii_prob(data, coef_data)

######### Creating table one. Dividing by gender because I have to divide by something. Not planning to use it.
output <- make_output_df(data, "gender", include_tests = TRUE)
output <- get_count(data, "gender", "person_id", "Number of patients", output)
output <- get_unique_count(data, "gender", "care_site_id", "Number of sites", output)
output <- get_median_iqr(data, "gender", 'age', "Age", output, round =1)
output <- get_n_percent_value(data, 'gender', 'gender', "MALE", "Male", output, round =1)

### Scores
output <- get_median_iqr(data, "gender", 'apache_ii_score', "APACHE II score", output, round =1)
output <- get_median_iqr(data, "gender", 'apache_ii_prob',
                         "APACHE II probability of mortality", output, round =1)

### Outcomes
output <- get_n_percent_value(data, 'gender', 'icu_outcome', "Dead", "ICU mortality",
                              output, round =1)
output <- get_median_iqr(data, "gender", 'icu_los', "ICU length of stay (Days)", output, round =1)


# writing the output data frame to an excel file
write.xlsx(output, file = "output/01_output.xlsx", borders = c("all"), colWidths = c("auto"),
           na.string = "-")
