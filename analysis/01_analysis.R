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
data <- data %>% filter(age >=18)

##### Note - I'm not sure what to do about WCC 1000/mcgl.
data <- fix_apache_ii_units(data)
data <- calculate_apache_ii_score(data)

###### Steps. Get the APACHE dataset with the correct start and end date. That's the main patient list.
### Get care site, death and discharge information for the same date range. Everyone who didn't die lived.
### Filter for the inclusion critera.
### Get table one.
### Calculate APACHE II score and prob of death.
###
