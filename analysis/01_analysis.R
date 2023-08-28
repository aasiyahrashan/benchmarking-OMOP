####### First getting database containing apache variables. Will save it and use for further analysis.
# postgres_conn <- postgres_connect(host = host,
#                                   dbname = dbname,
#                                   port = 5432,
#                                   user = user,
#                                   password = password)
#
# ### Getting dataset of physiology variables
# data <- get_score_variables(postgres_conn, schema_name, start_date, end_date,
#                             0, 2, "CCAA", "APACHE II")
# dbDisconnect(postgres_conn)
save(data, file = "data/00_orig_data.RData")

####### 373,166.
###### Excluding everyone <18. 354,766.
data <- data %>% filter(age >=18)

##### Note - I'm not sure what to do about WCC 1000/mcgl.
data_units_edited <- fix_apache_ii_units(data)
data <- calculate_apache_ii_score(data)

###### Steps. Get the APACHE dataset with the correct start and end date. That's the main patient list.
### Get care site, death and discharge information for the same date range. Everyone who didn't die lived.
### Filter for the inclusion critera.
### Get table one.
### Calculate APACHE II score and prob of death.
###
