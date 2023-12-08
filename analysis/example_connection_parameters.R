# ------------------------------------------------------------
# Connection parameters
# These parameters are needed to connect to the OMOP database
# ------------------------------------------------------------
# The name of your dataset or quality registry (eg. CCAA, NICE, etc.)
dataset_name <- ""

# The driver of your database (eg. PostgreSQL, SQL Server, etc.)
# If you don't use PostgreSQL, and are unsure, run the code below:
# odbc::odbcListDrivers()
driver <- ""

# The dialect used by your database. We use it for SQLRender, which supports the
# following dialects. Choose one of the following:
# sql server, oracle, postgresql, pdw, impala, netezza, bigquery, spark, sqlite,
# redshift, hive, sqlite extended, duckdb, snowflake, or synapse.
dialect <- ""

# The hostname of the server your database is on
host <- ""

# (OPTIONAL) The port your server is available on
port <- ""

# The name of your database (POSSIBLY DIFFERENT FROM YOUR DATA SET'S NAME!)
dbname <- ""

# The schema used to store your OMOP CDM tables in
schema <- ""

# The username to gain access to your database
user <- ""

# The password to gain access to your database
password <- ""
