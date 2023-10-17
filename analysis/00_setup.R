library(tidyverse)
### Download from here. https://github.com/aasiyahrashan/SeverityScoresOMOP
library(SeverityScoresOMOP)
## Download this from here. https://github.com/aasiyahrashan/TableOneDataframe
library(TableOneDataframe)
library(glue)
library(readxl)
library(googlesheets4)
library(DBI)
library(openxlsx)
library(mice)

start_date <- "2019-01-01"
end_date <- "2022-12-31"
output_path <- "."

### The 'secrets.R' file needs to include the following parameters for the OMOP database
# host <- ""
# dbname <- ""
# schema <- ""
# user <- ""
# password <- ""
source("secrets.R")
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")
#source("analysis/01_analyis.R")
