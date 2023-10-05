if(!require("TableOneDataframe")){
  if(!require("remotes")) install.packages("remotes")
  remotes::install_github("aasiyahrashan/TableOneDataframe")
}

library(tidyverse)
library(SeverityScoresOMOP)
library(TableOneDataframe)
library(glue)
library(readxl)
library(googlesheets4)
library(DBI)
library(openxlsx)

start_date <- "2020-01-01"
end_date <- "2023-07-31"
output_path <- "."

### Create the 'secrets.R' file. It needs to include the following parameters
### for the OMOP database:
# driver <- ""
# host <- ""
# port <- "" (if applicable)
# dbname <- ""
# schema <- ""
# user <- ""
# password <- ""
# dataset_name <- ""

source("analysis/secrets.R")
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")
source("analysis/01_analysis.R")
