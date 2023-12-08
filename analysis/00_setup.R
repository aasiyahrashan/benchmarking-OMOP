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
library(lubridate)
source("analysis/connection_parameters.R")
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")

start_date <- "2019-01-01"
end_date <- "2022-12-31"
output_path <- "."

source("analysis/01_download_data_calculate_scores.R")
source("analysis/02_calculate_smrs.R")
