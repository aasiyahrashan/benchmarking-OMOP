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
source("secrets.R")
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")
#source("analysis/01_analyis.R")
