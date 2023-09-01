library(tidyverse)
library(SeverityScoresOMOP)
library(TableOneDataframe)
library(glue)
library(readxl)
library(googlesheets4)
library(DBI)

start_date <- "2020-01-01"
end_date <- "2023-07-31"
output_path <- "."
source("secrets.R")

#source("analysis/01_analyis.R")
