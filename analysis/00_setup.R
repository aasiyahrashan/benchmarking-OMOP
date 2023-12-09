# ------------------------------------------------------------------------------
# Benchmarking OMOP - Setup
#
# By:
# Aasiyah Rashan  <aasiyah@nicslk.com>
# Daniel Puttmann <d.p.puttmann@amstedamumc.nl>
#
# Date of last edit: 2023-09-26
# ------------------------------------------------------------------------------
library(conflicted)
library(tidyverse)
### Download from here. https://github.com/aasiyahrashan/SeverityScoresOMOP
library(SeverityScoresOMOP)
## Download this from here. https://github.com/aasiyahrashan/TableOneDataframe
library(TableOneDataframe)
library(glue)
library(readxl)
library(googlesheets4)
library(openxlsx)
library(mice)
library(SqlRender)
## Required for the dbGetQuery function
library(DBI)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag",    "dplyr")

# Benchmark date range defining -------------------------------------------
start_date <- "2019-01-01"
end_date <- "2022-12-31"
output_path <- "."

# Creating output folders -------------------------------------------------
dir.create(glue("{getwd()}/output"))
dir.create(glue("{getwd()}/data"))

# Checking Pre-requisites -------------------------------------------------
## Registry-customized files creating & opening
## Creates & opens a custom connection parameters file if nonexistent.
conn_parr_path <- glue("{getwd()}/analysis/connection_parameters.R")
if (!file.exists(conn_parr_path)) {
  example_path <- glue("{getwd()}/analysis/example_connection_parameters.R")
  file.copy(from = example_path, to = conn_parr_path)
  browseURL(conn_parr_path)
  stop("Error: fill in connection_parameters.R before rerunning")
}

source("analysis/connection_parameters.R")
if(!dialect %in% listSupportedDialects()$dialect){
  stop(glue("Your dialect ({dialect}) is not recognized by SQLRender, please",
            "resubmit your dialect as one of the following:",
            "\n{toString(listSupportedDialects()$dialect)}"))
}

## Creates & opens a custom *_concepts.csv file from example if nonexistent.
## Add standardized codes to it for look-up during APACHE II calculation.
## Be careful to not save with ";" but "," as separator!
concepts_path <- glue("{getwd()}/analysis/{dataset_name}_concepts.csv")
if (!file.exists(concepts_path)) {
  example_path <- glue("{getwd()}/analysis/example_concepts.csv")
  file.copy(from = example_path, to = concepts_path)
  browseURL(concepts_path)
  stop(glue("ERROR - fill in {dataset_name}_concepts.csv before rerunning"))
}

##### Sourcing function scripts and running analysis.
source("analysis/connection_parameters.R")
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")

source("analysis/01_download_data_calculate_scores.R")
source("analysis/02_calculate_smrs.R")
