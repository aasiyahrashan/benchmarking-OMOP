# ------------------------------------------------------------------------------
# Benchmarking OMOP - Setup
#
# By:
# Aasiyah Rashan  <aasiyah@nicslk.com>
# Daniel Puttmann <d.p.puttmann@amstedamumc.nl>
#
# Date of last edit: 2023-12-09
# ------------------------------------------------------------------------------

# Load and/or install packages --------------------------------------------
# These first two are project specific, and need to be downloaded and installed
# from their repo's. Links:
# https://github.com/aasiyahrashan/SeverityScoresOMOP
# https://github.com/aasiyahrashan/TableOneDataframe
library(SeverityScoresOMOP)
library(TableOneDataframe)

if(!("pacman" %in% installed.packages()[,"Package"])) install.packages("pacman")
pacman::p_load(conflicted, tidyverse, lubridate, glue, readxl, googlesheets4,
               openxlsx, mice, SqlRender, DBI, data.table)
conflicted::conflict_prefer(dplyr::filter,
                            dplyr::lag,
                            dplyr::between,
                            lubridate::year)

# Benchmark date range defining -------------------------------------------
start_date <- "2019-07-01"
# Realised this needs to be 2023-01-01 because the time is imputed as 00:00:00
# in sql. Otherwise, the query leaves out patients admitted on December 31st.
end_date <- "2023-01-01"
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
if (!dialect %in% listSupportedDialects()$dialect) {
  stop(glue(
    "Your dialect ({dialect}) is not recognized by SQLRender, please",
    "resubmit your dialect as one of the following:",
    "\n{toString(listSupportedDialects()$dialect)}"
  ))
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

## Sourcing function scripts and running analysis.
source("analysis/connection_parameters.R")
source("analysis/inclusion_criteria_functions.R")
source("analysis/apache_ii_prob_functions.R")
source("analysis/smr_graph_functions.R")

## Analysis on OMOP data
source("analysis/01_download_data_calculate_scores.R")
source("analysis/02_calculate_smrs.R")

## Analysis on source data (separate betwen CCAA and NICE)
if(dataset_name == "CCAA"){
  source("analysis/CCAA source data analysis/source_apache_calculation_functions.R")
  source("analysis/CCAA source data analysis/01_source_analysis.R")
}
