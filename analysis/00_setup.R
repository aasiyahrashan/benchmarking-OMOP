# ------------------------------------------------------------------------------
# Benchmarking OMOP - Setup
#
# By:
# Aasiyah Rashan  <aasiyah@nicslk.com>
# Daniel Puttmann <d.p.puttmann@amstedamumc.nl>
#
# Date of last edit: 2023-09-26
# ------------------------------------------------------------------------------

# GitHub packages installing ----------------------------------------------
if (!require("remotes")) install.packages("remotes")
remotes::install_github("r-lib/conflicted")
remotes::install_github("aasiyahrashan/TableOneDataframe", quiet = TRUE)
remotes::install_github("aasiyahrashan/SeverityScoresOMOP", quiet = TRUE)
if(!require("openxlsx")) install.packages("openxlsx")

# Library loading ---------------------------------------------------------
library(TableOneDataframe)
library(SeverityScoresOMOP)
library(glue)
library(SqlRender)
library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag",    "dplyr")

# Benchmark date range defining -------------------------------------------
start_date <- "'2019-01-01'"
end_date <- "'2022-12-31'"
output_path <- "."

# Creating output folders -------------------------------------------------
dir.create(glue("{getwd()}/output"))
dir.create(glue("{getwd()}/data"))

# Registry-customized files creating & opening ----------------------------
## Creates & opens a custom connection parameters file if nonexistent.
conn_parr_path <- glue("{getwd()}/analysis/connection_parameters.R")
if(!file.exists(conn_parr_path)) {
  example_path <- glue("{getwd()}/analysis/example_connection_parameters.R")
  file.copy(from = example_path, to = conn_parr_path)
  browseURL(conn_parr_path)
  stop(glue("ERROR - fill in connection_parameters.R before rerunning"))
}
source("analysis/connection_parameters.R")

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

# Run benchmark analysis scripts ------------------------------------------
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")
source("analysis/01_analysis.R")
# source("analysis/01_download_data_calculate_scores.R")
# source("analysis/02_calculate_smrs.R")
