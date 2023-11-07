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
remotes::install_github("aasiyahrashan/TableOneDataframe", quiet = TRUE)
# remotes::install_github("aasiyahrashan/SeverityScoresOMOP@nice", quiet = TRUE)


# Library loading ---------------------------------------------------------
library(TableOneDataframe)
# library(SeverityScoresOMOP)
library(tidy3verse)
library(glue)

devtools::load_all(glue("../SeverityScoresOMOP"))

# Benchmark date range defining -------------------------------------------
start_date <- "2019-01-01"
end_date <- "2022-12-31"
output_path <- "."

# Registry-specific files creating & opening ------------------------------
## Creates and opens a custom connection parameters file if nonexistent.
conn_parr_path <- glue("{getwd()}/analysis/connection_parameters.R")
if(!file.exists(conn_parr_path)) {
  example_path <- glue("{getwd()}/analysis/example_connection_parameters.R")
  file.copy(from = example_path, to = conn_parr_path)
  browseURL(conn_parr_path)
}
source("analysis/connection_parameters.R")

## Creates and opens a custom *_concepts.csv file from example if nonexistent.
## Add standardized codes to it for look-up during APACHE II calculation.
## Be careful to not save with ";" but "," as separator!
concepts_path <- glue("{getwd()}/analysis/{dataset_name}_concepts.csv")
if (!file.exists(concepts_path)) {
  example_path <- glue("{getwd()}/analysis/example_concepts.csv")
  file.copy(from = example_path, to = concepts_path)
  browseURL(concepts_path)
}

# Run benchmark analysis scripts ------------------------------------------
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")
source("analysis/01_analysis.R")
