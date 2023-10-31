# ------------------------------------------------------------------------------
# Benchmarking OMOP - Setup
#
# By:
# Aasiyah Rashan  <aasiyah@nicslk.com>
# Daniel Puttmann <d.p.puttmann@amstedamumc.nl>
#
# Date of last edit: 2023-09-26
# ------------------------------------------------------------------------------

# Install github packages -------------------------------------------------
if (!require("remotes")) install.packages("remotes")
remotes::install_github("aasiyahrashan/TableOneDataframe")
remotes::install_github("aasiyahrashan/SeverityScoresOMOP@nice")

# Library loading ---------------------------------------------------------
library(SeverityScoresOMOP)
library(TableOneDataframe)
library(glue)

# Define benchmark date range ---------------------------------------------
start_date <- "2020-01-01"
end_date <- "2023-07-31"
output_path <- "."

# Create & Open registry-specific files -----------------------------------
## Get all variables to do the analysis from 'secrets.R'.
## If is is not filled in, it will open the file for you.
conn_parr_path <- glue("{getwd()}/analysis/connection_parameters.R")
if(!file.exists(secrets_path)) {
  example_path <- glue("{getwd()}/analysis/example_connection_parameters.R")
  file.copy(from = example_path,
            to = conn_parr_path)
  browseURL(conn_parr_path)
}
source("analysis/connection_parameters.R")

## Creates and opens a custom *_concepts.csv file from example if nonexistent.
## Add standardized codes to it for look-up during APACHE II calculation.
## Be careful to not save with ";" but "," as separator!
concepts_path <- glue("{getwd()}/analysis/{dataset_name}_concepts.csv")
if (!file.exists(concepts_path)) {
  example_path <- glue("{getwd()}/analysis/example_concepts.csv")
  file.copy(from = example_path,
            to = conn_parr_path)
  browseURL(concepts_path)
}

# Run benchmark analysis scripts ------------------------------------------
source("analysis/apache_ii_prob_function.R")
source("analysis/smr_graph_functions.R")
source("analysis/01_analysis.R")
