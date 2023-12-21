#' NICE specific patient apache exclusion
#' Also creates country variable for SMR calculation
#' This is done based on a boolean flag
#'
#' @param conn A connection object to a database
#' @param data data in which the exclusion needs to be executed
#'
#' @import DBI
#' @import dplyr
#' @import glue
#' @export
apply_nice_specific_exclusions <- function(conn, data) {
  # Selects patients with nice_apache_excluded flag (concept_id = 2000000014)
  raw_sql <- glue("SELECT DISTINCT person_id
                                   ,visit_occurrence_id
                                   ,visit_detail_id
                     FROM NICEOMOP.omop.observation
                    WHERE observation_datetime >= '2019-01-01'
                      AND observation_datetime <= '2023-01-01'
                      AND observation_concept_id = 2000000014")

  nice_apache_exclusion_dataset <- dbGetQuery(conn, raw_sql)

  # Selects patients with hospital admissions, but
  # without ICU admissions (cutoff 2023)
  raw_sql <- glue("SELECT vo.person_id,
                          vo.visit_occurrence_id
                     FROM NICEOMOP.omop.visit_occurrence AS vo
          LEFT OUTER JOIN NICEOMOP.omop.visit_detail AS vd
                       ON vd.person_id = vo.person_id
                    WHERE vd.visit_detail_id IS NULL")
  nice_missing_dataset <- dbGetQuery(conn, raw_sql)

  # Remove excluded icu admissions and missing icu admissions
  data <- data %>%
    anti_join(nice_apache_exclusion_dataset,
              by = c("person_id",
                     "visit_occurrence_id",
                     "visit_detail_id")) %>%
    anti_join(nice_missing_dataset,
              by = c("person_id",
                     "visit_occurrence_id")) %>%
    mutate(country = letters[1])

  data
}

#' CCAA specific patient exclusion
#' Also creates country variable for SMR calculation
#' @param data data in which the exclusion needs to be executed
#' @param output_path Place mapping files are stored
#'
#' @import dplyr
#' @export
apply_ccaa_specific_exclusions <- function(data, output_path) {
  all_implementation <- read_csv(glue("{output_path}/data/all_implementation.csv"))
  all_implementation <- all_implementation %>%
    ### Only including valid registries
    filter(!is.na(`Unit ID`)) %>%
    filter(Registry %in% c(
      "PRICE", "IRIS", "NICR", "Afghanistan",
      "Malaysia", "Bangladesh", "Kenya", "Uganda", "Ghana",
      "Sierra Leone", "South Africa", "Ethiopia"
    )) %>%
    ### Assigning each registry to a letter.
    mutate(
      country =
        LETTERS[match(Registry, unique(all_implementation$Registry)) + 1]
    ) %>%
    ### Excluding test units, wards, pediatric and neonatal ICUs, HDUs,
    # maternity units, emergency unit.
    filter(!`ICU Type` %in% c("TEST", "WARD", "PEDIATRIC", "NEONATAL")) %>%
    filter(
      !grepl("*PICU*", `ICU Name`, ignore.case = TRUE),
      !grepl("*Pediatric*", `ICU Name`, ignore.case = TRUE),
      !grepl("*Ward*", `ICU Name`, ignore.case = TRUE),
      !grepl("*HDU*", `ICU Name`, ignore.case = TRUE),
      !grepl("*High Risk Unit*", `ICU Name`, ignore.case = TRUE),
      !grepl("*High Dependency*", `ICU Name`, ignore.case = TRUE),
      !grepl("*High care unit*", `ICU Name`, ignore.case = TRUE),
      !grepl("*GyneObs ICU*", `ICU Name`, ignore.case = TRUE),
      !grepl("*Gyn/Obs*", `ICU Name`, ignore.case = TRUE),
      !grepl("*OBS*", `ICU Name`, ignore.case = TRUE),
      !grepl("*Gyne Post Operative*", `ICU Name`, ignore.case = TRUE),
      !grepl("*G/OICU*", `ICU Name`, ignore.case = TRUE),
      !grepl("*Obstetric ICU*", `ICU Name`, ignore.case = TRUE),
      !grepl("*Maternal HDU*", `ICU Name`, ignore.case = TRUE),
      !grepl("EU", `ICU Name`, ignore.case = TRUE),
      !grepl("ED", `ICU Name`, ignore.case = TRUE),
      # Adding this for source analysis - excluding test units.
      !grepl("Test Unit", `ICU Name`, ignore.case = TRUE)
    )

  ## Joining to the main dataset so only patients admitted to allowed units
  # are included.
  data <- data %>%
    mutate(`Unit ID` = gsub(" .*$", "", care_site_name)) %>%
    inner_join(all_implementation, by = "Unit ID")

  data
}
