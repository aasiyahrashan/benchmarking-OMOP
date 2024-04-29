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
  # Select patients with hospital admissions, but without ICU admissions
  raw_sql <- glue("SELECT DISTINCT vo.person_id
                                   ,vo.visit_occurrence_id
                     FROM NICEOMOP.omop.visit_occurrence AS vo
          LEFT OUTER JOIN NICEOMOP.omop.visit_detail AS vd
                       ON vd.person_id = vo.person_id
                    WHERE vd.visit_detail_id IS NULL")
  nice_no_icu_admission_dataset <- dbGetQuery(conn, raw_sql)

  # Select ICU readmissions
    # 2000000012 = Number of Readmission
    # 2000000019 = Time until readmission on ICU in same hospital
  raw_sql <- glue("SELECT DISTINCT person_id
                                   ,visit_occurrence_id
                                   ,visit_detail_id
                     FROM NICEOMOP.omop.observation
                    WHERE observation_concept_id
                       IN (2000000012, 2000000019)")
  nice_readmission_dataset <- dbGetQuery(conn, raw_sql)

  # Remove patients with missing ICU admissions due to cutoff next year
  data <- data %>%
    anti_join(nice_no_icu_admission_dataset,
              by = c("person_id",
                     "visit_occurrence_id")) %>%
    anti_join(nice_readmission_dataset,
              by = c("person_id",
                     "visit_occurrence_id",
                     "visit_detail_id")) %>%
    # Add country letter a for future use later in the code
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
      !grepl("*Paediatric*", `ICU Name`, ignore.case = TRUE),
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
      !grepl("\\bEU\\b", `ICU Name`, ignore.case = TRUE),
      !grepl("\\bED\\b", `ICU Name`, ignore.case = TRUE),
      # Adding this for source analysis - excluding test units.
      !grepl("Test Unit", `ICU Name`, ignore.case = TRUE)
    ) %>%
    # Also excluding this specific unit from Ethiopia which had data collected for a very short time, and no info about untis, etc.
    filter(`Unit ID` != "5f48b1a3393af5001be5c7aa")

  ## Joining to the main dataset so only patients admitted to allowed units
  # are included.
  data <- data %>%
    mutate(`Unit ID` = gsub(" .*$", "", care_site_name)) %>%
    inner_join(all_implementation, by = "Unit ID")

  data
}
