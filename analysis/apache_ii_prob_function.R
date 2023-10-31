#' Remove special characters and spaces from a
#' string variable to make it easier to join.
#' @param string_var string to be cleaned
#' @import stringr
#' @noRd
clean_string_to_join <- function(string_var) {
  string_var <- as.character(string_var)
  string_var <- tolower(string_var)
  string_var <- str_trim(string_var)
  string_var <- str_squish(string_var)
  string_var <- str_replace_all(string_var, "(disorder)", "")
  string_var <- str_replace_all(string_var, "(procedure)", "")
  string_var <- str_replace_all(string_var, "[^[:alnum:]]", "")
}

apply_ccaa_specific_exclusions <- function(data, output_path) {
  all_implementation <-
    read_csv(glue("{output_path}/data/all_implementation.csv"))
  all_implementation <-
    ### Only including valid registries
    filter(!is.na(`Unit ID`)) %>%
    filter(
      Registry %in% c(
        "PRICE",
        "IRIS",
        "NICR",
        "Afghanistan",
        "Malaysia",
        "Bangladesh",
        "Kenya",
        "Uganda",
        "Ghana",
        "Sierra Leone",
        "South Africa",
        "Ethiopia"
      )
    ) %>%
    ### Excluding wards, maternity units, emergency units, test units,
    # pediatric ICUs, neonatal ICUs, and HDUs.
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
      !grepl("ED", `ICU Name`, ignore.case = TRUE)
    )

  #### Joining to the main dataset so only patients admitted to allowed units are included.
  data <- data %>%
    mutate(`Unit ID` = gsub(" .*$", "", care_site_name)) %>%
    inner_join(all_implementation, by = "Unit ID")

  data
}

download_mapping_files <-
  function(snomed_mapping_path,
           ap2_path,
           ap2_coefs_path,
           implementation_asia_path,
           implementation_africa_path,
           output_path) {
    # Only downloading the data if it doesn't already exist.
    files_dont_exist <-
      !file.exists(
        glue("{output_path}/data/snomed_ap4.csv"),
        glue("{output_path}/data/ap2.csv"),
        glue("{output_path}/data/ap2_coefs.csv"),
        glue("{output_path}/data/all_implementation.csv")
      )

    if (any(files_dont_exist)) {
      googlesheets4::gs4_auth(email = "*@nicslk.com")

      # Downloading data and saving it to csv.
      print("Downloading mapping data")

      read_excel(snomed_mapping_path, skip = 1, sheet = "Mapped") %>%
        mutate(`Variable ID` = as.character(`Variable ID`)) %>%
        write_csv(file = glue("{output_path}/data/snomed_ap4.csv"))

      ap2 <- read_sheet(ap2_path, sheet = "Sh1-AP4toAP2") %>%
        write_csv(file = glue("{output_path}/data/ap2.csv"))
      ap2_coefs <- read_sheet(ap2_coefs_path) %>%
        write_csv(file = glue("{output_path}/data/ap2_coefs.csv"))

      #### Getting implementation sheet data.
      asia <- implementation_asia_path %>%
        sheet_names() %>%
        #### This removes the values from the vector above.
        #### Removing invalid registries and metadata sheets.
        setdiff(
          c(
            "CCA",
            "NICS",
            "Demo",
            "sheet",
            "weekly validation",
            "Live Sites Contacts",
            "Troubleshooting",
            "Sheet9",
            "check list ",
            "expenses",
            "Philippines",
            "Sri Lanka",
            "Maldives",
            "Indonesia"
          )
        ) %>%
        set_names() %>%
        map_df(
          ~ read_sheet(
            ss = implementation_asia_path,
            sheet = .,
            skip = 7
          ) %>%
            select(`Hospital name`, `ICU Name`, `Unit ID`, `ICU Type`),
          .id = "Registry"
        )

      africa <- implementation_africa_path %>%
        sheet_names() %>%
        #### This removes the values from the vector above.
        #### Removing invalid registries and metadata sheets
        setdiff(
          c(
            "Ethical approvals",
            "All GECO",
            "NICS",
            "weekly validation",
            "Live Sites Contacts",
            "Troubleshooting",
            "Sheet9",
            "check list ",
            "expenses",
            "Tasks for Madiha",
            "Nigeria",
            "Namibia",
            "Cameroon"
          )
        ) %>%
        set_names() %>%
        map_df(
          ~ read_sheet(
            ss = implementation_africa_path,
            sheet = .,
            skip = 7
          ) %>%
            select(`Hospital name`, `ICU Name`, `Unit ID`, `ICU Type`),
          .id = "Registry"
        )

      rbind(asia, africa) %>%
        write_csv(file = glue("{output_path}/data/all_implementation.csv"))
    }
  }


#' Gets diagnosis coefficents for APACHE II probabilty of death calculation.
#' @param data Dataset after get_diagnoses.sql query. The download_mapping_data
#'             function should also have been run.
#' @param output_path file path data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
get_apache_ii_coefficents <- function(data, output_path) {
  # Reading in the various mapping sheets.
  # Forcing only one version of the main IDs because joins break otherwise.
  # Have also tried to correct on mapping sheets.
  snomed_mapping <-
    read_csv(glue("{output_path}/data/snomed_ap4.csv")) %>%
    mutate(
      `Variable ID` = as.character(`Variable ID`),
      `Fully Specified Names (FSNs)` = clean_string_to_join(`Fully Specified Names (FSNs)`)
    ) %>%
    distinct(`Variable ID`, .keep_all = TRUE) %>%
    distinct(`Fully Specified Names (FSNs)`, .keep_all = TRUE)

  ap2 <- read_csv(glue("{output_path}/data/ap2.csv")) %>%
    mutate(`APACHE IV diagnosis` = clean_string_to_join(`APACHE IV diagnosis`)) %>%
    distinct(`APACHE IV diagnosis`, .keep_all = TRUE)

  ap2_coefs <-
    read_csv(glue("{output_path}/data/ap2_coefs.csv")) %>%
    mutate(name = clean_string_to_join(name))

  #### Cleaning up the diagnosis name
  #### The source name has both variable and value. Getting value only.
  #### It's the section before the first comma.
  data <- data %>%
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only))

  # Getting primary diagnoses.
  # Order as follows:
  # 1. apache_diagnosis: primary diagnosis at AP4 collection.
  # 2. operation 1: first option for operative patients
  # 3. disorder 1: first option for non operative patients.
  disorder_1 <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^disorder1,*", diagnosis_name)) %>%
    mutate(concept_code = as.character(concept_code)) %>%
    ##### Joining all the diagnoses by concept ID and name
    # Disorder. Joining on both name and concept ID because there are mismatches.
    left_join(
      snomed_mapping,
      by = c("concept_code" = "Variable ID"),
      na_matches = "never"
    ) %>%
    left_join(snomed_mapping,
      by = c("diagnosis_name_value_only" = "Fully Specified Names (FSNs)")
    ) %>%
    mutate(
      disorder_diagnosis =
        clean_string_to_join(coalesce(Diagnosis.x, Diagnosis.y))
    ) %>%
    # Joining to APACHE II and cleaning up the diagnosis name
    left_join(
      ap2,
      by = c("disorder_diagnosis" = "APACHE IV diagnosis"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      diagnosis_name_value_only_disorder = diagnosis_name_value_only,
      disorder_ap2 = `APACHE II`
    )

  ##### Doing the same for operation 1.
  operation_1 <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^operation1,*", diagnosis_name)) %>%
    mutate(concept_code = as.character(concept_code)) %>%
    #### The source name has both variable and value. Getting value only.
    #### It's the section before the first comma.
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only)) %>%
    ##### Joining all the diagnoses by concept ID and name
    # Operation Joining on both name and concept ID because there are mismatches.
    left_join(
      snomed_mapping,
      by = c("concept_code" = "Variable ID"),
      na_matches = "never"
    ) %>%
    left_join(snomed_mapping,
      by = c("diagnosis_name_value_only" = "Fully Specified Names (FSNs)")
    ) %>%
    mutate(
      operation_diagnosis =
        clean_string_to_join(coalesce(Diagnosis.x, Diagnosis.y))
    ) %>%
    # Joining to APACHE II and cleaning up the diagnosis name
    left_join(
      ap2,
      by = c("operation_diagnosis" = "APACHE IV diagnosis"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      diagnosis_name_value_only_operation = diagnosis_name_value_only,
      operation_ap2 = `APACHE II`
    )

  #### The apache diagnosis is simpler because it's already in a AP4 format.
  apache_iv <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^apache_diagnosis,", diagnosis_name)) %>%
    #### These diags have the system name as well, which won't match.
    # They are before the second comma. Removing them.
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name_value_only)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only)) %>%
    left_join(
      ap2,
      by = c("diagnosis_name_value_only" = "APACHE IV diagnosis"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      diagnosis_name_value_only_ap2 = diagnosis_name_value_only,
      apache_iv_ap2 = `APACHE II`
    )

  #### Now joining them all together and picking primary diagnosis
  # Picking primary diagnosis.
  data <- data %>%
    distinct(person_id, visit_occurrence_id, visit_detail_id) %>%
    left_join(apache_iv,
      by = c("person_id", "visit_occurrence_id", "visit_detail_id")
    ) %>%
    left_join(operation_1,
      by = c("person_id", "visit_occurrence_id", "visit_detail_id")
    ) %>%
    left_join(disorder_1,
      by = c("person_id", "visit_occurrence_id", "visit_detail_id")
    ) %>%
    mutate(
      primary_diagnosis_name =
        coalesce(
          diagnosis_name_value_only_ap2,
          diagnosis_name_value_only_operation,
          diagnosis_name_value_only_disorder
        )
    ) %>%
    mutate(primary_diag_ap2 = clean_string_to_join(coalesce(
      apache_iv_ap2,
      operation_ap2, disorder_ap2
    ))) %>%
    # Getting the coefficients based on the diag string.
    left_join(ap2_coefs,
      by = c("primary_diag_ap2" = "name"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      primary_diagnosis_name,
      primary_diag_ap2,
      ap2_diag_coef = coefficient
    )

  data
}

#' Calculates APACHE II prob death
#' @param data Dataset after running the calculate_apache_ii_score() function.
#' @param coef_data Data after get_apache_ii_coefficents has been run.
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
calculate_apache_ii_prob <- function(data, coef_data) {
  data <-
    left_join(data,
      coef_data,
      by = c(
        "person_id", "visit_occurrence_id",
        "visit_detail_id"
      )
    )

  # Now calculating prob mortality.
  data <- data %>%
    mutate(
      surgical_coef = if_else(emergency_admission == "Yes", 0.603, 0, 0),
      logit_ap2 = -3.517 + (0.146 * apache_ii_score) + ap2_diag_coef
        + surgical_coef,
      apache_ii_prob = exp(logit_ap2) / (1 + exp(logit_ap2))
    ) %>%
    select(-c("surgical_coef", "logit_ap2"))

  data
}

#' Gets diagnosis coefficents for APACHE II probabilty of death calculation
#' for NICE.
#' @param data Dataset after get_diagnoses.sql & download_mapping_data function
#' @param output_path file path data is written here
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
get_apache_ii_coefficents_NICE <- function(data, output_path) {
  # Reading in the various mapping sheets.
  # Forcing only one version of the main IDs because joins break otherwise.
  # Have also tried to correct on mapping sheets.
  snomed_mapping <-
    read_csv(glue("{output_path}/data/snomed_ap4.csv")) %>%
    mutate(
      `Variable ID` = as.character(`Variable ID`),
      `Fully Specified Names (FSNs)` = clean_string_to_join(`Fully Specified Names (FSNs)`)
    ) %>%
    distinct(`Variable ID`, .keep_all = TRUE) %>%
    distinct(`Fully Specified Names (FSNs)`, .keep_all = TRUE)
  ap2 <- read_csv(glue("{output_path}/data/ap2.csv")) %>%
    mutate(`APACHE IV diagnosis` = clean_string_to_join(`APACHE IV diagnosis`)) %>%
    distinct(`APACHE IV diagnosis`, .keep_all = TRUE)
  ap2_coefs <-
    read_csv(glue("{output_path}/data/ap2_coefs.csv")) %>%
    mutate(name = clean_string_to_join(name))

  #### Cleaning up the diagnosis name
  #### The source name has both variable and value. Getting value only.
  #### It's the section before the first comma.
  data <- data %>%
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only))

  # Getting primary diagnoses.
  # Order as follows.
  # First apache_diagnosis, because that was the primary when AP4 was collected.
  # Then operation 1 because that's the first option for operative patients
  # Then disorder 1 because that's the first option for non operative patients.
  disorder_1 <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^disorder1,*", diagnosis_name)) %>%
    mutate(concept_code = as.character(concept_code)) %>%
    ##### Joining all the diagnoses by concept ID and name
    # Disorder. Joining on both name and concept ID because there are mismatches.
    left_join(
      snomed_mapping,
      by = c("concept_code" = "Variable ID"),
      na_matches = "never"
    ) %>%
    left_join(snomed_mapping,
      by = c("diagnosis_name_value_only" = "Fully Specified Names (FSNs)")
    ) %>%
    mutate(
      disorder_diagnosis =
        clean_string_to_join(coalesce(Diagnosis.x, Diagnosis.y))
    ) %>%
    # Joining to APACHE II and cleaning up the diagnosis name
    left_join(
      ap2,
      by = c("disorder_diagnosis" = "APACHE IV diagnosis"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      diagnosis_name_value_only_disorder = diagnosis_name_value_only,
      disorder_ap2 = `APACHE II`
    )

  ##### Doing the same for operation 1.
  operation_1 <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^operation1,*", diagnosis_name)) %>%
    mutate(concept_code = as.character(concept_code)) %>%
    #### The source name has both variable and value. Getting value only.
    #### It's the section before the first comma.
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only)) %>%
    ##### Joining all the diagnoses by concept ID and name
    # Operation Joining on both name and concept ID because there are mismatches.
    left_join(
      snomed_mapping,
      by = c("concept_code" = "Variable ID"),
      na_matches = "never"
    ) %>%
    left_join(snomed_mapping,
      by = c("diagnosis_name_value_only" = "Fully Specified Names (FSNs)")
    ) %>%
    mutate(
      operation_diagnosis =
        clean_string_to_join(coalesce(Diagnosis.x, Diagnosis.y))
    ) %>%
    # Joining to APACHE II and cleaning up the diagnosis name
    left_join(
      ap2,
      by = c("operation_diagnosis" = "APACHE IV diagnosis"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      diagnosis_name_value_only_operation = diagnosis_name_value_only,
      operation_ap2 = `APACHE II`
    )

  #### The apache diagnosis is simpler because it's already in a AP4 format.
  apache_iv <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^apache_diagnosis,", diagnosis_name)) %>%
    #### These diags have the system name as well, which won't match.
    # They are before the second comma. Removing them.
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name_value_only)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only)) %>%
    left_join(
      ap2,
      by = c("diagnosis_name_value_only" = "APACHE IV diagnosis"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      diagnosis_name_value_only_ap2 = diagnosis_name_value_only,
      apache_iv_ap2 = `APACHE II`
    )

  #### Now joining them all together and picking primary diagnosis
  # Picking primary diagnosis.
  data <- data %>%
    distinct(person_id, visit_occurrence_id, visit_detail_id) %>%
    left_join(apache_iv,
      by = c("person_id", "visit_occurrence_id", "visit_detail_id")
    ) %>%
    left_join(operation_1,
      by = c("person_id", "visit_occurrence_id", "visit_detail_id")
    ) %>%
    left_join(disorder_1,
      by = c("person_id", "visit_occurrence_id", "visit_detail_id")
    ) %>%
    mutate(
      primary_diagnosis_name =
        coalesce(
          diagnosis_name_value_only_ap2,
          diagnosis_name_value_only_operation,
          diagnosis_name_value_only_disorder
        )
    ) %>%
    mutate(primary_diag_ap2 = clean_string_to_join(coalesce(
      apache_iv_ap2,
      operation_ap2, disorder_ap2
    ))) %>%
    # Getting the coefficients based on the diag string.
    left_join(ap2_coefs,
      by = c("primary_diag_ap2" = "name"),
      na_matches = "never"
    ) %>%
    select(
      person_id,
      visit_occurrence_id,
      visit_detail_id,
      primary_diagnosis_name,
      primary_diag_ap2,
      ap2_diag_coef = coefficient
    )

  data
}
