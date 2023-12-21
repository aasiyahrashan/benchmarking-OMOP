#' CCAA specific
#' Remove special characters and spaces from a string variable to make it
#' easier to join.
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


#' CCAA specific
#' Remove special characters and spaces from a string variable to make it
#' easier to join.
#' @param string_var string to be cleaned
#' @import stringr
#' @noRd
clean_freetext_strings <- function(string_var) {
  string_var <- as.character(string_var)
  string_var <- tolower(string_var)
  string_var <- str_replace_all(string_var, "[[:punct:]]", "")
  string_var <- str_squish(string_var)
  string_var <- if_else(string_var == "", as.character(NA), string_var)
}

#' CCAA specific
#' Download CCAA specific mapping files for APACHE II and to define inclusion.
#' @import googlesheets4
download_mapping_files <- function(freetext_mapping_path, snomed_mapping_path,
                                   ap2_path, ap2_coefs_path,
                                   implementation_asia_path,
                                   implementation_africa_path,
                                   units_of_measure_path,
                                   output_path) {
  # Only downloading the data if it doesn't already exist.
  files_dont_exist <-
    !file.exists(
      glue("{output_path}/data/freetext_to_snomed.csv"),
      glue("{output_path}/data/snomed_ap4.csv"),
      glue("{output_path}/data/ap2.csv"),
      glue("{output_path}/data/ap2_coefs.csv"),
      glue("{output_path}/data/all_implementation.csv"),
      glue("{output_path}/data/measures.csv")
      )

  if (any(files_dont_exist)) {
    googlesheets4::gs4_auth(email = "*@nicslk.com")

    # Downloading data and saving it to csv.
    print("Downloading mapping data")

    read_excel(snomed_mapping_path, skip = 1, sheet = "Mapped") %>%
      mutate(`Variable ID` = as.character(`Variable ID`)) %>%
      write_csv(file = glue("{output_path}/data/snomed_ap4.csv"))

    freetext_mapped <- read_sheet(freetext_mapping_path) %>%
      write_csv(file = glue("{output_path}/data/freetext_to_snomed.csv"))
    ap2 <- read_sheet(ap2_path, sheet = "Sh1-AP4toAP2") %>%
      write_csv(file = glue("{output_path}/data/ap2.csv"))
    ap2_coefs <- read_sheet(ap2_coefs_path) %>%
      write_csv(file = glue("{output_path}/data/ap2_coefs.csv"))

    #### Getting implementation sheet data.
    asia <- implementation_asia_path %>%
      sheet_names() %>%
      #### This removes the values from the vector above.
      # Removing invalid registries and metadata sheets.
      setdiff(c(
        "CCA", "NICS", "Demo", "sheet",
        "weekly validation", "Live Sites Contacts", "Troubleshooting",
        "Sheet9", "check list ", "expenses", "Philippines", "Sri Lanka",
        "Maldives", "Indonesia"
      )) %>%
      set_names() %>%
      map_df(
        ~ read_sheet(ss = implementation_asia_path, sheet = ., skip = 7) %>%
          select(`Hospital name`, `ICU Name`, `Unit ID`, `ICU Type`),
        .id = "Registry"
      )

    africa <- implementation_africa_path %>%
      sheet_names() %>%
      ## This removes the values from the vector above. Removing invalid
      ## registries and metadata sheets
      setdiff(c(
        "Ethical approvals", "All GECO", "NICS", "weekly validation",
        "Live Sites Contacts", "Troubleshooting",
        "Sheet9", "check list ", "expenses", "Tasks for Madiha",
        "Nigeria", "Namibia", "Cameroon"
      )) %>%
      set_names() %>%
      map_df(
        ~ read_sheet(ss = implementation_africa_path, sheet = ., skip = 7) %>%
          select(`Hospital name`, `ICU Name`, `Unit ID`, `ICU Type`),
        .id = "Registry"
      )

    rbind(asia, africa) %>%
      write_csv(file = glue("{output_path}/data/all_implementation.csv"))

    read_sheet(units_of_measure_path, sheet = "ProposedFormat") %>%
      janitor::clean_names() %>%
      rename_with(~paste0(., "_measure"),
                  !contains(c("registry", "hospital_name",
                              "icu_name", "unit_id"))) %>%
      write_csv(file = glue("{output_path}/data/measures.csv"))
  }
}

#' CCAA specific
#' Extracting the primary or all apache iv diagnosis and snomed diagnosis
#' This function will give 3 columns named primary_apache, extracted_snomed_diag
#' and extracted_snomed_code
#' For 'primary' diagnosis type, if diagnosis was entered as apache iv, then
#' primary_apache captures it
#' Else extracted_snomed_diag and extracted_snomed_code were extracted from
#' snomed operations and disorders
#' @param data admission dataset
#' @import dplyr
#' @noRd
extract_snomed_and_apache_diagnosis <- function(data) {
  ### First making the snomed code missing if it's mapped to 0.
  data <- data %>%
    mutate(concept_code = if_else(concept_code == "No matching concept",
      NA, concept_code
    ))

  disorder_1 <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^disorder1,*", diagnosis_name)) %>%
    mutate(
      extracted_snomed_diag_dis = diagnosis_name,
      extracted_snomed_code_dis = concept_code
    ) %>%
    select(
      person_id, visit_occurrence_id, visit_detail_id,
      extracted_snomed_diag_dis, extracted_snomed_code_dis
    ) %>%
    distinct()

  operation_1 <- data %>%
    filter(grepl("^operation1,*", diagnosis_name)) %>%
    mutate(
      extracted_snomed_diag_op = diagnosis_name,
      extracted_snomed_code_op = concept_code
    ) %>%
    select(
      person_id, visit_occurrence_id, visit_detail_id,
      extracted_snomed_diag_op, extracted_snomed_code_op
    ) %>%
    distinct()

  apache_iv <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^apache_diagnosis,", diagnosis_name)) %>%
    mutate(extracted_apache_diag = diagnosis_name) %>%
    select(
      person_id, visit_occurrence_id, visit_detail_id,
      extracted_apache_diag
    ) %>%
    distinct()

  data <- data %>%
    distinct(person_id, visit_occurrence_id, visit_detail_id) %>%
    left_join(apache_iv, by = c(
      "person_id", "visit_occurrence_id",
      "visit_detail_id"
    )) %>%
    left_join(operation_1, by = c(
      "person_id", "visit_occurrence_id",
      "visit_detail_id"
    )) %>%
    left_join(disorder_1, by = c(
      "person_id", "visit_occurrence_id",
      "visit_detail_id"
    )) %>%
    ### Picking the correct SNOMED name if available.
    mutate(extracted_snomed_diag = case_when(
      !is.na(extracted_snomed_diag_op) ~ extracted_snomed_diag_op,
      !is.na(extracted_snomed_diag_dis) ~ extracted_snomed_diag_dis
    )) %>%
    mutate(extracted_snomed_code = case_when(
      !is.na(extracted_snomed_diag_op) ~ extracted_snomed_code_op,
      !is.na(extracted_snomed_diag_dis) ~ extracted_snomed_code_dis
    )) %>%
    #### Cleaning the diangnosis names to remove names of the variables.
    #### The substitution removes everything up to and including the first comma,
    ## and also any space after it.
    mutate(
      extracted_snomed_diag =
        sub("^[^,]*,\\s*", "", extracted_snomed_diag)
    ) %>%
    mutate(
      extracted_apache_diag =
        sub("^[^,]*,\\s*", "", extracted_apache_diag)
    ) %>%
    ## Running this substitution twice for the APACHE variables since the
    # system is appended at the beginning separated by a comma.
    mutate(extracted_apache_diag = sub("^[^,]*,\\s*", "", extracted_apache_diag))

  data
}


#' CCAA specific
#' Extracts the releveant snomed diagnosis and snomed code for the
#' diagnosis entered as freetext
#' @param admission_data admission_data that contains snomed_diag and
#' snomed_code columns
#' @param output_path path to the folder containing dataset of freetexts
#' and relevant snomed codes
#' @import dplyr
#' @noRd
freetext_mapping_to_snomed <- function(admission_data, output_path) {
  freetext_mapped <- read_csv(glue("{output_path}/data/freetext_to_snomed.csv"))

  admission_data <- admission_data %>%
    mutate(extracted_snomed_diag = if_else(is.na(extracted_snomed_code),
      clean_freetext_strings(extracted_snomed_diag),
      extracted_snomed_diag
    )) %>%
    left_join(freetext_mapped, by = c("extracted_snomed_diag" = "sourceName")) %>%
    distinct(person_id, visit_occurrence_id, visit_detail_id,
      extracted_snomed_diag,
      .keep_all = TRUE
    ) %>%
    mutate(
      extracted_snomed_diag = if_else(is.na(extracted_snomed_code) &
        !is.na(`SNOMED code`) &
        mappingStatus != "FLAGGED",
      targetConceptName,
      extracted_snomed_diag,
      extracted_snomed_diag
      ),
      extracted_snomed_code = if_else(is.na(extracted_snomed_code) &
        !is.na(`SNOMED code`) &
        mappingStatus != "FLAGGED",
      as.character(`SNOMED code`),
      extracted_snomed_code,
      extracted_snomed_code
      )
    ) %>%
    select(-setdiff(names(freetext_mapped), c("sourceName")))

  admission_data
}

#' CCAA specific
#' Maps the snomed diagnosis to APACHE IV
#' @param admission_data Needs to have extracted_apache_diag
#' and extracted_snomed_code
#' #' @param output_path file path reports and data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
snomed_to_apache_iv_mapping <- function(admission_data, output_path) {
  # Reading in the snomed to apache iv mapping sheets.
  snomed_mapping <- read_csv(glue("{output_path}/data/snomed_ap4.csv")) %>%
    mutate(
      `Variable ID` = as.character(`Variable ID`),
      `Fully Specified Names (FSNs)` = clean_string_to_join(`Fully Specified Names (FSNs)`)
    ) %>%
    distinct(`Variable ID`, .keep_all = TRUE)

  admission_data <- admission_data %>%
    mutate_at(vars(extracted_snomed_code), ~ as.character(.)) %>%
    # Joining the primary snomed diagnosis on concept ID
    left_join(snomed_mapping,
      by = c("extracted_snomed_code" = "Variable ID"),
      na_matches = "never"
    ) %>%
    mutate(apache_iv_diag = coalesce(extracted_apache_diag, Diagnosis)) %>%
    select(-setdiff(names(snomed_mapping), c("Variable ID")))

  admission_data
}

#' CCAA specific
#' Maps the APACHE IV to APACHE II
#' @param admission_data Needs to have apache_iv_diag column
#' @param output_path file path reports and data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
apache_iv_to_apache_ii_mapping <- function(admission_data, output_path) {
  # Reading in the apache iv to apache ii mapping sheets.
  ap2 <- read_csv(glue("{output_path}/data/ap2.csv")) %>%
    mutate(`APACHE IV diagnosis` = clean_string_to_join(`APACHE IV diagnosis`)) %>%
    distinct(`APACHE IV diagnosis`, .keep_all = TRUE)

  admission_data <- admission_data %>%
    mutate(apache_iv_cleaned = clean_string_to_join(apache_iv_diag)) %>%
    # Joining to APACHE II and cleaning up the diagnosis name
    left_join(ap2,
      by = c("apache_iv_cleaned" = "APACHE IV diagnosis"),
      na_matches = "never"
    ) %>%
    rename(apache_ii_diag = `APACHE II`) %>%
    select(
      -setdiff(names(ap2), c("APACHE IV diagnosis", "APACHE II")),
      -apache_iv_cleaned
    )

  admission_data
}

#' CCAA specific
#' Maps the APACHE II diagnosis to coefficient
#' @param admission_data Needs to have apache_ii_diag column
#' @param output_path file path reports and data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
apache_ii_to_coefficient_mapping <- function(admission_data, output_path) {
  # Reading in the apache ii to coefficient mapping sheet.
  ap2_coefs <- read_csv(glue("{output_path}/data/ap2_coefs.csv")) %>%
    mutate(name = clean_string_to_join(name)) %>%
    distinct(name, .keep_all = TRUE)

  admission_data <- admission_data %>%
    mutate(apache_ii_cleaned = clean_string_to_join(apache_ii_diag)) %>%
    # Getting the coefficients.
    left_join(ap2_coefs,
      by = c("apache_ii_cleaned" = "name"),
      na_matches = "never"
    ) %>%
    rename(ap2_diag_coef = coefficient) %>%
    mutate(primary_diagnosis_name = coalesce(extracted_apache_diag,
                                             extracted_snomed_diag)) %>%
    select(
      -setdiff(names(ap2_coefs), c("name", "coefficient")),
      -apache_ii_cleaned
    )

  admission_data
}

#' Gets diangosis coefficents for APACHE II probabilty of death calculation.
#' @param diag_data Dataset after get_diagnoses.sql query.
#' The download_mapping_data function should also have been run.
#' @param output_path file path data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
get_apache_ii_coefficents <- function(diag_data, dataset_name, output_path) {
  ### CCAA
  if (dataset_name == "CCAA") {
    # Extracting the primary diagnosis from APACHE IV
    # and snomed disorder and operation
    diag_data <- extract_snomed_and_apache_diagnosis(diag_data)

    # Mapping the freetext diagnosis to SNOMED codes
    diag_data <- freetext_mapping_to_snomed(diag_data, output_path)

    # Mapping snomed to apache iv
    diag_data <- snomed_to_apache_iv_mapping(diag_data, output_path)

    # Mapping apache iv to apache ii
    diag_data <- apache_iv_to_apache_ii_mapping(diag_data, output_path)

    # Mapping apache ii to coefficients
    diag_data <- apache_ii_to_coefficient_mapping(diag_data, output_path)

    coef_data <- diag_data %>%
      select(
        person_id, visit_occurrence_id, visit_detail_id,
        primary_diagnosis_name,
        extracted_apache_diag,
        extracted_snomed_diag,
        extracted_snomed_code,
        ap2_diag_coef
      )

    #### NICE
  } else if (dataset_name == "NICE") {
    nice_coef <- read_delim(glue("{getwd()}/analysis/nice_ap2_coefficients.csv"))

    coef_data <- diag_data %>%
      left_join(nice_coef,
        by = c("diagnosis_concept_id"),
        na_matches = "never"
      ) %>%
      select(
        person_id,
        visit_occurrence_id,
        visit_detail_id,
        primary_diagnosis_name,
        primary_diag_ap2,
        ap2_diag_coef
      )
  } else {
    stop(
      "No coefficients for this dataset found. The dataset_name variable ",
      "should be either 'CCAA' or 'NICE'."
    )
  }

  coef_data
}

#' Calculates APACHE II prob death
#' @param data Dataset after the SeverityScoresOMOP::calculate_apache_ii_score()
#' function has been run. Also after get_apache_ii_coefficents has been run.
#' Assumes a variable called ap2_diag_coef is in the datset.
#' @param imputation Determines the variable names the score and prob are stored in.
#' Either 'normal' or 'none'.
#' @import dplyr
#' @import stringr
#' @noRd
calculate_apache_ii_prob <- function(data, imputation = "normal") {
  if (imputation == "normal") {
    score_name <- "apache_ii_score"
    prob_name <- "apache_ii_prob"
  } else if (imputation == "none") {
    score_name <- "apache_ii_score_no_imputation"
    prob_name <- "apache_ii_prob_no_imputation"
  }

  # Now calculating prob mortality.
  data <- data %>%
    mutate(
      surgical_coef = if_else(emergency_admission == "Yes", 0.603, 0, 0),
      logit_ap2 = -3.517 + (0.146 * !!sym(score_name)) + ap2_diag_coef
        + surgical_coef,
      !!prob_name := exp(logit_ap2) / (1 + exp(logit_ap2))
    ) %>%
    select(-c("surgical_coef", "logit_ap2"))

  data
}
