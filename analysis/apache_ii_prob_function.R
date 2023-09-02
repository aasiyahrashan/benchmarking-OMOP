#' Remove special characters and spaces from a string variable to make it easier to join.
#' @param string_var string to be cleaned
#' @import stringr
#' @noRd
clean_string_to_join <- function(string_var){
  string_var <- as.character(string_var)
  string_var <- tolower(string_var)
  string_var <- str_trim(string_var)
  string_var <- str_squish(string_var)
  string_var <- str_replace_all(string_var, "(disorder)", "")
  string_var <- str_replace_all(string_var, "(procedure)", "")
  string_var <- str_replace_all(string_var, "[^[:alnum:]]", "")
}

download_mapping_files <- function(snomed_mapping_path, ap2_path, ap2_coefs_path, output_path){

  # Only downloading the data if it doesn't already exist.
  files_dont_exist <-
    !file.exists(glue("{output_path}/data/snomed_ap4.csv"),
                 glue("{output_path}/data/ap2.csv"),
                 glue("{output_path}/data/ap2_coefs.csv"))

  if(any(files_dont_exist)){
      googlesheets4::gs4_auth(email = "*@nicslk.com")

    # Downloading data and saving it to csv.
    print("Downloading mapping data")

    read_excel(snomed_mapping_path, skip =1, sheet = "Mapped") %>%
      mutate(`Variable ID` = as.character(`Variable ID`)) %>%
      write_csv(file = glue("{output_path}/data/snomed_ap4.csv"))

    ap2 <- read_sheet(ap2_path, sheet = "Sh1-AP4toAP2") %>%
      write_csv(file = glue("{output_path}/data/ap2.csv"))
    ap2_coefs <- read_sheet(ap2_coefs_path) %>%
      write_csv(file = glue("{output_path}/data/ap2_coefs.csv"))
  }
}


#' Calculates APACHE II prob death
#' @param data Dataset after the SeverityScoresOMOP::calculate_apache_ii_score() function has been run.
#' @param output_path file path data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
get_apache_ii_coefficents <- function(admission_data, output_path){

  # Reading in the various mapping sheets.
  # Forcing only one version of the main IDs because joins break otherwise.
  # Have also tried to correct on mapping sheets.
  snomed_mapping <- read_csv(glue("{output_path}/data/snomed_ap4.csv")) %>%
    mutate(`Variable ID` = as.character(`Variable ID`),
           `Fully Specified Names (FSNs)` = clean_string_to_join(`Fully Specified Names (FSNs)`)) %>%
    distinct(`Variable ID`, .keep_all = TRUE) %>%
    distinct(`Fully Specified Names (FSNs)`, .keep_all = TRUE)
  ap2 <- read_csv(glue("{output_path}/data/ap2.csv")) %>%
    mutate(`APACHE IV diagnosis` = clean_string_to_join(`APACHE IV diagnosis`)) %>%
    distinct(`APACHE IV diagnosis`, .keep_all = TRUE)
  ap2_coefs <- read_csv(glue("{output_path}/data/ap2_coefs.csv")) %>%
    mutate(name = clean_string_to_join(name))


  # Getting primary diagnoses.
  # Order as follows.
  # First apache_diagnosis, because that was the primary when AP4 was collected.
  # Then operation 1 because that's the first option for operative patients
  # Then disorder 1 because that's the first option for non operative patients.
  disorder_1 <- admission_data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^disorder1,*", diagnosis_name)) %>%
    mutate(concept_code = as.character(concept_code)) %>%
    #### The source name has both variable and value. Getting value only.
    #### It's the section before the first comma.
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only)) %>%
    ##### Joining all the diagnoses by concept ID and name
    # Disorder. Joining on both name and concept ID because there are mismatches.
    left_join(snomed_mapping, by = c("concept_code" = "Variable ID"),
              na_matches = "never") %>%
    left_join(snomed_mapping,
              by = c("diagnosis_name_value_only" = "Fully Specified Names (FSNs)")) %>%
    mutate(disorder_diagnosis =
             clean_string_to_join(coalesce(Diagnosis.x, Diagnosis.y))) %>%
    select(-Diagnosis.x, -Diagnosis.y) %>%
   # Joining to APACHE II and cleaning up the diagnosis name
    left_join(ap2, by = c("disorder_diagnosis" = "APACHE IV diagnosis"), na_matches = "never") %>%
    rename(disorder_ap2 = `APACHE II`) %>%
    select(person_id, visit_occurrence_id, visit_detail_id, disorder_ap2)

  ##### Doing the same for operation 1.
  operation_1 <- admission_data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^operation1,*", diagnosis_name)) %>%
    mutate(concept_code = as.character(concept_code)) %>%
    #### The source name has both variable and value. Getting value only.
    #### It's the section before the first comma.
    mutate(diagnosis_name_value_only = sub("^[^,]*,", "", diagnosis_name)) %>%
    mutate(diagnosis_name_value_only = clean_string_to_join(diagnosis_name_value_only)) %>%
    ##### Joining all the diagnoses by concept ID and name
    # Operation Joining on both name and concept ID because there are mismatches.
    left_join(snomed_mapping, by = c("concept_code" = "Variable ID"),
              na_matches = "never") %>%
    left_join(snomed_mapping,
              by = c("diagnosis_name_value_only" = "Fully Specified Names (FSNs)")) %>%
    mutate(operation_diagnosis =
             clean_string_to_join(coalesce(Diagnosis.x, Diagnosis.y))) %>%
    select(-Diagnosis.x, -Diagnosis.y) %>%
    # Joining to APACHE II and cleaning up the diagnosis name
    left_join(ap2, by = c("operation_diagnosis" = "APACHE IV diagnosis"), na_matches = "never") %>%
    rename(operation_ap2 = `APACHE II`) %>%
    select(person_id, visit_occurrence_id, visit_detail_id, operation_ap2)

  #### The apache diagnosis is simpler because it's already in a AP4 format.
  apache_iv <- admission_data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("apache_diagnosis,*", diagnosis_name)) %>%
    left_join(ap2, by = c("primary_apache_ii" = "APACHE IV diagnosis"),
              na_matches = "never") %>%
    rename(apache_iv_ap2 = `APACHE II`)

  #### Now joining them all together and picking primary diagnosis
  # Picking primary diagnosis.
  admission_data %>%
    left_join(apache_iv, by = c("person_id", "visit_occurrence_id", "visit_detail_id")) %>%
    left_join(operation_1, by = c("person_id", "visit_occurrence_id", "visit_detail_id")) %>%
    left_join(disorder_1, by = c("person_id", "visit_occurrence_id", "visit_detail_id")) %>%
    mutate(primary_diag_ap2 = clean_string_to_join(coalesce(apache_iv_ap2,
                                                            operation_ap2, disorder_ap2))) %>%
    # Getting the coefficients based on the diag string.
    left_join(ap2_coefs, by = c("primary_diag_ap2" = "name"),
              na_matches = "never") %>%
    rename(ap2_diag_coef = coefficient) %>%
    select(person_id, visit_occurrence_id, visit_detail_id, ap2_diag_coef)
}

calculate_apache_ii_prob <- function(data, coef_data){

  data <- left_join(data, coef_data, on = c("person_id", "visit_occurrence_id",
                                            "visit_detail_id"))

  # Now calculating prob mortality.
  data <- data %>%
    mutate(surgical_coef = if_else(emergency_admission == "Yes", 0.603, 0, 0),
           logit_ap2 = -3.517 + (0.146*apache_ii_score) + ap2_diag_coef
           + surgical_coef,
           apache_ii_prob = exp(logit_ap2)/(1 + exp(logit_ap2))) %>%
    select(patient_id, apache_ii_prob)

  data
}
