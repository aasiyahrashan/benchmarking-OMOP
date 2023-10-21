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


#' Remove special characters and spaces from a string variable to make it easier to join.
#' @param string_var string to be cleaned
#' @import stringr
#' @noRd
clean_freetext_strings <- function(string_var){

  string_var <- as.character(string_var)
  string_var <- tolower(string_var)
  string_var <- str_replace_all(string_var, "[[:punct:]]", "")
  string_var <- str_squish(string_var)
  string_var <- if_else(string_var == "", as.character(NA), string_var)

}

apply_ccaa_specific_exclusions <- function(data, output_path){

  all_implementation <- read_csv(glue("{output_path}/data/all_implementation.csv"))
  all_implementation <- all_implementation %>%
    ### Only including valid registries
    filter(!is.na(`Unit ID`)) %>%
    filter(Registry %in% c("PRICE", "IRIS", "NICR", "Afghanistan",
                           "Malaysia", "Bangladesh", "Kenya", "Uganda", "Ghana",
                           "Sierra Leone", "South Africa", "Ethiopia")) %>%
    ### Excluding test units, wards, pediatric and neonatal ICUs, HDUs, maternity units,
    # emergency unit.
    filter(!`ICU Type` %in% c("TEST", "WARD", "PEDIATRIC", "NEONATAL")) %>%
    filter(!grepl("*PICU*", `ICU Name`, ignore.case=TRUE),
           !grepl("*Pediatric*", `ICU Name`, ignore.case=TRUE),
           !grepl("*Ward*", `ICU Name`, ignore.case=TRUE),
           !grepl("*HDU*", `ICU Name`, ignore.case=TRUE),
           !grepl("*High Risk Unit*", `ICU Name`, ignore.case=TRUE),
           !grepl("*High Dependency*", `ICU Name`, ignore.case=TRUE),
           !grepl("*High care unit*", `ICU Name`, ignore.case=TRUE),
           !grepl("*GyneObs ICU*", `ICU Name`, ignore.case=TRUE),
           !grepl("*Gyn/Obs*", `ICU Name`, ignore.case=TRUE),
           !grepl("*OBS*", `ICU Name`, ignore.case=TRUE),
           !grepl("*Gyne Post Operative*", `ICU Name`, ignore.case=TRUE),
           !grepl("*G/OICU*", `ICU Name`, ignore.case=TRUE),
           !grepl("*Obstetric ICU*", `ICU Name`, ignore.case=TRUE),
           !grepl("*Maternal HDU*", `ICU Name`, ignore.case=TRUE),
           !grepl("EU", `ICU Name`, ignore.case=TRUE),
           !grepl("ED", `ICU Name`, ignore.case=TRUE))

  #### Joining to the main dataset so only patients admitted to allowed units are included.
  data <- data %>%
    mutate(`Unit ID` = gsub( " .*$", "", care_site_name)) %>%
    inner_join(all_implementation, by = "Unit ID")

  data
}

download_mapping_files <- function(freetext_mapping_path, snomed_mapping_path,
                                   ap2_path, ap2_coefs_path,
                                   implementation_asia_path,
                                   implementation_africa_path,
                                   output_path){

  # Only downloading the data if it doesn't already exist.
  files_dont_exist <-
    !file.exists(glue("{output_path}/data/freetext_to_snomed.csv"),
                 glue("{output_path}/data/snomed_ap4.csv"),
                 glue("{output_path}/data/ap2.csv"),
                 glue("{output_path}/data/ap2_coefs.csv"),
                 glue("{output_path}/data/all_implementation.csv"))

  if(any(files_dont_exist)){
      googlesheets4::gs4_auth(email = "*@nicslk.com")

    # Downloading data and saving it to csv.
    print("Downloading mapping data")

    read_excel(snomed_mapping_path, skip =1, sheet = "Mapped") %>%
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
      #### This removes the values from the vector above. Removing invalid registries and metadata sheets.
      setdiff(c("CCA", "NICS", "Demo", "sheet",
                "weekly validation", "Live Sites Contacts", "Troubleshooting",
                "Sheet9", "check list ", "expenses", "Philippines", "Sri Lanka",
                "Maldives", "Indonesia")) %>%
      set_names() %>%
      map_df(~read_sheet(ss = implementation_asia_path, sheet = ., skip = 7) %>%
               select(`Hospital name`, `ICU Name`, `Unit ID`, `ICU Type`),
             .id = "Registry")

    africa <- implementation_africa_path %>%
      sheet_names() %>%
      #### This removes the values from the vector above. Removing invalid registries and metadata sheets
      setdiff(c("Ethical approvals", "All GECO", "NICS", "weekly validation",
                "Live Sites Contacts", "Troubleshooting",
                "Sheet9", "check list ", "expenses", "Tasks for Madiha",
                "Nigeria", "Namibia", "Cameroon")) %>%
      set_names() %>%
      map_df(~read_sheet(ss = implementation_africa_path, sheet = ., skip = 7) %>%
               select(`Hospital name`, `ICU Name`, `Unit ID`, `ICU Type`),
             .id = "Registry")

      rbind(asia, africa) %>%
      write_csv(file = glue("{output_path}/data/all_implementation.csv"))
  }
}

#' Extracting the primary or all apache iv diagnosis and snomed diagnosis
#' This function will give 3 columns named primary_apache, extracted_snomed_diag and extracted_snomed_code
#' For 'primary' diagnosis type, if diagnosis was entered as apache iv, then primary_apache captures it
#' Else extracted_snomed_diag and extracted_snomed_code were extracted from snomed operations and disorders
#' @param data admission dataset
#' @import dplyr
#' @noRd
extract_snomed_and_apache_diagnosis <- function(data){

  ### First making the snomed code missing if it's mapped to 0.
  data <- data %>%
    mutate(concept_code = if_else(concept_code == "No matching concept", NA,  concept_code))

  disorder_1 <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^disorder1,*", diagnosis_name)) %>%
    mutate(extracted_snomed_diag_dis = diagnosis_name,
           extracted_snomed_code_dis = concept_code) %>%
    select(person_id, visit_occurrence_id, visit_detail_id,
           extracted_snomed_diag_dis, extracted_snomed_code_dis) %>%
    distinct()

  operation_1 <- data %>%
    filter(grepl("^operation1,*", diagnosis_name)) %>%
    mutate(extracted_snomed_diag_op = diagnosis_name,
           extracted_snomed_code_op = concept_code) %>%
    select(person_id, visit_occurrence_id, visit_detail_id,
           extracted_snomed_diag_op, extracted_snomed_code_op) %>%
    distinct()

  apache_iv <- data %>%
    ### Only getting diagnoses that have a snomed vocabulary ID
    filter(grepl("^apache_diagnosis,", diagnosis_name)) %>%
    mutate(extracted_apache_diag = diagnosis_name) %>%
    select(person_id, visit_occurrence_id, visit_detail_id,
           extracted_apache_diag) %>%
    distinct()

  data <- data %>%
    distinct(person_id, visit_occurrence_id, visit_detail_id) %>%
    left_join(apache_iv, by = c("person_id", "visit_occurrence_id", "visit_detail_id")) %>%
    left_join(operation_1, by = c("person_id", "visit_occurrence_id", "visit_detail_id")) %>%
    left_join(disorder_1, by = c("person_id", "visit_occurrence_id", "visit_detail_id")) %>%
    ### Picking the correct SNOMED name if available.
    mutate(extracted_snomed_diag = case_when(
      !is.na(extracted_snomed_diag_op) ~ extracted_snomed_diag_op,
      !is.na(extracted_snomed_diag_dis) ~ extracted_snomed_diag_dis)) %>%
    mutate(extracted_snomed_code = case_when(
      !is.na(extracted_snomed_diag_op)  ~ extracted_snomed_code_op,
      !is.na(extracted_snomed_diag_dis) ~ extracted_snomed_code_dis)) %>%
    #### Cleaning the diangnosis names to remove names of the variables.
    #### The substitution removes everything up to and including the first comma, and also any space after it.
    mutate(extracted_snomed_diag = sub("^[^,]*,\\s*", "", extracted_snomed_diag)) %>%
    mutate(extracted_apache_diag = sub("^[^,]*,\\s*", "", extracted_apache_diag)) %>%
    #### Running this substitution twice for the APACHE variables since the system is appended at the beginning separated by a comma.
    mutate(extracted_apache_diag = sub("^[^,]*,\\s*", "", extracted_apache_diag))

  data
}


#' Extracts the releveant snomed diagnosis and snomed code for the diagnosis entered as freetext
#' @param admission_data admission_data that contains snomed_diag and snomed_code columns
#' @param output_path path to the folder containing dataset of freetexts and relevant snomed codes
#' @import dplyr
#' @noRd
freetext_mapping_to_snomed <- function(admission_data, output_path){

  freetext_mapped <- read_csv(glue("{output_path}/data/freetext_to_snomed.csv"))

  admission_data <- admission_data %>%
    mutate(extracted_snomed_diag = if_else(is.na(extracted_snomed_code),
                                           clean_freetext_strings(extracted_snomed_diag),
                                           extracted_snomed_diag)) %>%
    left_join(freetext_mapped, by = c("extracted_snomed_diag" = "sourceName")) %>%
    distinct(person_id, visit_occurrence_id, visit_detail_id,
             extracted_snomed_diag, .keep_all = TRUE) %>%
    mutate(extracted_snomed_diag = if_else(is.na(extracted_snomed_code) &
                                             !is.na(`SNOMED code`) &
                                             mappingStatus != "FLAGGED", targetConceptName,
                                           extracted_snomed_diag, extracted_snomed_diag),
           extracted_snomed_code = if_else(is.na(extracted_snomed_code) &
                                             !is.na(`SNOMED code`) &
                                             mappingStatus != "FLAGGED", as.character(`SNOMED code`),
                                           extracted_snomed_code, extracted_snomed_code)) %>%
    select(-setdiff(names(freetext_mapped), c("sourceName")))

  admission_data
}

#' Maps the snomed diagnosis to APACHE IV
#' @param admission_data Needs to have extracted_apache_diag and extracted_snomed_code
#' #' @param output_path file path reports and data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
snomed_to_apache_iv_mapping <- function(admission_data, output_path){
  # Reading in the snomed to apache iv mapping sheets.
  snomed_mapping <- read_csv(glue("{output_path}/data/snomed_ap4.csv")) %>%
    mutate(`Variable ID` = as.character(`Variable ID`),
           `Fully Specified Names (FSNs)` = clean_string_to_join(`Fully Specified Names (FSNs)`)) %>%
    distinct(`Variable ID`, .keep_all = TRUE)

  admission_data <- admission_data %>%
    mutate_at(vars(extracted_snomed_code), ~as.character(.)) %>%
    # Joining the primary snomed diagnosis on concept ID
    left_join(snomed_mapping, by = c("extracted_snomed_code" = "Variable ID"),
              na_matches = "never") %>%
    mutate(apache_iv_diag = coalesce(extracted_apache_diag, Diagnosis)) %>%
    select(-setdiff(names(snomed_mapping), c("Variable ID")))

  admission_data
}

#' Maps the APACHE IV to APACHE II
#' @param admission_data Needs to have apache_iv_diag column
#' @param output_path file path reports and data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
apache_iv_to_apache_ii_mapping <- function(admission_data, output_path){

  # Reading in the apache iv to apache ii mapping sheets.
  ap2 <- read_csv(glue("{output_path}/data/ap2.csv")) %>%
    mutate(`APACHE IV diagnosis` = clean_string_to_join(`APACHE IV diagnosis`)) %>%
    distinct(`APACHE IV diagnosis`, .keep_all = TRUE)

  admission_data <- admission_data %>%
    mutate(apache_iv_cleaned = clean_string_to_join(apache_iv_diag)) %>%
    # Joining to APACHE II and cleaning up the diagnosis name
    left_join(ap2, by = c("apache_iv_cleaned" = "APACHE IV diagnosis"), na_matches = "never") %>%
    rename(apache_ii_diag = `APACHE II`) %>%
    select(-setdiff(names(ap2), c("APACHE IV diagnosis", "APACHE II")), -apache_iv_cleaned)

  admission_data
}

#' Maps the APACHE II diagnosis to coefficient
#' @param admission_data Needs to have apache_ii_diag column
#' @param output_path file path reports and data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
apache_ii_to_coefficient_mapping <- function(admission_data, output_path){

  # Reading in the apache ii to coefficient mapping sheet.
  ap2_coefs <- read_csv(glue("{output_path}/data/ap2_coefs.csv")) %>%
    mutate(name = clean_string_to_join(name)) %>%
    distinct(name, .keep_all = TRUE)

  admission_data <- admission_data %>%
    mutate(apache_ii_cleaned = clean_string_to_join(apache_ii_diag)) %>%
    # Getting the coefficients.
    left_join(ap2_coefs, by = c("apache_ii_cleaned" = "name"),
              na_matches = "never") %>%
    rename(ap2_diag_coef = coefficient) %>%
    select(-setdiff(names(ap2_coefs), c("name", "coefficient")), -apache_ii_cleaned)

  admission_data
}

#' Gets diangosis coefficents for APACHE II probabilty of death calculation.
#' @param data Dataset after get_diagnoses.sql query. The download_mapping_data function should also have been run.
#' @param output_path file path data is written to
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @noRd
get_apache_ii_coefficents <- function(data, output_path){

  # Extracting the primary diagnosis from APACHE IV and snomed disorder and operation
  data <- extract_snomed_and_apache_diagnosis(data)

  # Mapping the freetext diagnosis to SNOMED codes
  data <- freetext_mapping_to_snomed(data, output_path)

  # Mapping snomed to apache iv
  data <- snomed_to_apache_iv_mapping(data, output_path)

  # Mapping apache iv to apache ii
  data <- apache_iv_to_apache_ii_mapping(data, output_path)

  # Mapping apache ii to coefficients
  data <- apache_ii_to_coefficient_mapping(data, output_path)

  data <- data %>%
    select(person_id, visit_occurrence_id, visit_detail_id,
           extracted_apache_diag,
           extracted_snomed_diag,
           extracted_snomed_code,
           ap2_diag_coef)

  data
}

#' Calculates APACHE II prob death
#' @param data Dataset after the SeverityScoresOMOP::calculate_apache_ii_score() function has been run. Also after get_apache_ii_coefficents has been run. Assumes a variable called ap2_diag_coef is in the datset.
#' @import dplyr
#' @import stringr
#' @noRd
calculate_apache_ii_prob <- function(data){

  # Now calculating prob mortality.
  data <- data %>%
    mutate(surgical_coef = if_else(emergency_admission == "Yes", 0.603, 0, 0),
           logit_ap2 = -3.517 + (0.146*apache_ii_score) + ap2_diag_coef
           + surgical_coef,
           apache_ii_prob = exp(logit_ap2)/(1 + exp(logit_ap2))) %>%
    select(-c("surgical_coef", "logit_ap2"))

  data
}
