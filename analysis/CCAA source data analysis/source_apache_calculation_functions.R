calculate_min_max_variables <- function(data){

  # Defining comorbidity lists
  acute_renal_failure <- c("Renal failure, Mild",
                           "Renal failure, Moderate to severe",
                           "CKD requiring dialysis")
  comorbid_list <- c(
    "AIDS", "Hepatic disease, Moderate to severe", "Renal failure, Moderate to severe",
    "Respiratory disease, Severe moderate", "Leukemia", "Lymphoma", "Metastatic cancer",
    "CKD requiring dialysis", "Cirrhosis", "GI bleeding", "Tumor", "Cerebrovascular disease"
  )


  # Creating min and max variables for APACHE II calculation
  data <- data %>%
    mutate(max_hr = pmax(Admission.heart_rate, AdmissionAssessment.heart_rate,
                         DailyAssessment.heart_rate, DailyAssessment.heart_rate2,
                         DailyAssessment.heart_rate3, na.rm = TRUE),
           min_hr = pmin(Admission.heart_rate, AdmissionAssessment.heart_rate,
                         DailyAssessment.heart_rate, DailyAssessment.heart_rate2,
                         DailyAssessment.heart_rate3, na.rm = TRUE),
           min_rr = pmin(Admission.respiratory_rate, AdmissionAssessment.respiratory_rate,
                         DailyAssessment.respiratory_rate, DailyAssessment.respiratory_rate2,
                         DailyAssessment.respiratory_rate3, na.rm = TRUE),
           max_rr = pmax(Admission.respiratory_rate, AdmissionAssessment.respiratory_rate,
                         DailyAssessment.respiratory_rate, DailyAssessment.respiratory_rate2,
                         DailyAssessment.respiratory_rate3, na.rm = TRUE),
           max_temp = pmax(Admission.temperature, AdmissionAssessment.temperature,
                           DailyAssessment.temperature, DailyAssessment.temperature2,
                           DailyAssessment.temperature3, na.rm = TRUE),
           min_temp = pmin(Admission.temperature, AdmissionAssessment.temperature,
                           DailyAssessment.temperature, DailyAssessment.temperature2,
                           DailyAssessment.temperature3, na.rm = TRUE),
           min_wcc = pmin(AdmissionAssessment.white_blood_cells,
                          DailyAssessment.highest_wcc, DailyAssessment.lowest_wcc,
                          SariDailyAssessment.white_blood_cells, na.rm = TRUE),
           max_wcc = pmax(AdmissionAssessment.white_blood_cells,
                          DailyAssessment.highest_wcc, DailyAssessment.lowest_wcc,
                          SariDailyAssessment.white_blood_cells, na.rm = TRUE),
           max_sbp = pmax(Admission.systolic_blood_pressure,
                          AdmissionAssessment.systolic_blood_pressure,
                          DailyAssessment.systolic_blood_pressure,
                          DailyAssessment.systolic_blood_pressure2,
                          DailyAssessment.systolic_blood_pressure3,
                          SariDailyAssessment.systolic_blood_pressure,
                          na.rm = TRUE),
           min_sbp = pmin(Admission.systolic_blood_pressure,
                          AdmissionAssessment.systolic_blood_pressure,
                          DailyAssessment.systolic_blood_pressure,
                          DailyAssessment.systolic_blood_pressure2,
                          DailyAssessment.systolic_blood_pressure3,
                          SariDailyAssessment.systolic_blood_pressure,
                          na.rm = TRUE),
           min_dbp = pmin(Admission.diastolic_blood_pressure,
                          AdmissionAssessment.diastolic_blood_pressure,
                          DailyAssessment.diastolic_blood_pressure,
                          DailyAssessment.diastolic_blood_pressure2,
                          DailyAssessment.diastolic_blood_pressure3,
                          SariDailyAssessment.diastolic_blood_pressure,
                          na.rm = TRUE),
           max_dbp = pmax(Admission.diastolic_blood_pressure,
                          AdmissionAssessment.diastolic_blood_pressure,
                          DailyAssessment.diastolic_blood_pressure,
                          DailyAssessment.diastolic_blood_pressure2,
                          DailyAssessment.diastolic_blood_pressure3,
                          SariDailyAssessment.diastolic_blood_pressure,
                          na.rm = TRUE),
           max_fio2 = pmax(AdmissionAssessment.fraction_inspired_oxygen,
                           SariAdmissionAssessment.fraction_inspired_oxygen,
                           DailyAssessment.fraction_inspired_oxygen,
                           SariDailyAssessment.fraction_inspired_oxygen,
                           na.rm = TRUE),
           min_fio2 = pmin(AdmissionAssessment.fraction_inspired_oxygen,
                           SariAdmissionAssessment.fraction_inspired_oxygen,
                           DailyAssessment.fraction_inspired_oxygen,
                           SariDailyAssessment.fraction_inspired_oxygen, na.rm = TRUE),
           max_pao2 = pmax(AdmissionAssessment.partial_pressure_arterial_oxygen,
                           AdmissionAssessment.partial_pressure_oxygen,
                           SariAdmissionAssessment.partial_pressure_arterial_oxygen,
                           DailyAssessment.partial_pressure_arterial_oxygen,
                           DailyAssessment.partial_pressure_oxygen,
                           SariDailyAssessment.partial_pressure_arterial_oxygen,
                           na.rm = TRUE),
           min_pao2 = pmin(AdmissionAssessment.partial_pressure_arterial_oxygen,
                           AdmissionAssessment.partial_pressure_oxygen,
                           SariAdmissionAssessment.partial_pressure_arterial_oxygen,
                           DailyAssessment.partial_pressure_arterial_oxygen,
                           DailyAssessment.partial_pressure_oxygen,
                           SariDailyAssessment.partial_pressure_arterial_oxygen,
                           na.rm = TRUE),
           min_paco2 = pmin(AdmissionAssessment.partial_pressure_carbon_dioxide,
                            DailyAssessment.partial_pressure_carbon_dioxide,
                            SariDailyAssessment.partial_pressure_carbon_dioxide, na.rm = TRUE),
           max_paco2 = pmax(AdmissionAssessment.partial_pressure_carbon_dioxide,
                            DailyAssessment.partial_pressure_carbon_dioxide,
                            SariDailyAssessment.partial_pressure_carbon_dioxide, na.rm = TRUE),
           max_hematocrit = pmax(AdmissionAssessment.packed_cell_volume, na.rm = TRUE),
           min_hematocrit = pmin(AdmissionAssessment.packed_cell_volume, na.rm = TRUE),
           min_ph = pmin(AdmissionAssessment.arterial_ph,
                         SariDailyAssessment.arterial_ph,
                         na.rm = TRUE),
           max_ph = pmax(AdmissionAssessment.arterial_ph,
                         SariDailyAssessment.arterial_ph,
                         na.rm = TRUE),
           max_sodium = pmax(AdmissionAssessment.serum_sodium,
                             SariDailyAssessment.serum_sodium, na.rm = TRUE),
           min_sodium = pmin(AdmissionAssessment.serum_sodium,
                             SariDailyAssessment.serum_sodium, na.rm = TRUE),
           min_potassium = pmin(AdmissionAssessment.serum_potassium,
                                SariDailyAssessment.potassium_level1,
                                SariDailyAssessment.serum_potassium, na.rm = TRUE),
           max_potassium = pmax(AdmissionAssessment.serum_potassium,
                                SariDailyAssessment.potassium_level1,
                                SariDailyAssessment.serum_potassium, na.rm = TRUE),
           max_bicarbonate = pmax(AdmissionAssessment.bicarbonate,
                                  AdmissionAssessment.serum_bicarbonate,
                                  SariDailyAssessment.bicarbonate, na.rm = TRUE),
           min_bicarbonate = pmin(AdmissionAssessment.bicarbonate,
                                  AdmissionAssessment.serum_bicarbonate,
                                  SariDailyAssessment.bicarbonate, na.rm = TRUE),
           min_creatinine = pmin(AdmissionAssessment.serum_creatinine,
                                 SariAdmissionAssessment.serum_creatinine,
                                 DailyAssessment.serum_creatinine,
                                 SariDailyAssessment.serum_creatinine,
                                 na.rm = TRUE),
           max_creatinine = pmax(AdmissionAssessment.serum_creatinine,
                                 SariAdmissionAssessment.serum_creatinine,
                                 DailyAssessment.serum_creatinine,
                                 SariDailyAssessment.serum_creatinine,
                                 na.rm = TRUE),
           min_map = min_dbp + 1 / 3 * (min_sbp - min_dbp),
           max_map = max_dbp + 1 / 3 * (max_sbp - max_dbp),
           age = Admission.age)
  # GCS variables need to be translated to numbers
  data <- data %>%
    mutate(across(c(Admission.gcs_verbal,
                    DailyAssessment.gcs_verbal,
                    DailyAssessment.gcs_verbal2,
                    DailyAssessment.gcs_verbal3,
                    SariDailyAssessment.gcs_verbal,
                    AdmissionAssessment.gcs_verbal,
                    SariAdmissionAssessment.gcs_verbal,
                    DailyAssessment.lowest_gcs_verbal,
                    DailyAssessment.highest_gcs_verbal), ~
                    case_when(
                      grepl("none", .x, ignore.case = T) ~ 1L,
                      grepl("incomprehensible sounds", .x, ignore.case = T) ~ 2L,
                      grepl("inappropriate words", .x, ignore.case = T) ~ 3L,
                      grepl("confused", .x, ignore.case = T) ~ 4L,
                      grepl("oriented", .x, ignore.case = T) ~ 5L
                    )),
           across(c("Admission.gcs_motor",
                    "DailyAssessment.gcs_motor",
                    "DailyAssessment.gcs_motor2",
                    "DailyAssessment.gcs_motor3",
                    "SariDailyAssessment.gcs_motor",
                    "AdmissionAssessment.gcs_motor",
                    "SariAdmissionAssessment.gcs_motor",
                    "DailyAssessment.lowest_gcs_motor",
                    "DailyAssessment.highest_gcs_motor"), ~
                    case_when(
                      grepl("no motor response", .x, ignore.case = T) ~ 1L,
                      grepl("extension to pain", .x, ignore.case = T) ~ 2L,
                      grepl("flexion in response to pain", .x, ignore.case = T) ~ 3L,
                      grepl("withdraws from pain", .x, ignore.case = T) ~ 4L,
                      grepl("locailzes to pain", .x, ignore.case = T) ~ 5L,
                      grepl("obeys commands", .x, ignore.case = T) ~ 6L
                    )),
           across(c("Admission.gcs_eye",
                    "DailyAssessment.gcs_eye",
                    "DailyAssessment.gcs_eye2",
                    "DailyAssessment.gcs_eye3",
                    "SariDailyAssessment.gcs_eye",
                    "AdmissionAssessment.gcs_eye",
                    "SariAdmissionAssessment.gcs_eye",
                    "DailyAssessment.lowest_gcs_eye",
                    "DailyAssessment.highest_gcs_eye"), ~
                    case_when(
                      grepl("no eye opening", .x, ignore.case = T) ~ 1L,
                      grepl("eye opening in response to pain", .x, ignore.case = T) ~ 2L,
                      grepl("eye opening to speech", .x, ignore.case = T) ~ 3L,
                      grepl("eye opening spontaneously", .x, ignore.case = T) ~ 4L
                    )),
           gcs_a = Admission.gcs_verbal + Admission.gcs_motor + Admission.gcs_eye,
           gcs_d_1 = DailyAssessment.gcs_verbal + DailyAssessment.gcs_motor + DailyAssessment.gcs_eye,
           gcs_d_2 = DailyAssessment.gcs_verbal2 + DailyAssessment.gcs_motor2 + DailyAssessment.gcs_eye2,
           gcs_d_3 = DailyAssessment.gcs_verbal3 + DailyAssessment.gcs_motor3 + DailyAssessment.gcs_eye3,
           gcs_sd = SariDailyAssessment.gcs_verbal + SariDailyAssessment.gcs_motor + SariDailyAssessment.gcs_eye,
           gcs_aa = AdmissionAssessment.gcs_verbal + AdmissionAssessment.gcs_motor + AdmissionAssessment.gcs_eye,
           gcs_sa = SariAdmissionAssessment.gcs_verbal + SariAdmissionAssessment.gcs_motor + SariAdmissionAssessment.gcs_eye,
           gcs_da_l = DailyAssessment.lowest_gcs_verbal + DailyAssessment.lowest_gcs_motor + DailyAssessment.lowest_gcs_eye,
           gcs_da_h = DailyAssessment.highest_gcs_verbal + DailyAssessment.highest_gcs_motor + DailyAssessment.highest_gcs_eye,
           min_gcs = pmin(gcs_a, gcs_d_1, gcs_d_2, gcs_d_3, gcs_sd, gcs_aa, gcs_sa,
                          gcs_da_l, gcs_da_h, na.rm = TRUE),
           max_gcs = pmax(gcs_a, gcs_d_1, gcs_d_2, gcs_d_3, gcs_sd, gcs_aa, gcs_sa,
                          gcs_da_l, gcs_da_h, na.rm = TRUE))

  ### MAP, comorbidities, emergency admissions.
  data <- data %>%
    mutate(renal_failure =
             if_else((Admission.comorbid_conditions %in% acute_renal_failure |
                      Admission.comorbid_conditions2 %in% acute_renal_failure |
                      Admission.comorbid_conditions3 %in% acute_renal_failure |
                      Admission.comorbid_conditions4 %in% acute_renal_failure), 1, 0),
           comorbidity =
             if_else((Admission.comorbid_conditions %in% comorbid_list |
                        Admission.comorbid_conditions2 %in% comorbid_list |
                        Admission.comorbid_conditions3 %in% comorbid_list |
                        Admission.comorbid_conditions4 %in% comorbid_list), 1, 0),
           non_operative = if_else(
             (Admission.diagnosis_type %in% c("Non operative", "Planned others", "Planned-other",
                                              "Unplanned others", "Unplanned-other")) &
               (is.na(Admission.diagnosis_type2) |
                  Admission.diagnosis_type2 %in% c("Non operative", "Non operative2",
                                                   "Planned others", "Planned-other",
                                                   "Unplanned others", "Unplanned-other")),
             1L, 0L, 0L
           ),
           post_operative = if_else(
             Admission.diagnosis_type %in% c("Post operative", "Planned following surgery",
                                             "Unplanned following surgery") |
               Admission.diagnosis_type2 %in% c("Post operative", "Post operative2",
                                                "Planned following surgery",
                                                "Unplanned following surgery"), 1L, 0L, 0L
           ),
           emergency_surgery = if_else(
             post_operative == 1 & (Admission.emergency_surgery == "Yes" |
                                      Admission.diagnosis_type == "Unplanned following surgery" |
                                      Admission.diagnosis_type2 == "Unplanned following surgery" |
                                      Admission.admission_type == "Unplanned"), 1L, 0L, 0L
           ),
           # To use for APACHE II score if both diagnosis_type variables are empty but admission_type is filled in
           # As unplanned can either be unplanned after surgery or unplanned non operative, we capture this in the same category as AP2 assigns same score for both category,
           emer_surg_or_medical = if_else(is.na(Admission.diagnosis_type) &
                                            is.na(Admission.diagnosis_type2) &
                                            Admission.admission_type == "Unplanned", 1L, 0L, 0L),
           planned = if_else(
             (post_operative == 1 & emergency_surgery == 0) |
               # If both diagnosis_type variables are empty, but admission_type is filled
               (non_operative == 0 & post_operative == 0 &
                  Admission.admission_type == "Planned")   == 1, 1L, 0L, 0L),
           emergency_admission = case_when(non_operative == 1 |
                                           emergency_surgery == 1 | emer_surg_or_medical == 1 ~ 1,
                                         post_operative == 1 & emergency_surgery == 0 ~ 0)
           )
  data
}




#' Converts the units of measurements needed for apache ii and etropics calculation
#' Also renames unit of measure variables for use with the general apache functions.
#' @param admission is a merged dataset of admission data, implementation sheet and units of measures
#' @param daily is a merged dataset of daily data, implementation sheet and units of measures
#' @noRd
unit_conversion_source <- function(admission, daily) {
  if (!missing(admission) & !missing(daily)) {
    stop("Only one dataset should be given as input")
  } else if (!missing(admission)) {
    # Measurements needed for apache ii and etropics calculation
    columns_admission <- c(
      "AdmissionAssessment.temperature", "AdmissionAssessment.white_blood_cells",
      "AdmissionAssessment.partial_pressure_arterial_oxygen", "AdmissionAssessment.fraction_inspired_oxygen",
      "AdmissionAssessment.packed_cell_volume", "AdmissionAssessment.systolic_blood_pressure",
      "AdmissionAssessment.diastolic_blood_pressure", "AdmissionAssessment.heart_rate",
      "AdmissionAssessment.respiratory_rate", "AdmissionAssessment.arterial_ph",
      "AdmissionAssessment.serum_sodium", "AdmissionAssessment.serum_potassium",
      "AdmissionAssessment.serum_bicarbonate", "AdmissionAssessment.serum_creatinine",
      "AdmissionAssessment.hemoglobin", "AdmissionAssessment.blood_urea"
    )

    measures <- c(
      "temperature_measure", "white_blood_cell_count_measure",
      "pa_o2_measure", "fi_o2_measure", "packed_cell_volume_measure",
      "systolic_blood_pressure_measure", "diastolic_blood_pressure_measure",
      "heart_rate_measure", "respiratory_rate_measure", "p_h_measure",
      "serum_sodium_measure", "serum_potassium_measure", "serum_hco3_measure",
      "serum_creatinine_measure", "haemoglobin_measure",
      "blood_urea_measure"
    )

    unit_list <- list(
      c("°F", "°C"),
      c(
        "10^3/mm³", "cells/mm³", "lakhs/mm³", "K/µL", "10^9/L", "cells/μL",
        "cells/L", "10^3/mm^3", "10^3/µL"
      ),
      c("mmHg", "KPa"), c("/", "%", "l/min"), c("%", "/", "L/L"),
      c("mmHg"), c("mmHg"), c("b/min"), c("/min"), c("NA"),
      c("mmol/L", "mEq/L"), c("mmol/L", "mEq/L"), c("mmol/L", "mEq/L"),
      c("μmol/L", "mg/dl", "mmol/L", "mg/L"), c("g/dl", "g/L"),
      c("mmol/L", "mg/dl", "μmol/L", "mg/dL", "g/L")
    )

    admission <- admission %>%
      mutate(across(columns_admission, as.numeric))

    unique_unit_ids <- unique(admission$unitId)

    # If a measuremeant of an icu unit doesn't have a measuring unit in the units of measure sheet
    # or new unit has been added for that measurement, this will give the error with that icu unit and its measurement
    for (unit in unique_unit_ids) {
      adm_one_unit <- admission %>%
        filter(unitId == unit)
      for (i in seq_along(columns_admission)) {
        not_null_values <- sum(!is.na(adm_one_unit[, columns_admission[i]]))
        if (not_null_values > 0) {
          if (!measures[i] == "p_h_measure") {
            measure_unit <- unique(adm_one_unit[, measures[i]])
            if (!measure_unit %in% unit_list[[i]]) {
              stop(paste0(paste("For unit id", unit), paste(", measure unit doesn't exist or new measure unit has been added for", columns_admission[i])))
            }
          }
        }
      }
    }

    admission <- admission %>%
      mutate(
        across(c(max_temp, min_temp), ~ case_when(
          temperature_measure == "°C" ~ .x,
          temperature_measure == "°F" ~ (.x - 32) * 5 / 9
        )),
        unit_temp = "degree Celsius",
        across(c(max_wcc, min_wcc), ~ case_when(
          white_blood_cell_count_measure %in% c("cells/mm³", "cells/μL", "1000/mcgl") ~ .x / 1000,
          white_blood_cell_count_measure %in% c(
            "10^9/L", "10^3/mm³", "K/µL",
            "10^3/mm^3", "10^3/µL"
          ) ~ .x,
          white_blood_cell_count_measure == "lakhs/mm³" ~ .x * 100,
          white_blood_cell_count_measure == "cells/L" ~ .x * 0.000000001
        )),
        unit_wcc = "billion per liter",
        across(c(max_fio2, min_fio2), ~ case_when(
          fi_o2_measure == "%" ~ .x * 0.01,
          fi_o2_measure == "l/min" ~ (20 + 4 * .x) * 0.01,
          fi_o2_measure == "/" ~ .x
        )),
        unit_fio2 = "ratio",
        across(c(max_pao2, min_pao2), ~ case_when(
          pa_o2_measure == "mmHg" ~ .x,
          pa_o2_measure == "KPa" ~ .x * 7.50062
        )),
        unit_pao2 = "millimeter mercury column",
        across(c(max_hematocrit, min_hematocrit), ~ case_when(
          packed_cell_volume_measure == "%" ~ .x,
          packed_cell_volume_measure %in% c("/", "L/L") ~ .x * 100
        )),
        unit_hematocrit = "percent",
        across(c(max_creatinine, min_creatinine), ~ case_when(
          serum_creatinine_measure == "mg/dl" ~ .x,
          serum_creatinine_measure == "mg/L" ~ .x * 0.1,
          serum_creatinine_measure == "mmol/L" ~ .x * 11.312,
          serum_creatinine_measure == "μmol/L" ~ .x * 0.0113
        )),
        unit_creatinine = "milligram per deciliter",
        unit_paco2 = "millimeter mercury column",
        unit_sodium = "millimole per liter",
        unit_potassium = "millimole per liter",
        unit_bicarbonate = "millimole per liter"

      )

    output_data <- admission
  }

  output_data
}
