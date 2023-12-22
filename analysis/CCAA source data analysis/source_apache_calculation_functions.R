#' Converts the units of measurements needed for apache ii and etropics calculation
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
        AdmissionAssessment.temperature = case_when(
          temperature_measure == "°C" ~ AdmissionAssessment.temperature,
          temperature_measure == "°F" ~ (AdmissionAssessment.temperature - 32) * 5 / 9
        ),
        AdmissionAssessment.white_blood_cells = case_when(
          white_blood_cell_count_measure %in% c("cells/mm³", "cells/μL", "1000/mcgl") ~ AdmissionAssessment.white_blood_cells / 1000,
          white_blood_cell_count_measure %in% c(
            "10^9/L", "10^3/mm³", "K/µL",
            "10^3/mm^3", "10^3/µL"
          ) ~ AdmissionAssessment.white_blood_cells,
          white_blood_cell_count_measure == "lakhs/mm³" ~ AdmissionAssessment.white_blood_cells * 100,
          white_blood_cell_count_measure == "cells/L" ~ AdmissionAssessment.white_blood_cells * 0.000000001
        ),
        AdmissionAssessment.fraction_inspired_oxygen = case_when(
          fi_o2_measure == "%" ~ AdmissionAssessment.fraction_inspired_oxygen * 0.01,
          fi_o2_measure == "l/min" ~ (20 + 4 * AdmissionAssessment.fraction_inspired_oxygen) * 0.01,
          fi_o2_measure == "/" ~ AdmissionAssessment.fraction_inspired_oxygen
        ),
        AdmissionAssessment.partial_pressure_arterial_oxygen = case_when(
          pa_o2_measure == "mmHg" ~ AdmissionAssessment.partial_pressure_arterial_oxygen,
          pa_o2_measure == "KPa" ~ AdmissionAssessment.partial_pressure_arterial_oxygen * 7.50062
        ),
        AdmissionAssessment.packed_cell_volume = case_when(
          packed_cell_volume_measure == "%" ~ AdmissionAssessment.packed_cell_volume,
          packed_cell_volume_measure %in% c("/", "L/L") ~ AdmissionAssessment.packed_cell_volume * 100
        ),
        AdmissionAssessment.serum_creatinine = case_when(
          serum_creatinine_measure == "mg/dl" ~ AdmissionAssessment.serum_creatinine,
          serum_creatinine_measure == "mg/L" ~ AdmissionAssessment.serum_creatinine * 0.1,
          serum_creatinine_measure == "mmol/L" ~ AdmissionAssessment.serum_creatinine * 11.312,
          serum_creatinine_measure == "μmol/L" ~ AdmissionAssessment.serum_creatinine * 0.0113
        ),
        AdmissionAssessment.hemoglobin = case_when(
          haemoglobin_measure == "g/dl" ~ AdmissionAssessment.hemoglobin,
          haemoglobin_measure == "g/L" ~ AdmissionAssessment.hemoglobin / 10
        ),
        AdmissionAssessment.blood_urea = case_when(
          blood_urea_measure == "mmol/L" ~ AdmissionAssessment.blood_urea * 6.006,
          blood_urea_measure == "μmol/L" ~ AdmissionAssessment.blood_urea * 0.0060,
          blood_urea_measure == "mg/dl" ~ AdmissionAssessment.blood_urea,
          blood_urea_measure == "mg/dL" ~ AdmissionAssessment.blood_urea,
          blood_urea_measure == "g/L" ~ AdmissionAssessment.blood_urea * 100
        )
      )

    output_data <- admission
  } else if (!missing(daily)) {
    columns_admission_daily <- c(
      "DailyAssessment.temperature", "DailyAssessment.lowest_wcc",
      "DailyAssessment.highest_wcc"
    )

    daily <- daily %>%
      mutate(across(columns_admission_daily, as.numeric))

    daily <- daily %>%
      mutate(
        DailyAssessment.temperature = if_else(
          temperature_measure == "°C", DailyAssessment.temperature,
          (DailyAssessment.temperature - 32) * (5 / 9)
        ),
        # WARNING - need conversion for K/µL.
        DailyAssessment.lowest_wcc = case_when(
          white_blood_cell_count_measure %in% c("cells/mm³", "cells/μL", "1000/mcgl") ~ DailyAssessment.lowest_wcc,
          white_blood_cell_count_measure %in% c(
            "10^9/L", "10^3/mm³", "K/µL", "10^3/mm^3",
            "10^3/µL"
          ) ~ DailyAssessment.lowest_wcc * 1000,
          white_blood_cell_count_measure == "lakhs/mm³" ~ DailyAssessment.lowest_wcc * 100000,
          white_blood_cell_count_measure == "cells/L" ~ DailyAssessment.lowest_wcc * 0.000001
        ),
        DailyAssessment.highest_wcc = case_when(
          white_blood_cell_count_measure %in% c("cells/mm³", "cells/μL", "1000/mcgl") ~ DailyAssessment.highest_wcc,
          white_blood_cell_count_measure %in% c(
            "10^9/L", "10^3/mm³", "K/µL", "10^3/mm^3",
            "10^3/µL"
          ) ~ DailyAssessment.highest_wcc * 1000,
          white_blood_cell_count_measure == "lakhs/mm³" ~ DailyAssessment.highest_wcc * 100000,
          white_blood_cell_count_measure == "cells/L" ~ DailyAssessment.highest_wcc * 0.000001
        ),
        DailyAssessment.fraction_inspired_oxygen = case_when(
          fi_o2_measure == "%" ~ DailyAssessment.fraction_inspired_oxygen * 0.01,
          fi_o2_measure == "l/min" ~ (20 + 4 * DailyAssessment.fraction_inspired_oxygen) * 0.01,
          fi_o2_measure == "/" ~ DailyAssessment.fraction_inspired_oxygen
        )
      )

    output_data <- daily
  }

  output_data
}


#' Calculates APACHE II score
#' @param admission_data Needs to have data with unit conversions
#' @import dplyr
#' @noRd
calculate_apache_ii_score_source <- function(admission_data) {
  acute_renal_failure <- c("Renal failure, Mild", "Renal failure, Moderate to severe", "CKD requiring dialysis")
  comorbid_list <- c(
    "AIDS", "Hepatic disease, Moderate to severe", "Renal failure, Moderate to severe",
    "Respiratory disease, Severe moderate", "Leukemia", "Lymphoma", "Metastatic cancer",
    "CKD requiring dialysis", "Cirrhosis", "GI bleeding", "Tumor", "Cerebrovascular disease"
  )

  apache_ii <- admission_data %>%
    mutate(
      temp_aps = case_when(
        AdmissionAssessment.temperature >= 41 | AdmissionAssessment.temperature < 30 ~ 4,
        (AdmissionAssessment.temperature >= 39 & AdmissionAssessment.temperature < 41) | (AdmissionAssessment.temperature >= 30 & AdmissionAssessment.temperature < 32) ~ 3,
        AdmissionAssessment.temperature >= 32 & AdmissionAssessment.temperature < 34 ~ 2,
        (AdmissionAssessment.temperature >= 38.5 & AdmissionAssessment.temperature < 39) | (AdmissionAssessment.temperature >= 34 & AdmissionAssessment.temperature < 36) ~ 1,
        AdmissionAssessment.temperature >= 36 & AdmissionAssessment.temperature < 38.5 ~ 0,
        TRUE ~ 0
      ),
      wbc_aps = case_when(
        AdmissionAssessment.white_blood_cells >= 40 | AdmissionAssessment.white_blood_cells < 1 ~ 4,
        (AdmissionAssessment.white_blood_cells >= 20 & AdmissionAssessment.white_blood_cells < 40) | (AdmissionAssessment.white_blood_cells >= 1 & AdmissionAssessment.white_blood_cells < 3) ~ 2,
        AdmissionAssessment.white_blood_cells >= 15 & AdmissionAssessment.white_blood_cells < 20 ~ 1,
        AdmissionAssessment.white_blood_cells >= 3 & AdmissionAssessment.white_blood_cells < 15 ~ 0,
        TRUE ~ 0
      ),
      MAP = (2 * AdmissionAssessment.diastolic_blood_pressure + AdmissionAssessment.systolic_blood_pressure) / 3,
      map_aps = case_when(
        MAP >= 160 | MAP < 50 ~ 4,
        MAP >= 130 & MAP < 160 ~ 3,
        (MAP >= 110 & MAP < 130) | (MAP >= 50 & MAP < 70) ~ 2,
        MAP >= 70 & MAP < 110 ~ 0,
        TRUE ~ 0
      ),
      # For AaDO2 calculation, PaCO2 is imputed as 40 mmHg
      AaDO2 = (AdmissionAssessment.fraction_inspired_oxygen * 710) - (40 * 1.25) - AdmissionAssessment.partial_pressure_arterial_oxygen,
      AaDO2_aps = case_when(
        AdmissionAssessment.fraction_inspired_oxygen >= 0.5 & AaDO2 >= 500 ~ 4,
        AdmissionAssessment.fraction_inspired_oxygen >= 0.5 & AaDO2 >= 350 & AaDO2 < 500 ~ 3,
        AdmissionAssessment.fraction_inspired_oxygen >= 0.5 & AaDO2 >= 200 & AaDO2 < 350 ~ 2,
        AdmissionAssessment.fraction_inspired_oxygen >= 0.5 & AaDO2 < 200 ~ 0,
        AdmissionAssessment.fraction_inspired_oxygen < 0.5 & AdmissionAssessment.partial_pressure_arterial_oxygen > 70 ~ 0,
        AdmissionAssessment.fraction_inspired_oxygen < 0.5 & AdmissionAssessment.partial_pressure_arterial_oxygen > 60 & AdmissionAssessment.partial_pressure_arterial_oxygen <= 70 ~ 1,
        AdmissionAssessment.fraction_inspired_oxygen < 0.5 & AdmissionAssessment.partial_pressure_arterial_oxygen >= 55 & AdmissionAssessment.partial_pressure_arterial_oxygen <= 60 ~ 3,
        AdmissionAssessment.fraction_inspired_oxygen < 0.5 & AdmissionAssessment.partial_pressure_arterial_oxygen < 55 ~ 4,
        TRUE ~ 0
      ),
      hmcrt_aps = case_when(
        AdmissionAssessment.packed_cell_volume >= 60 | (AdmissionAssessment.packed_cell_volume > 0 & AdmissionAssessment.packed_cell_volume < 20) ~ 4,
        (AdmissionAssessment.packed_cell_volume >= 50 & AdmissionAssessment.packed_cell_volume < 60) | (AdmissionAssessment.packed_cell_volume >= 20 & AdmissionAssessment.packed_cell_volume < 30) ~ 2,
        AdmissionAssessment.packed_cell_volume >= 46 & AdmissionAssessment.packed_cell_volume < 50 ~ 1,
        AdmissionAssessment.packed_cell_volume >= 30 & AdmissionAssessment.packed_cell_volume < 46 ~ 0,
        TRUE ~ 0
      ),
      hr_aps = case_when(
        AdmissionAssessment.heart_rate >= 180 | AdmissionAssessment.heart_rate < 40 ~ 4,
        (AdmissionAssessment.heart_rate >= 140 & AdmissionAssessment.heart_rate < 180) | (AdmissionAssessment.heart_rate >= 40 & AdmissionAssessment.heart_rate < 55) ~ 3,
        (AdmissionAssessment.heart_rate >= 110 & AdmissionAssessment.heart_rate < 140) | (AdmissionAssessment.heart_rate >= 55 & AdmissionAssessment.heart_rate < 70) ~ 2,
        AdmissionAssessment.heart_rate >= 70 & AdmissionAssessment.heart_rate < 110 ~ 0,
        TRUE ~ 0
      ),
      res_aps = case_when(
        AdmissionAssessment.respiratory_rate >= 50 | AdmissionAssessment.respiratory_rate < 6 ~ 4,
        AdmissionAssessment.respiratory_rate >= 35 & AdmissionAssessment.respiratory_rate < 50 ~ 3,
        AdmissionAssessment.respiratory_rate >= 6 & AdmissionAssessment.respiratory_rate < 10 ~ 2,
        (AdmissionAssessment.respiratory_rate >= 25 & AdmissionAssessment.respiratory_rate < 35) | (AdmissionAssessment.respiratory_rate >= 10 & AdmissionAssessment.respiratory_rate < 12) ~ 1,
        AdmissionAssessment.respiratory_rate >= 12 & AdmissionAssessment.respiratory_rate < 25 ~ 0,
        TRUE ~ 0
      ),
      artph_aps = case_when(
        AdmissionAssessment.arterial_ph >= 7.7 | AdmissionAssessment.arterial_ph < 7.15 ~ 4,
        (AdmissionAssessment.arterial_ph >= 7.6 & AdmissionAssessment.arterial_ph < 7.7) | (AdmissionAssessment.arterial_ph >= 7.15 & AdmissionAssessment.arterial_ph < 7.25) ~ 3,
        AdmissionAssessment.arterial_ph >= 7.25 & AdmissionAssessment.arterial_ph < 7.33 ~ 2,
        AdmissionAssessment.arterial_ph >= 7.5 & AdmissionAssessment.arterial_ph < 7.6 ~ 1,
        AdmissionAssessment.arterial_ph >= 7.33 & AdmissionAssessment.arterial_ph < 7.5 ~ 0,
        TRUE ~ 0
      ),
      sod_aps = case_when(
        AdmissionAssessment.serum_sodium >= 180 | AdmissionAssessment.serum_sodium < 111 ~ 4,
        (AdmissionAssessment.serum_sodium >= 160 & AdmissionAssessment.serum_sodium < 180) | (AdmissionAssessment.serum_sodium >= 111 & AdmissionAssessment.serum_sodium < 120) ~ 3,
        (AdmissionAssessment.serum_sodium >= 155 & AdmissionAssessment.serum_sodium < 160) | (AdmissionAssessment.serum_sodium >= 120 & AdmissionAssessment.serum_sodium < 130) ~ 2,
        AdmissionAssessment.serum_sodium >= 150 & AdmissionAssessment.serum_sodium < 155 ~ 1,
        AdmissionAssessment.serum_sodium >= 130 & AdmissionAssessment.serum_sodium < 150 ~ 0,
        TRUE ~ 0
      ),
      pot_aps = case_when(
        AdmissionAssessment.serum_potassium >= 7 | AdmissionAssessment.serum_potassium < 2.5 ~ 4,
        AdmissionAssessment.serum_potassium >= 6 & AdmissionAssessment.serum_potassium < 7 ~ 3,
        AdmissionAssessment.serum_potassium >= 2.5 & AdmissionAssessment.serum_potassium < 3 ~ 2,
        (AdmissionAssessment.serum_potassium >= 5.5 & AdmissionAssessment.serum_potassium < 6) | (AdmissionAssessment.serum_potassium >= 3 & AdmissionAssessment.serum_potassium < 3.5) ~ 1,
        AdmissionAssessment.serum_potassium >= 3.5 & AdmissionAssessment.serum_potassium < 5.5 ~ 0,
        TRUE ~ 0
      ),
      aph = if_else(!is.na(AdmissionAssessment.arterial_ph), 1, 0, 0),
      hco3_aps = case_when(
        (aph == 0 & AdmissionAssessment.serum_bicarbonate >= 52) | (aph == 0 & AdmissionAssessment.serum_bicarbonate < 15) ~ 4,
        aph == 0 & AdmissionAssessment.serum_bicarbonate >= 41 & AdmissionAssessment.serum_bicarbonate < 52 ~ 3,
        aph == 0 & AdmissionAssessment.serum_bicarbonate >= 15 & AdmissionAssessment.serum_bicarbonate < 18 ~ 3,
        aph == 0 & AdmissionAssessment.serum_bicarbonate >= 18 & AdmissionAssessment.serum_bicarbonate < 22 ~ 2,
        aph == 0 & AdmissionAssessment.serum_bicarbonate >= 32 & AdmissionAssessment.serum_bicarbonate < 41 ~ 1,
        aph == 0 & AdmissionAssessment.serum_bicarbonate >= 22 & AdmissionAssessment.serum_bicarbonate < 32 ~ 0,
        TRUE ~ 0
      ),
      gcs_eye = case_when(
        grepl("no eye opening", AdmissionAssessment.gcs_eye, ignore.case = T) ~ 1L,
        grepl("eye opening in response to pain", AdmissionAssessment.gcs_eye, ignore.case = T) ~ 2L,
        grepl("eye opening to speech", AdmissionAssessment.gcs_eye, ignore.case = T) ~ 3L,
        grepl("eye opening spontaneously", AdmissionAssessment.gcs_eye, ignore.case = T) ~ 4L
      ),
      gcs_verbal = case_when(
        grepl("none", AdmissionAssessment.gcs_verbal, ignore.case = T) ~ 1L,
        grepl("incomprehensible sounds", AdmissionAssessment.gcs_verbal, ignore.case = T) ~ 2L,
        grepl("inappropriate words", AdmissionAssessment.gcs_verbal, ignore.case = T) ~ 3L,
        grepl("confused", AdmissionAssessment.gcs_verbal, ignore.case = T) ~ 4L,
        grepl("oriented", AdmissionAssessment.gcs_verbal, ignore.case = T) ~ 5L
      ),
      gcs_motor = case_when(
        grepl("no motor response", AdmissionAssessment.gcs_motor, ignore.case = T) ~ 1L,
        grepl("extension to pain", AdmissionAssessment.gcs_motor, ignore.case = T) ~ 2L,
        grepl("flexion in response to pain", AdmissionAssessment.gcs_motor, ignore.case = T) ~ 3L,
        grepl("withdraws from pain", AdmissionAssessment.gcs_motor, ignore.case = T) ~ 4L,
        grepl("locailzes to pain", AdmissionAssessment.gcs_motor, ignore.case = T) ~ 5L,
        grepl("obeys commands", AdmissionAssessment.gcs_motor, ignore.case = T) ~ 6L
      ),
      gcs = gcs_eye + gcs_verbal + gcs_motor,
      gcs_aps = if_else(!is.na(gcs), 15 - gcs, 0),
      com_creat = if_else((Admission.comorbid_conditions %in% acute_renal_failure |
        Admission.comorbid_conditions2 %in% acute_renal_failure |
        Admission.comorbid_conditions3 %in% acute_renal_failure |
        Admission.comorbid_conditions4 %in% acute_renal_failure), 1, 0),
      creat_aps = case_when(
        AdmissionAssessment.serum_creatinine >= 3.5 ~ 4,
        AdmissionAssessment.serum_creatinine >= 2 & AdmissionAssessment.serum_creatinine < 3.5 ~ 3,
        AdmissionAssessment.serum_creatinine >= 1.5 & AdmissionAssessment.serum_creatinine < 2 ~ 2,
        AdmissionAssessment.serum_creatinine < 0.6 ~ 2,
        AdmissionAssessment.serum_creatinine >= 0.6 & AdmissionAssessment.serum_creatinine < 1.5 ~ 0,
        TRUE ~ 0
      ),
      creat_aps = if_else(com_creat == 1, creat_aps * 2, creat_aps),
      age_aps = case_when(
        Admission.age >= 75 ~ 6,
        Admission.age >= 65 & Admission.age < 75 ~ 5,
        Admission.age >= 55 & Admission.age < 65 ~ 3,
        Admission.age >= 45 & Admission.age < 55 ~ 2,
        Admission.age < 45 ~ 0,
        TRUE ~ 0
      ),
      com_chro = if_else((Admission.comorbid_conditions %in% comorbid_list |
        Admission.comorbid_conditions2 %in% comorbid_list |
        Admission.comorbid_conditions3 %in% comorbid_list |
        Admission.comorbid_conditions4 %in% comorbid_list), 1, 0),
      cond_chro = if_else(com_chro == 1, 1, 0),
      chro_aps = case_when(
        cond_chro == 1 & Admission.diagnosis_type %in% c("non_operative", "Non operative") ~ 5,
        cond_chro == 1 & Admission.emergency_surgery == "Yes" & Admission.diagnosis_type %in% c("post_operative", "Post operative") ~ 5,
        cond_chro == 1 & Admission.emergency_surgery == "No" & Admission.diagnosis_type %in% c("post_operative", "Post operative") ~ 2,
        TRUE ~ 0
      ),
      apache_ii_score = temp_aps + wbc_aps + map_aps + AaDO2_aps + hmcrt_aps +
        hr_aps + res_aps + artph_aps + sod_aps + pot_aps + hco3_aps +
        gcs_aps + creat_aps + age_aps + chro_aps
    ) %>%
    select(patient_id, apache_ii_score)

  admission_data <- left_join(admission_data, apache_ii, by = "patient_id")

  admission_data
}
