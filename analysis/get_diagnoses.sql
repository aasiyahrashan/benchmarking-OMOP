WITH icu_admission_details AS (

	--- We generally want ICU admission information.
	--- But CCHIC doesn't have it, so we use hospital info.
	SELECT p.person_id
		,vo.visit_occurrence_id
		,vd.visit_detail_id
		,COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date, vo.visit_start_datetime, vo.visit_start_date) AS icu_admission_datetime
	FROM @dbname.@schema.person p
	INNER JOIN @dbname.@schema.visit_occurrence vo ON p.person_id = vo.person_id
	-- this should contain ICU stay information, if it exists at all
	LEFT JOIN @dbname.@schema.visit_detail vd ON p.person_id = vd.person_id AND vo.visit_occurrence_id = vd.visit_occurrence_id
	WHERE COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date, vo.visit_start_datetime, vo.visit_start_date) >= @start_date
	  AND COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date, vo.visit_start_datetime, vo.visit_start_date) <= @end_date

	),

	---- Getting both operative and non-operative diagnoses.
	--- Condition data
condition_data AS (
	SELECT c.person_id
		,c.condition_concept_id AS diagnosis_concept_id
		,COALESCE(c.condition_start_datetime, c.condition_start_date) AS diagnosis_start_datetime
		,c.visit_occurrence_id
		,c.visit_detail_id
		,c.condition_source_value AS diagnosis_name
		,concept.vocabulary_id
		,concept.concept_code
	FROM icu_admission_details adm
	INNER JOIN @dbname.@schema.condition_occurrence c
		-- making sure the visits match up, and filtering by number of days in ICU
		ON adm.person_id = c.person_id
		AND adm.visit_occurrence_id = c.visit_occurrence_id
		AND (adm.visit_detail_id = c.visit_detail_id OR adm.visit_detail_id IS NULL)
		--- Only want data recorded on the day of admission
		AND DATEDIFF(dd, adm.icu_admission_datetime, COALESCE(c.condition_start_datetime, c.condition_start_date)) = 0
	--- getting source IDs to use when getting diagnosis coefficents.
	INNER JOIN @dbname.@schema.concept ON concept.concept_id = c.condition_concept_id

	),

	----- Procedure data
procedure_data
AS (
	SELECT p.person_id
		,p.procedure_concept_id AS diagnosis_concept_id
		,COALESCE(p.procedure_datetime, p.procedure_date) AS diagnosis_start_datetime
		,p.visit_occurrence_id
		,p.visit_detail_id
		,p.procedure_source_value AS diagnosis_name
		,concept.vocabulary_id
		,concept.concept_code
	FROM icu_admission_details adm
	INNER JOIN @dbname.@schema.procedure_occurrence p
		-- making sure the visits match up, and filtering by number of days in ICU
		ON adm.person_id = p.person_id
		AND adm.visit_occurrence_id = p.visit_occurrence_id
		AND (adm.visit_detail_id = p.visit_detail_id OR adm.visit_detail_id IS NULL)
		--- Only want data recorded on the day of admission
		AND DATEDIFF(dd, adm.icu_admission_datetime, COALESCE(p.procedure_datetime, p.procedure_date)) = 0
	--- getting source IDs to use when getting diagnosis coefficents.
	INNER JOIN @dbname.@schema.concept ON concept.concept_id = p.procedure_concept_id
	)
SELECT *
FROM condition_data

UNION

SELECT *
FROM procedure_data
