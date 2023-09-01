with icu_admission_details as (
	--- We generally want ICU admission information.
	--- But CCHIC doesn't have it, so we use hospital info.
	select
	p.person_id
	, vo.visit_occurrence_id
	, vd.visit_detail_id
	, COALESCE(vd.visit_detail_start_datetime, vo.visit_start_datetime) as icu_admission_datetime
	from {schema}.person p
	inner join {schema}.visit_occurrence vo
	on p.person_id = vo.person_id
	-- this should contain ICU stay information, if it exists at all
	left join {schema}.visit_detail vd
	on p.person_id = vd.person_id
	where COALESCE(vd.visit_detail_start_datetime, vo.visit_start_datetime) >= '{start_date}'
	and COALESCE(vd.visit_detail_start_datetime, vo.visit_start_datetime) < '{end_date}'

),
---- Getting both operative and non-operative diagnoses.
--- Condition data
condition_data as (
select
c.person_id,
c.condition_concept_id as diagnosis_concept_id,
c.condition_start_datetime as diagnosis_start_datetime,
c.visit_occurrence_id,
c.visit_detail_id,
c.condition_source_value as diagnosis_name,
concept.vocabulary_id,
concept.concept_code
from icu_admission_details adm
inner join {schema}.condition_occurrence c
-- making sure the visits match up, and filtering by number of days in ICU
on adm.person_id = c.person_id
and adm.visit_occurrence_id = c.visit_occurrence_id
and (adm.visit_detail_id = c.visit_detail_id or adm.visit_detail_id is null)
--- Only want data recorded on the day of admission
and DATE_PART('day', c.condition_start_datetime - adm.icu_admission_datetime) = 0
--- getting source IDs to use when getting diagnosis coefficents.
inner join {schema}.concept
on concept.concept_id = c.condition_concept_id),

----- Procedure data
procedure_data as (
select
p.person_id,
p.procedure_concept_id as diagnosis_concept_id,
p.procedure_datetime as diagnosis_start_datetime,
p.visit_occurrence_id,
p.visit_detail_id,
p.procedure_source_value as diagnosis_name,
concept.vocabulary_id,
concept.concept_code
from icu_admission_details adm
inner join {schema}.procedure_occurrence p
-- making sure the visits match up, and filtering by number of days in ICU
on adm.person_id = p.person_id
and adm.visit_occurrence_id = p.visit_occurrence_id
and (adm.visit_detail_id = p.visit_detail_id or adm.visit_detail_id is null)
--- Only want data recorded on the day of admission
and DATE_PART('day', p.procedure_datetime - adm.icu_admission_datetime) = 0
--- getting source IDs to use when getting diagnosis coefficents.
inner join {schema}.concept
on concept.concept_id = p.procedure_concept_id)

select * from condition_data
union
select * from procedure_data



