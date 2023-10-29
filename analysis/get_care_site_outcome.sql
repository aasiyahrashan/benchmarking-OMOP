	select
	p.person_id
	, p.person_source_value -- for debugging only. Not required for the main analysis.
	, vo.visit_occurrence_id
	, vd.visit_detail_id
	, COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date,
	            vo.visit_start_datetime, vo.visit_start_date) as icu_admission_datetime
	, COALESCE(vd.visit_detail_end_datetime, vd.visit_detail_start_date,
	            vo.visit_end_datetime, vo.visit_start_date) as icu_discharge_datetime
	, cs.care_site_id
	, cs.care_site_name
	, d.death_datetime
	from {schema}.person p
	inner join {schema}.visit_occurrence vo
	on p.person_id = vo.person_id
	-- this should contain ICU stay information, if it exists at all
	left join {schema}.visit_detail vd
	on p.person_id = vd.person_id
	and (vo.visit_occurrence_id = vd.visit_occurrence_id or vd.visit_occurrence_id is null)
	--- Getting care site information
	inner join {schema}.care_site cs
	on p.care_site_id = cs.care_site_id
	--- Getting death information.
	left join {schema}.death d
	on p.person_id = d.person_id
	where COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date,
	                vo.visit_start_datetime, vo.visit_start_date) >= '{start_date}'
	and COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date,
	              vo.visit_start_datetime, vo.visit_start_date) < '{end_date}'
