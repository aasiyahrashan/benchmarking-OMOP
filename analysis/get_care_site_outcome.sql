SELECT p.person_id
     ,p.person_source_value -- for debugging only. Not required for the main analysis.
	   ,vo.visit_occurrence_id
	   ,vd.visit_detail_id
       ,COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date, vo.visit_start_datetime, vo.visit_start_date) AS icu_admission_datetime
       ,COALESCE(vd.visit_detail_end_datetime,   vd.visit_detail_start_date, vo.visit_end_datetime,   vo.visit_start_date) AS icu_discharge_datetime
	   ,cs.care_site_id
	   ,cs.care_site_name
	   ,d.death_datetime
FROM @schema.person p
INNER JOIN @schema.visit_occurrence vo
	ON p.person_id = vo.person_id
-- this should contain ICU stay information, if it exists at all
LEFT JOIN @schema.visit_detail vd
	ON p.person_id = vd.person_id
--- Getting care site information
INNER JOIN @schema.care_site cs
	ON p.care_site_id = cs.care_site_id
--- Getting death information.
LEFT JOIN @schema.death d
	ON p.person_id = d.person_id
WHERE COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date, vo.visit_start_datetime, vo.visit_start_date) >= '@start_date'
  AND COALESCE(vd.visit_detail_start_datetime, vd.visit_detail_start_date, vo.visit_start_datetime, vo.visit_start_date) <= '@end_date'
