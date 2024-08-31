SELECT
    Id,
    Currency_source,
    Currency_target,
    Amount_original,
    Exchange_rate_value,
    Amount_target_converted_to_smallest_unit_of_count,
    Emitter,
    Document,
    Rubric,
    Subrubric,
    Date,
    CAST(SUM(Occurrences) AS UNSIGNED) AS Occurrences
FROM
	(
	SELECT
		er.exchange_rate_id AS Id,
		cs_source.currency_name AS Currency_source,
		cs_target.currency_name AS Currency_target,
		CONCAT_WS(
			' ',
		GROUP_CONCAT( DISTINCT CONCAT( as_subpart_target.arabic_numeral, IFNULL( CONCAT( ' ', uoc_target.unit_of_count_abbreviation ), '' )) ORDER BY uoc_target.unit_of_count_id ASC SEPARATOR ' ' )) AS Amount_original,
		er.exchange_rate_value AS Exchange_rate_value,
		as_target.amount_converted_to_smallest_unit_of_count AS Amount_target_converted_to_smallest_unit_of_count,
		
		emitter.person_name_standardized AS Emitter,
        doc.document_name AS Document,
        rubstand.rubric_name_standardized AS Rubric,
        subrubstand.subrubric_name_standardized AS Subrubric,
        
        dts.start_date_standardized AS date,
		COUNT(DISTINCT eri.line_id) AS Occurrences
	FROM
		exchange_rate er
		JOIN currency_standardized cs_source ON er.currency_source_id = cs_source.currency_standardized_id
		JOIN currency_standardized cs_target ON er.currency_target_id = cs_target.currency_standardized_id
		LEFT JOIN amount_simple as_target ON er.amount_simple_target_id = as_target.amount_simple_id
		LEFT JOIN amount_simple_subpart as_subpart_target ON as_target.amount_simple_id = as_subpart_target.amount_simple_id
		LEFT JOIN unit_of_count uoc_target ON as_subpart_target.unit_of_count_id = uoc_target.unit_of_count_id
		JOIN exchange_rate_internal_reference eri ON er.exchange_rate_id = eri.exchange_rate_id
		LEFT JOIN line l ON eri.line_id = l.line_id
		LEFT JOIN document doc ON l.document_id = doc.document_id
		
		JOIN emission emission ON doc.document_id = emission.document_id
   		JOIN person emitter ON emission.person_id = emitter.person_id
    	JOIN rubric_extracted rubext ON l.rubric_extracted_id = rubext.rubric_extracted_id
    	JOIN rubric_standardized rubstand ON rubext.rubric_standardized_id = rubstand.rubric_standardized_id
    	LEFT JOIN subrubric_extracted subrubext ON l.subrubric_extracted_id = subrubext.subrubric_extracted_id
    	LEFT JOIN subrubric_standardized subrubstand ON subrubext.subrubric_standardized_id = subrubstand.subrubric_standardized_id
		
		LEFT JOIN date dts ON l.date_id = dts.date_id		
	GROUP BY
		er.exchange_rate_id,
		emitter.person_name_standardized,
        doc.document_name,
        rubstand.rubric_name_standardized,
        subrubstand.subrubric_name_standardized,
		dts.start_date_standardized 
	UNION ALL
	SELECT
		er.exchange_rate_id AS Id,
		cs_source.currency_name AS Currency_source,
		cs_target.currency_name AS Currency_target,
		CONCAT_WS(
			' ',
		GROUP_CONCAT( DISTINCT CONCAT( as_subpart_target.arabic_numeral, IFNULL( CONCAT( ' ', uoc_target.unit_of_count_abbreviation ), '' )) ORDER BY uoc_target.unit_of_count_id ASC SEPARATOR ' ' )) AS Amount_original,
		er.exchange_rate_value,
		as_target.amount_converted_to_smallest_unit_of_count AS Amount_target_converted_to_smallest_unit_of_count,
		CASE 
        	WHEN external_dates.start_date_standardized > '1300-05-12' AND external_dates.start_date_standardized < '1317-02-05' THEN 'Jean XXII'
        	WHEN external_dates.start_date_standardized > '1317-04-10' AND external_dates.start_date_standardized < '1323-01-10' THEN 'Benoit XII'
        	ELSE NULL
    	END AS Emitter,
		NULL AS Document,
		NULL AS Rubric,
		NULL AS Subrubric,
		external_dates.start_date_standardized AS date,
		COUNT(DISTINCT erd.exchange_rate_date_id) AS Occurrences
	FROM
		exchange_rate er
		JOIN currency_standardized cs_source ON er.currency_source_id = cs_source.currency_standardized_id
		JOIN currency_standardized cs_target ON er.currency_target_id = cs_target.currency_standardized_id
		LEFT JOIN amount_simple as_target ON er.amount_simple_target_id = as_target.amount_simple_id
		LEFT JOIN amount_simple_subpart as_subpart_target ON as_target.amount_simple_id = as_subpart_target.amount_simple_id
		LEFT JOIN unit_of_count uoc_target ON as_subpart_target.unit_of_count_id = uoc_target.unit_of_count_id
		JOIN exchange_rate_date erd ON er.exchange_rate_id = erd.exchange_rate_id
		JOIN date external_dates ON erd.date_id = external_dates.date_id 
	GROUP BY
		er.exchange_rate_id,
		external_dates.start_date_standardized 
	) AS combined_results
GROUP BY
    Id,
    Currency_source,
    Currency_target,
    Amount_original,
    Exchange_rate_value,
    Amount_target_converted_to_smallest_unit_of_count,
    Emitter,
    Document,
    Rubric,
    Subrubric,
    Date
ORDER BY
	Id ASC