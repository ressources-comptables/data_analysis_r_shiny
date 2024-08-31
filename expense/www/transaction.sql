SELECT
	Emitter,
	Document,
	Rubric,
	Subrubric,
	Line_id,
	Date,
	Amount_id
FROM
	(
	SELECT
		emitter.person_name_standardized AS Emitter,
		doc.document_name AS Document,
		rubstand.rubric_name_standardized AS Rubric,
		subrubstand.subrubric_name_standardized AS Subrubric,
		l.line_id AS Line_id,
		date.start_date_standardized AS Date,
		ac.amount_composite_id AS Amount_id
	FROM
		line l
		JOIN document doc ON l.document_id = doc.document_id
		JOIN emission emission ON doc.document_id = emission.document_id
		JOIN person emitter ON emission.person_id = emitter.person_id
		JOIN rubric_extracted rubext ON l.rubric_extracted_id = rubext.rubric_extracted_id
		JOIN rubric_standardized rubstand ON rubext.rubric_standardized_id = rubstand.rubric_standardized_id
		LEFT JOIN subrubric_extracted subrubext ON l.subrubric_extracted_id = subrubext.subrubric_extracted_id
		LEFT JOIN subrubric_standardized subrubstand ON subrubext.subrubric_standardized_id = subrubstand.subrubric_standardized_id
		JOIN date ON l.date_id = date.date_id
		JOIN amount_composite ac ON l.line_id = ac.line_id
	WHERE
		l.line_type_id = 2 
	GROUP BY
		emitter.person_name_standardized,
		doc.document_name,
		rubstand.rubric_name_standardized,
		subrubstand.subrubric_name_standardized,
		l.line_id,
		date.start_date_standardized,
		ac.amount_composite_id
	UNION ALL
		SELECT
		emitter.person_name_standardized AS Emitter,
		doc.document_name AS Document,
		rubstand.rubric_name_standardized AS Rubric,
		subrubstand.subrubric_name_standardized AS Subrubric,
		l.line_id AS Line_id,
		date.start_date_standardized AS Date,
		asim.amount_simple_id AS Amount_id
	FROM
		line l
		JOIN document doc ON l.document_id = doc.document_id
		JOIN emission emission ON doc.document_id = emission.document_id
		JOIN person emitter ON emission.person_id = emitter.person_id
		JOIN rubric_extracted rubext ON l.rubric_extracted_id = rubext.rubric_extracted_id
		JOIN rubric_standardized rubstand ON rubext.rubric_standardized_id = rubstand.rubric_standardized_id
		LEFT JOIN subrubric_extracted subrubext ON l.subrubric_extracted_id = subrubext.subrubric_extracted_id
		LEFT JOIN subrubric_standardized subrubstand ON subrubext.subrubric_standardized_id = subrubstand.subrubric_standardized_id
		JOIN date ON l.date_id = date.date_id
		JOIN amount_simple asim ON l.line_id = asim.line_id
	WHERE
		l.line_type_id = 2 
	GROUP BY
		emitter.person_name_standardized,
		doc.document_name,
		rubstand.rubric_name_standardized,
		subrubstand.subrubric_name_standardized,
		l.line_id,
		date.start_date_standardized,
		asim.amount_simple_id
	) AS combined_results 
GROUP BY
	Emitter,
	Document,
	Rubric,
	Subrubric,
	Line_id,
	Date, 
	Amount_id