-- sqlite> .headers on
-- sqlite> .output data.csv
-- sqlite> .mode csv

ATTACH DATABASE "lexibank.sqlite" AS lb;
ATTACH DATABASE "clts.sqlite" AS clts;
ATTACH DATABASE "johanssonsoundsymbolic.sqlite" AS jss;


WITH lb_segments
    AS (
    SELECT
        ROW_NUMBER() OVER() AS wd_id,
        '' AS unicode,
        f.cldf_segments || ' ' AS segments,
        f.cldf_segments AS word,
        l.cldf_glottocode AS glottocode,
        l.cldf_macroarea AS macroarea,
        l.family AS family,
        p.Concepticon_Gloss AS concept
    FROM
        lb.formtable AS f,
        lb.languagetable AS l,
        lb.parametertable AS p
    WHERE
        f.cldf_languagereference = l.cldf_id
            AND
        f.cldf_parameterreference = p.cldf_id
    UNION ALL
    SELECT
        wd_id,
        substr(segments, 0, instr(segments, ' ')),
        substr(segments, instr(segments, ' ') + 1),
        word,
        glottocode,
        macroarea,
        family,
        concept
    FROM
        lb_segments
    WHERE
        segments != ''
    )
SELECT
    lb.wd_id,
    lb.unicode,
    lb.word,
    lb.glottocode AS language,
    lb.macroarea,
    lb.family,
    lb.concept,
    clts.name,
    n_der.n_phones AS nPhonemesPerWord
FROM
    lb_segments AS lb
LEFT JOIN
    clts."data/sounds.tsv" AS clts
ON
    clts.grapheme = lb.unicode
INNER JOIN
    (
        SELECT
            jss_p.Concepticon_Gloss as concept
        FROM
            jss.parametertable AS jss_p
    ) AS jss
ON
    -- Add only concepts that ARE in JSS
    jss.concept = lb.concept
LEFT JOIN
    (
        SELECT
            jss_l.cldf_glottocode AS glottocode
        FROM
            jss.languagetable AS jss_l
    ) AS jss_l
ON
    jss_l.glottocode = lb.glottocode
INNER JOIN
    (
        SELECT
            -- wd_id is necessary for the joining, but I do not actually want that column
            wd_id,
            unicode,
            -- seems necessary in order to arrive at the correct number of segments, possibly because of the +1 in the segments iteration?
            count(*)-1 AS n_phones
        FROM
            lb_segments
        WHERE
            unicode != '+'
        GROUP BY
            wd_id
    ) AS n_der
ON n_der.wd_id = lb.wd_id 
WHERE
    -- Add only languages that ARE NOT in JSS
    jss_l.glottocode IS NULL
        AND
    lb.unicode != '+'
        AND
    lb.unicode != ''
;
