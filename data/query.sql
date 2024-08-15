.headers on
.output data.csv
.mode csv

ATTACH DATABASE "lexibank2.sqlite3" AS lb;
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
        l.cldf_latitude AS latitude,
        l.cldf_longitude AS longitude,
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
        latitude,
        longitude,
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
    lb.latitude,
    lb.longitude,
    lb.concept,
    clts.name AS name,
    n_der.n_phones AS nPhonemesPerWord,
    CASE
        WHEN name LIKE '%unrounded%' THEN 'unrounded'
        WHEN name LIKE '%rounded%' THEN 'rounded'
        WHEN name LIKE '%vowel%' THEN 'PROBLEM'
        ELSE ''
        END roundedness,
    CASE
        WHEN name LIKE '%back%' THEN 'back'
        WHEN name LIKE '%central%' THEN 'central'
        WHEN name LIKE '%front%' THEN 'front'
        WHEN name LIKE '%vowel%' THEN 'PROBLEM'
        ELSE ''
        END backness,
    CASE
        WHEN name LIKE '%mid%' THEN 'mid'
        WHEN name LIKE '%close%' THEN 'high'
        WHEN name LIKE '%open%' THEN 'low'
        WHEN name LIKE '%vowel%' THEN 'PROBLEM'
        ELSE ''
        END height,
    CASE
        WHEN
            name LIKE '%close%back%'
                OR
            name LIKE '%close%central%'
                OR
            -- schwa
            name LIKE '%mid central%'
            THEN 'high-back'
       WHEN
            name LIKE '%open%back%'
                OR
            name LIKE '%open central%'
            THEN 'low-back'
       WHEN name LIKE '%open%front%' THEN 'low-front'
       WHEN
            name LIKE '%close%front%'
                OR
            name LIKE '%mid front%'
            THEN 'high-front'
        WHEN name LIKE '%vowel%' THEN 'PROBLEM'
        ELSE ''
        END extreme,
    -- height || '-' || roundedness AS extreme_roundedness
    CASE 
        WHEN name LIKE '%voiceless%' THEN 'unvoiced'
        WHEN name LIKE '%voiced%' THEN 'voiced'
        WHEN name LIKE '%consonant' THEN 'PROBLEM'
        ELSE ''
        END voicing,
    CASE
        WHEN name LIKE '%nasal consonant' THEN 'nasal'
        WHEN
            name LIKE '%stop consonant'
                OR
            name LIKE '%ejective%'
                OR
            name LIKE '%click consonant'
                OR
            name LIKE '%affricate consonant'
                OR
            name LIKE '%implosive%'
            THEN 'stop'
        WHEN name LIKE '%lateral approximant consonant' THEN 'lateral'
        WHEN 
            name LIKE '%fricative consonant'
                OR
            name LIKE '%approximant consonant'
            THEN 'continuant'
        WHEN
            name LIKE '%tap consonant'
                OR
            name LIKE '%trill consonant'
            THEN 'vibrant'
        WHEN name LIKE '%consonant' THEN 'PROBLEM'
        ELSE ''
        END manner,
    CASE
        WHEN
            name LIKE '%alveolar%'
                OR
            name LIKE '%dental%'
            THEN 'alveolar'
        WHEN
            name LIKE '%glottal%'
                OR
            name LIKE '%pharyngeal %'
            THEN 'glottal'
        WHEN name LIKE '%labi%' THEN 'labial'  
        WHEN
            name LIKE '%palatal%'
                OR
            name LIKE '%retroflex%'
            THEN 'palatal'
        WHEN
            name LIKE '%velar %'
                OR
            name LIKE '%uvular%'
            THEN 'velar'
        WHEN name LIKE '%consonant' THEN 'PROBLEM'
        END position
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

