-- =========================================================================
--  RUN_ONCE_2.sql — round two. Everything a query can still settle.
-- =========================================================================
--
--  NO PLACEHOLDERS. Block 0 builds a proxy MM population as a temporary view
--  and everything else reuses it, so the whole file runs as-is.
--
--  IF YOU HAVE THE COHORT TABLE, say so by replacing block 0's body with
--      SELECT DISTINCT cast(PATID as string) AS PATID FROM <your cohort table>
--  Four answers get sharper; nothing else changes. Round one skipped every
--  cohort-dependent query, which is why Q11, Q13, Q16 and Q25 are still open.
--
--  Round one already closed Q8, Q10, Q22, Q24 and Q26.
--
--  Blocks 1-9 SETTLE:  Q1, Q2, Q5, Q9, Q11, Q13, Q14, Q16, Q19, Q25, Q27, Q28
--  Blocks 10-13 PRICE: Q21, Q23, Q7, Q3, Q6 - they cannot decide these, but
--      they say how many patients each reading moves, which is usually what
--      the study team needs in order to decide at all. Proxy code lists
--      stand in for Annexes 2 and 3; the order of magnitude does not turn
--      on the exact list.
--  Blocks 14-16 CHECK the value lists and columns the package assumes but has
--      never seen data for. No open question, but a wrong assumption here is
--      silent - a value outside the expected set is dropped, not flagged.
--
--  What NO query can answer, now or ever:
--      Q12, Q15, Q20 - the protocol author, or annexes we do not hold.
--  Q3, Q6, Q7 and Q23 need those annexes to DECIDE; blocks 10-13 only size
--  them. Q21 is arithmetic rather than data - block 10 shows the days at stake.
--
--  The 2026q1 vintage is confirmed to exist. Roughly 10 minutes.


-- =========================================================================
-- BLOCK 0 — the population everything else is scoped to.        ~1 minute
-- =========================================================================
-- Anyone with a myeloma diagnosis in the study window. NOT the study cohort -
-- no age, enrolment or exclusion criteria - but it bounds every scan below to
-- the right order of magnitude and needs nothing from outside the CDM.

CREATE OR REPLACE TEMPORARY VIEW mm_pts AS
SELECT DISTINCT cast(PATID as string) AS PATID
FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
WHERE  cast(FST_DT as date) >= date('2016-01-01')
  AND  upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE 'C90%';

SELECT count(*) AS mm_patients FROM mm_pts;


-- =========================================================================
-- BLOCK 1 — Q28. What is DOD.MBR_MATCH_TYPE?                   ~5 seconds
-- =========================================================================
-- The fifth DOD column, in no Optum document we hold. If it grades how each
-- member was linked to the death record, some fraction of those 11.5M deaths
-- are lower-confidence links and nothing filters on them. Overall survival is
-- a secondary objective.

SELECT MBR_MATCH_TYPE, count(*) AS n,
       round(100.0 * count(*) / sum(count(*)) OVER (), 2) AS pct
FROM   hive_metastore.clnprw_optum.t_dod_2026q1
GROUP BY MBR_MATCH_TYPE ORDER BY n DESC;


-- =========================================================================
-- BLOCK 2 — Q9. Does a member's STATE change?                   ~1 minute
-- =========================================================================
-- REGION is absent from this extract, so region comes from a STATE crosswalk.
-- The open half of Q9 is what to do when STATE changes between enrolment rows.
-- If almost nobody moves, "take the row covering the index date" is a
-- formality. If many do, it is a real choice.

SELECT n_states, count(*) AS n_members
FROM ( SELECT PATID, count(DISTINCT STATE) AS n_states
       FROM   hive_metastore.clnprw_optum.t_member_enrollment_2026q1
       WHERE  STATE IS NOT NULL
       GROUP BY PATID )
GROUP BY n_states ORDER BY n_states;


-- =========================================================================
-- BLOCK 3 — Q16. Do enrolment rows overlap, and do they disagree? ~2 minutes
-- =========================================================================
-- MEMBER_ENROLLMENT gets a new row whenever anything changes, and a member on
-- two concurrent plans has two rows covering the same day. Ranked on ELIGEFF
-- alone the winner was arbitrary, so race, ethnicity, region and insurance
-- could differ between two runs of identical code. The package now breaks the
-- tie on (ELIGEFF, ELIGEND, PAT_PLANID); this says how much was at stake.

WITH pairs AS (
  SELECT a.PATID,
         CASE WHEN a.RACE  IS DISTINCT FROM b.RACE  THEN 1 ELSE 0 END AS d_race,
         CASE WHEN a.BUS   IS DISTINCT FROM b.BUS   THEN 1 ELSE 0 END AS d_bus,
         CASE WHEN a.STATE IS DISTINCT FROM b.STATE THEN 1 ELSE 0 END AS d_state
  FROM       hive_metastore.clnprw_optum.t_member_enrollment_2026q1 a
  INNER JOIN hive_metastore.clnprw_optum.t_member_enrollment_2026q1 b
          ON b.PATID = a.PATID
         AND b.PAT_PLANID <> a.PAT_PLANID
         AND a.ELIGEFF <= b.ELIGEND AND b.ELIGEFF <= a.ELIGEND   -- overlapping
  INNER JOIN mm_pts m ON m.PATID = cast(a.PATID as string)
)
SELECT count(DISTINCT PATID)                                        AS members_with_overlap,
       count(DISTINCT CASE WHEN d_race  = 1 THEN PATID END)         AS disagree_on_race,
       count(DISTINCT CASE WHEN d_bus   = 1 THEN PATID END)         AS disagree_on_bus,
       count(DISTINCT CASE WHEN d_state = 1 THEN PATID END)         AS disagree_on_state
FROM pairs;


-- =========================================================================
-- BLOCK 4 — Q19 and Q13. Enrolment gaps, and what censoring costs. ~2 minutes
-- =========================================================================
-- Q19: the protocol bridges gaps of 30 days or fewer. Those bridged days are
-- covered on paper and unobserved in fact. This says how many days the study
-- would be counting as person-time that nobody was enrolled for.
-- Q13: the same rows price censoring — a member with no gaps is unaffected
-- either way.

WITH spans AS (
  SELECT cast(e.PATID as string) AS PATID,
         cast(e.ELIGEFF as date) AS s,
         cast(e.ELIGEND as date) AS e_end,
         lag(cast(e.ELIGEND as date)) OVER (PARTITION BY e.PATID
                                            ORDER BY e.ELIGEFF) AS prev_end
  FROM       hive_metastore.clnprw_optum.t_member_enrollment_2026q1 e
  INNER JOIN mm_pts m ON m.PATID = cast(e.PATID as string)
),
gaps AS (
  SELECT PATID, datediff(s, prev_end) - 1 AS gap_days
  FROM   spans WHERE prev_end IS NOT NULL AND s > prev_end
)
SELECT count(*)                                                    AS n_gaps,
       count(DISTINCT PATID)                                       AS members_with_a_gap,
       sum(CASE WHEN gap_days <= 30 THEN 1 ELSE 0 END)             AS gaps_bridged_le_30,
       sum(CASE WHEN gap_days <= 30 THEN gap_days ELSE 0 END)      AS bridged_days_total,
       sum(CASE WHEN gap_days = 30 THEN 1 ELSE 0 END)              AS gaps_exactly_30,
       sum(CASE WHEN gap_days = 29 THEN 1 ELSE 0 END)              AS gaps_exactly_29,
       round(avg(gap_days), 1)                                     AS mean_gap_days
FROM gaps;


-- =========================================================================
-- BLOCK 5 — Q2 and Q1. Which MM codes, and which start year.     ~2 minutes
-- =========================================================================
-- Q2: the outpatient arm of I1 may use a broader code set than the inpatient
-- arm. This is every myeloma-adjacent stem actually present, with patient
-- counts, so the study team can see exactly what "broad" would add.
--   C90.0 multiple myeloma   C90.1 plasma cell leukaemia
--   C90.2 plasmacytoma       C88.x malignant immunoproliferative
--   203.0x is the ICD-9 equivalent.

SELECT upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) AS code,
       ICD_FLAG,
       count(DISTINCT cast(PATID as string))           AS patients,
       count(*)                                        AS claim_lines
FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
WHERE  cast(FST_DT as date) >= date('2016-01-01')
  AND (upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE 'C90%'
    OR upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE 'C88%'
    OR upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE '2030%'
    OR upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE '2731%')
GROUP BY 1, 2 ORDER BY patients DESC LIMIT 40;

-- Q1: the study period starts 2016 or 2018, and the figure and the body text
-- disagree. This is what the two years differ by, in patients: the year of
-- each member's FIRST myeloma diagnosis.
SELECT year(first_dx) AS first_dx_year, count(*) AS patients
FROM ( SELECT cast(PATID as string) AS PATID,
              min(cast(FST_DT as date)) AS first_dx
       FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
       WHERE  upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE 'C90%'
       GROUP BY cast(PATID as string) )
WHERE year(first_dx) BETWEEN 2015 AND 2026
GROUP BY 1 ORDER BY 1;


-- =========================================================================
-- BLOCK 6 — Q25. How much of the medical table is DENIED?        ~3 minutes
-- =========================================================================
-- Skipped in round one because it was scoped to a cohort table that was not
-- supplied. MEDICAL.PAID_STATUS separates PAID from DENIED and nothing in
-- either build filters on it, so every count built on medical claims includes
-- denied lines. A percent or two is a footnote; ten inflates every rate.

SELECT m.PAID_STATUS, count(*) AS n_lines,
       round(100.0 * count(*) / sum(count(*)) OVER (), 2) AS pct
FROM       hive_metastore.clnprw_optum.t_medical_2026q1 m
INNER JOIN mm_pts p ON p.PATID = cast(m.PATID as string)
GROUP BY m.PAID_STATUS ORDER BY n_lines DESC;


-- =========================================================================
-- BLOCK 7 — Q11. The three ED constructions, side by side.       ~3 minutes
-- =========================================================================
-- Also skipped in round one. The CDM has no ED flag and the vendor says the
-- classification is "as per the study requirements". The three constructions
-- select structurally different claim types - RVNU_CD is facility-only, CPT
-- 9928x is professional - so they disagree by construction, not just in
-- number. `then_admitted` is the sub-question: an ED claim carrying a CONF_ID
-- became an admission (business rule 14), and is at risk of being counted
-- twice.

WITH ed AS (
  SELECT cast(m.PATID as string) AS PATID,
         cast(m.FST_DT as date)  AS dt,
         CASE WHEN upper(regexp_replace(trim(m.RVNU_CD),'[^A-Za-z0-9]',''))
                   RLIKE '^(045[0-9]|0981)$'                 THEN 1 ELSE 0 END AS by_rev,
         CASE WHEN trim(m.POS) = '23'                        THEN 1 ELSE 0 END AS by_pos,
         CASE WHEN trim(m.PROC_CD) BETWEEN '99281' AND '99285' THEN 1 ELSE 0 END AS by_cpt,
         CASE WHEN m.CONF_ID IS NOT NULL AND trim(m.CONF_ID) <> ''
                                                             THEN 1 ELSE 0 END AS inpat
  FROM       hive_metastore.clnprw_optum.t_medical_2026q1 m
  INNER JOIN mm_pts p ON p.PATID = cast(m.PATID as string)
  WHERE  cast(m.FST_DT as date) >= date('2018-01-01')
)
SELECT count(DISTINCT CASE WHEN by_rev=1 THEN concat(PATID,'|',cast(dt as string)) END) AS by_revenue_045x,
       count(DISTINCT CASE WHEN by_pos=1 THEN concat(PATID,'|',cast(dt as string)) END) AS by_pos_23,
       count(DISTINCT CASE WHEN by_cpt=1 THEN concat(PATID,'|',cast(dt as string)) END) AS by_cpt_9928x,
       count(DISTINCT CASE WHEN by_rev+by_pos+by_cpt>0
                           THEN concat(PATID,'|',cast(dt as string)) END)                AS any_of_three,
       count(DISTINCT CASE WHEN by_rev=1 AND by_cpt=1
                           THEN concat(PATID,'|',cast(dt as string)) END)                AS revenue_and_cpt_same_day,
       count(DISTINCT CASE WHEN by_rev+by_pos+by_cpt>0 AND inpat=1
                           THEN concat(PATID,'|',cast(dt as string)) END)                AS then_admitted
FROM ed;


-- =========================================================================
-- BLOCK 8 — Q27. The two routes to "MM in first or second position". ~4 min
-- =========================================================================
-- Route A - CONFINEMENT.DIAG1/DIAG2, the stay's own first two diagnoses. This
--           is what the package implements: five positions, stay level.
-- Route B - MED_DIAGNOSIS.DIAG_POSITION 1 or 2 on a claim carrying that
--           CONF_ID, which is the route business rule 13 documents:
--           twenty-five positions, claim line level.
-- If the counts come back close, the choice does not matter. If they differ
-- materially, somebody has to say which one s7.8.1 means.

WITH stays AS (
  SELECT cast(cf.PATID as string) AS PATID, cf.CONF_ID,
         CASE WHEN upper(regexp_replace(coalesce(cf.DIAG1,''),'[^A-Za-z0-9]','')) LIKE 'C90%'
                OR upper(regexp_replace(coalesce(cf.DIAG2,''),'[^A-Za-z0-9]','')) LIKE 'C90%'
              THEN 1 ELSE 0 END AS route_a,
         CASE WHEN upper(regexp_replace(coalesce(cf.DIAG3,''),'[^A-Za-z0-9]','')) LIKE 'C90%'
                OR upper(regexp_replace(coalesce(cf.DIAG4,''),'[^A-Za-z0-9]','')) LIKE 'C90%'
                OR upper(regexp_replace(coalesce(cf.DIAG5,''),'[^A-Za-z0-9]','')) LIKE 'C90%'
              THEN 1 ELSE 0 END AS mm_in_pos_3_to_5
  FROM       hive_metastore.clnprw_optum.t_confinement_2026q1 cf
  INNER JOIN mm_pts p ON p.PATID = cast(cf.PATID as string)
  WHERE  cast(cf.ADMIT_DATE as date) >= date('2018-01-01')
),
route_b AS (
  SELECT DISTINCT cast(m.PATID as string) AS PATID, m.CONF_ID
  FROM       hive_metastore.clnprw_optum.t_medical_2026q1 m
  INNER JOIN hive_metastore.clnprw_optum.t_med_diagnosis_2026q1 d
          ON cast(d.PATID as string) = cast(m.PATID as string)
         AND d.CLMID = m.CLMID
  INNER JOIN mm_pts p ON p.PATID = cast(m.PATID as string)
  WHERE  m.CONF_ID IS NOT NULL AND trim(m.CONF_ID) <> ''
    AND  try_cast(d.DIAG_POSITION as int) IN (1, 2)
    AND  upper(regexp_replace(d.DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
)
SELECT count(*)                                                     AS n_stays,
       sum(s.route_a)                                               AS route_a_conf_diag1_2,
       count(b.CONF_ID)                                             AS route_b_claim_pos_1_2,
       sum(CASE WHEN s.route_a=1 AND b.CONF_ID IS NULL THEN 1 ELSE 0 END) AS route_a_only,
       sum(CASE WHEN s.route_a=0 AND b.CONF_ID IS NOT NULL THEN 1 ELSE 0 END) AS route_b_only,
       sum(s.mm_in_pos_3_to_5)                                      AS mm_only_in_conf_pos_3_5
FROM      stays s
LEFT JOIN route_b b ON b.PATID = s.PATID AND b.CONF_ID = s.CONF_ID;


-- =========================================================================
-- BLOCK 9 — Q5 and Q14. Two boundary readings, priced.           ~3 minutes
-- =========================================================================
-- Q5: "evidence of follow-up" is at least one claim from the index date. Read
-- literally the index claim itself satisfies it and it excludes nobody. These
-- three counts are the three readings, against each member's first myeloma
-- diagnosis as a stand-in index.
-- Q14: how many members have a claim exactly ON that date - the population
-- that moves when the baseline window includes or excludes the index day.

WITH idx AS (
  SELECT cast(PATID as string) AS PATID, min(cast(FST_DT as date)) AS ix
  FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
  WHERE  upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
    AND  cast(FST_DT as date) >= date('2018-01-01')
  GROUP BY cast(PATID as string)
),
clm AS (
  SELECT i.PATID, i.ix,
         max(CASE WHEN cast(m.FST_DT as date) >= i.ix THEN 1 ELSE 0 END) AS from_index,
         max(CASE WHEN cast(m.FST_DT as date) >  i.ix THEN 1 ELSE 0 END) AS after_index,
         max(CASE WHEN cast(m.FST_DT as date)  = i.ix THEN 1 ELSE 0 END) AS on_index
  FROM       idx i
  INNER JOIN hive_metastore.clnprw_optum.t_medical_2026q1 m
          ON cast(m.PATID as string) = i.PATID
  GROUP BY i.PATID, i.ix
)
SELECT count(*)              AS n_members,
       sum(from_index)       AS have_a_claim_from_index,
       sum(after_index)      AS have_a_claim_after_index,
       sum(on_index)         AS have_a_claim_on_index,
       count(*) - sum(after_index) AS excluded_by_the_stricter_reading
FROM clm;


-- =========================================================================
-- BLOCK 10 — Q21. Calendar months versus fixed day counts.       ~1 minute
-- =========================================================================
-- "12 months" can be read as 365 days or as add_months(-12). They differ by
-- 0-2 days depending on leap years and the day of the month, and the build
-- applies the day count. This says how often the two disagree at all, and by
-- how much - which is the whole of the question.

WITH idx AS (
  SELECT cast(PATID as string) AS PATID, min(cast(FST_DT as date)) AS ix
  FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
  WHERE  upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
    AND  cast(FST_DT as date) >= date('2018-01-01')
  GROUP BY cast(PATID as string)
)
SELECT datediff(add_months(ix, -12), date_sub(ix, 365)) AS days_apart,
       count(*)                                         AS n_members
FROM idx GROUP BY 1 ORDER BY 1;


-- =========================================================================
-- BLOCK 11 — Q23. Which pregnancy window, priced.                ~3 minutes
-- =========================================================================
-- X3 excludes pregnancy. The open question is whether the window is the study
-- period or each patient's own baseline. Proxy codes only - ICD-10 O00-O9A
-- plus Z33/Z34/Z3A - because Annex 3 has not been delivered; the shape of the
-- answer does not depend on the exact list.
--
-- If the two windows exclude nearly the same people, the choice is academic.

WITH idx AS (
  SELECT cast(PATID as string) AS PATID, min(cast(FST_DT as date)) AS ix
  FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
  WHERE  upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
    AND  cast(FST_DT as date) >= date('2018-01-01')
  GROUP BY cast(PATID as string)
),
preg AS (
  SELECT cast(d.PATID as string) AS PATID, cast(d.FST_DT as date) AS dt
  FROM       hive_metastore.clnprw_optum.t_med_diagnosis_2026q1 d
  INNER JOIN mm_pts p ON p.PATID = cast(d.PATID as string)
  WHERE  upper(regexp_replace(d.DIAG,'[^A-Za-z0-9]','')) RLIKE '^(O[0-9A-Z]|Z3[34A])'
)
SELECT count(DISTINCT i.PATID)                                          AS mm_members,
       count(DISTINCT CASE WHEN pr.dt BETWEEN date('2018-01-01')
                                          AND date('2026-03-31')
                           THEN i.PATID END)                            AS excluded_study_period,
       count(DISTINCT CASE WHEN pr.dt BETWEEN date_sub(i.ix, 365)
                                          AND date_sub(i.ix, 1)
                           THEN i.PATID END)                            AS excluded_own_baseline,
       count(DISTINCT CASE WHEN pr.dt BETWEEN date('2018-01-01') AND date('2026-03-31')
                            AND NOT (pr.dt BETWEEN date_sub(i.ix,365) AND date_sub(i.ix,1))
                           THEN i.PATID END)                            AS study_period_only
FROM      idx i
LEFT JOIN preg pr ON pr.PATID = i.PATID;


-- =========================================================================
-- BLOCK 12 — Q7 and Q3. Prior malignancy, and the pairing window. ~4 minutes
-- =========================================================================
-- Q7: the secondary 2L cohort permits a prior malignancy where the primary
-- cohorts exclude it. This is the size of the population that turns on it -
-- how many myeloma members carry another cancer code in their baseline year.
-- Q3: X2 pairs two outpatient claims of the same cancer within a window. This
-- prices 30 days against 60 - how many members the wider window would exclude
-- that the narrower one would not.
--
-- Proxy: any C-code that is not myeloma (C90) and not a non-melanoma skin
-- cancer (C44), which the protocol usually exempts. Annex 3 supplies the real
-- grouping; this is the order of magnitude.

WITH idx AS (
  SELECT cast(PATID as string) AS PATID, min(cast(FST_DT as date)) AS ix
  FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
  WHERE  upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
    AND  cast(FST_DT as date) >= date('2018-01-01')
  GROUP BY cast(PATID as string)
),
other AS (
  SELECT cast(d.PATID as string)                            AS PATID,
         cast(d.FST_DT as date)                             AS dt,
         substr(upper(regexp_replace(d.DIAG,'[^A-Za-z0-9]','')), 1, 3) AS cat
  FROM       hive_metastore.clnprw_optum.t_med_diagnosis_2026q1 d
  INNER JOIN mm_pts p ON p.PATID = cast(d.PATID as string)
  WHERE  upper(regexp_replace(d.DIAG,'[^A-Za-z0-9]','')) RLIKE '^C[0-9]'
    AND  upper(regexp_replace(d.DIAG,'[^A-Za-z0-9]','')) NOT LIKE 'C90%'
    AND  upper(regexp_replace(d.DIAG,'[^A-Za-z0-9]','')) NOT LIKE 'C44%'
),
baseline AS (
  SELECT o.PATID, o.cat, o.dt
  FROM       other o
  INNER JOIN idx i ON i.PATID = o.PATID
  WHERE  o.dt BETWEEN date_sub(i.ix, 365) AND date_sub(i.ix, 1)
),
paired AS (
  SELECT a.PATID, a.cat, min(datediff(b.dt, a.dt)) AS gap
  FROM       baseline a
  INNER JOIN baseline b ON b.PATID = a.PATID AND b.cat = a.cat AND b.dt > a.dt
  GROUP BY a.PATID, a.cat
)
SELECT n.mm_members,
       count(DISTINCT pr.PATID)                                         AS with_a_paired_other_cancer,
       count(DISTINCT CASE WHEN pr.gap <= 30 THEN pr.PATID END)         AS paired_within_30d,
       count(DISTINCT CASE WHEN pr.gap <= 60 THEN pr.PATID END)         AS paired_within_60d,
       count(DISTINCT CASE WHEN pr.gap > 30 AND pr.gap <= 60
                           THEN pr.PATID END)                           AS only_the_60d_window
FROM      (SELECT count(*) AS mm_members FROM idx) n
LEFT JOIN paired pr ON true
GROUP BY n.mm_members;


-- =========================================================================
-- BLOCK 13 — Q6. Do steroid-only claims trigger the exclusion?    ~3 minutes
-- =========================================================================
-- X1 excludes prior MM therapy in baseline. The question is whether a claim
-- for a steroid alone counts as "MM oncology therapy". Proxy J-codes only -
-- J1100 dexamethasone, J7509/J7510/J7512 prednisolone and prednisone,
-- J2920/J2930 methylprednisolone - against a handful of unambiguous myeloma
-- agents. cl_mma_codelist is the real list.
--
-- `steroid_only` is the population the answer moves.

WITH idx AS (
  SELECT cast(PATID as string) AS PATID, min(cast(FST_DT as date)) AS ix
  FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
  WHERE  upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
    AND  cast(FST_DT as date) >= date('2018-01-01')
  GROUP BY cast(PATID as string)
),
tx AS (
  SELECT cast(m.PATID as string) AS PATID, cast(m.FST_DT as date) AS dt,
         CASE WHEN trim(m.PROC_CD) IN ('J1100','J7509','J7510','J7512',
                                       'J2920','J2930')          THEN 1 ELSE 0 END AS steroid,
         CASE WHEN trim(m.PROC_CD) IN ('J9041','J9044','J9047','J9145',
                                       'J9228','J9308','J9999','J0202',
                                       'J9037','J9061')          THEN 1 ELSE 0 END AS mm_agent
  FROM       hive_metastore.clnprw_optum.t_medical_2026q1 m
  INNER JOIN mm_pts p ON p.PATID = cast(m.PATID as string)
  WHERE  m.PROC_CD IS NOT NULL
),
base AS (
  SELECT t.PATID, max(t.steroid) AS any_steroid, max(t.mm_agent) AS any_agent
  FROM       tx t
  INNER JOIN idx i ON i.PATID = t.PATID
  WHERE  t.dt BETWEEN date_sub(i.ix, 365) AND date_sub(i.ix, 1)
  GROUP BY t.PATID
)
SELECT count(*)                                                       AS members_with_baseline_tx,
       sum(CASE WHEN any_agent = 1 THEN 1 ELSE 0 END)                 AS any_mm_agent,
       sum(CASE WHEN any_steroid = 1 THEN 1 ELSE 0 END)               AS any_steroid,
       sum(CASE WHEN any_steroid = 1 AND any_agent = 0 THEN 1 ELSE 0 END) AS steroid_only
FROM base;


-- =========================================================================
-- BLOCK 14 — the value lists the code assumes.                    ~2 minutes
-- =========================================================================
-- Not open questions, but each is a mapping the package applies and has never
-- seen data for. A value outside the expected set is silently dropped.

-- Sex. The package maps M and F and sends everything else to Unknown.
SELECT GDR_CD, count(*) AS n
FROM   hive_metastore.clnprw_optum.t_member_enrollment_2026q1
GROUP BY GDR_CD ORDER BY n DESC;

-- STATE. The census crosswalk carries the 50 states plus DC. Anything else -
-- PR, VI, GU, a blank, a territory - falls to region Unknown, and this is the
-- only place that would show it.
SELECT STATE, count(*) AS n
FROM   hive_metastore.clnprw_optum.t_member_enrollment_2026q1
GROUP BY STATE ORDER BY n DESC LIMIT 70;

-- DIAG_POSITION. The rule is "first or second position", and the package casts
-- this to int. If it is zero-padded, or carries a non-numeric value, that cast
-- decides whether a row is read at all.
SELECT DIAG_POSITION, count(*) AS n
FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
GROUP BY DIAG_POSITION ORDER BY n DESC LIMIT 30;


-- =========================================================================
-- BLOCK 15 — the two columns round one could not see.             ~3 minutes
-- =========================================================================
-- The DESCRIBE results were truncated to the first rows on screen, so neither
-- of these was confirmed. Both are load-bearing, and both are LAST in the file
-- because if a column does not exist the statement errors - and that error is
-- itself the answer.

-- CONFINEMENT.ICD_FLAG. The MM-related hospitalisation test now reads it, with
-- the admit date only as a fallback. If this errors, the column is absent and
-- 07_hcru.R must go back to the date.
SELECT ICD_FLAG, count(*) AS n
FROM   hive_metastore.clnprw_optum.t_confinement_2026q1
GROUP BY ICD_FLAG ORDER BY n DESC;

-- MEDICAL.PAID_STATUS. Block 6 above depends on it; if that block errored,
-- this says why.
SELECT PAID_STATUS, count(*) AS n
FROM   hive_metastore.clnprw_optum.t_medical_2026q1
WHERE  cast(FST_DT as date) >= date('2024-01-01')
GROUP BY PAID_STATUS ORDER BY n DESC;


-- =========================================================================
-- BLOCK 16 — hospitalisation shape.                               ~2 minutes
-- =========================================================================
-- Three things the HCRU module assumes and has never checked.
--   * how often a stay has no discharge date - the protocol counts those as
--     events and excludes them from LOS summaries, so this is how much of the
--     LOS denominator goes missing;
--   * whether CONFINEMENT.LOS agrees with discharge minus admit. The
--     dictionary says LOS spans bundled records, so it may not, and the
--     package computes its own rather than reading it;
--   * how much of MEDICAL is inpatient, which is what business rule 14's
--     CONF_ID test turns on.

SELECT count(*)                                                        AS stays,
       sum(CASE WHEN DISCH_DATE IS NULL THEN 1 ELSE 0 END)             AS no_discharge_date,
       sum(CASE WHEN DISCH_DATE IS NOT NULL
                 AND try_cast(LOS as int) = datediff(cast(DISCH_DATE as date),
                                                     cast(ADMIT_DATE as date))
                THEN 1 ELSE 0 END)                                     AS los_equals_datediff,
       sum(CASE WHEN DISCH_DATE IS NOT NULL
                 AND try_cast(LOS as int) <> datediff(cast(DISCH_DATE as date),
                                                      cast(ADMIT_DATE as date))
                THEN 1 ELSE 0 END)                                     AS los_differs,
       round(avg(try_cast(LOS as int)), 2)                             AS mean_los_column,
       round(avg(datediff(cast(DISCH_DATE as date),
                          cast(ADMIT_DATE as date))), 2)               AS mean_datediff
FROM       hive_metastore.clnprw_optum.t_confinement_2026q1 cf
INNER JOIN mm_pts p ON p.PATID = cast(cf.PATID as string)
WHERE  cast(cf.ADMIT_DATE as date) >= date('2018-01-01');

-- And how business rule 14's test actually behaves. The ED filter is
-- `CONF_ID IS NULL OR trim(CONF_ID) = ''`. If a non-inpatient claim carries 0
-- rather than NULL, that filter matches nothing and every ED visit disappears
-- from Table 7. This says which shape the absence takes.
SELECT CASE WHEN CONF_ID IS NULL                        THEN 'null'
            WHEN trim(cast(CONF_ID as string)) = ''     THEN 'blank'
            WHEN trim(cast(CONF_ID as string)) = '0'    THEN 'zero'
            ELSE 'populated' END                  AS conf_id_state,
       count(*)                                   AS claims,
       count(DISTINCT cast(PATID as string))      AS members
FROM   hive_metastore.clnprw_optum.t_medical_2026q1
WHERE  cast(FST_DT as date) >= date('2024-01-01')
GROUP BY 1 ORDER BY claims DESC;
