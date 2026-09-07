-- =========================================================================
--  RUN_ONCE_3.sql — round three.                        ~25 minutes, 13 stmts
-- =========================================================================
--
--  Round two priced sixteen questions. This closes the two loose ends it left,
--  and asks the six things nobody has ever asked the warehouse.
--
--  WHAT IT SETTLES
--    Q13   round two's gap query counted a contiguous re-enrolment as a gap,
--          so the number of MEMBERS with a real break is still unknown. (The
--          109,679 bridged-days figure is unaffected - zeros contribute
--          nothing - so Q19 stands.)
--    Q25   17.4% of medical lines are denied. What decides whether to filter
--          is WHERE they fall. If denials are spread evenly, excluding them
--          shifts every rate a little; if they concentrate in the ED and
--          inpatient claims, they move HCRU and nothing else.
--    STATE 53 distinct values against a 51-entry crosswalk, and the profile
--          was ordered by count and truncated. Two values are becoming region
--          Unknown and nobody knows which.
--
--  WHAT IT MEASURES FOR THE FIRST TIME
--    * whether I4/N2 - 12 months of continuous enrolment before index, with
--      gaps of 30 days or fewer bridged - passes 40% of patients or 90%. This
--      package applies that criterion itself and its pass rate is unknown.
--    * how many patients get a usable age. YRDOB is 0 on some rows and capped
--      at 89 on others; both land in the 75+ transplant-eligibility band if
--      unguarded.
--    * whether any death date precedes the patient's last claim, which would
--      corrupt overall survival - a secondary objective.
--    * DIAG_POSITION's 26th value.
--
--  LAST, BECAUSE AN ERROR IS THE ANSWER
--    RX was profiled in round one with a DESCRIBE whose output was truncated
--    on screen - the same way CONFINEMENT.ICD_FLAG and MEDICAL.PAID_STATUS
--    went unconfirmed until round two had to re-ask. The package reads
--    RX.FILL_DT and RX.PATID. Neither is confirmed present.
--
--  NO PLACEHOLDERS. Block 0 rebuilds the same proxy population round two used,
--  so every number is directly comparable to SQL Result 2.pdf.


-- =========================================================================
-- BLOCK 0 — the same proxy population as round two.            ~1 minute
-- =========================================================================

CREATE OR REPLACE TEMPORARY VIEW mm_pts AS
SELECT DISTINCT cast(PATID as string) AS PATID
FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
WHERE  cast(FST_DT as date) >= date('2016-01-01')
  AND  upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE 'C90%';

-- The index date round two used, so the windows below match it exactly.
CREATE OR REPLACE TEMPORARY VIEW mm_idx AS
SELECT cast(PATID as string) AS PATID, min(cast(FST_DT as date)) AS ix
FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
WHERE  upper(regexp_replace(DIAG, '[^A-Za-z0-9]', '')) LIKE 'C90%'
  AND  cast(FST_DT as date) >= date('2018-01-01')
GROUP BY cast(PATID as string);


-- =========================================================================
-- BLOCK 1 — Q13. Members with a real break in enrolment.       ~2 minutes
-- =========================================================================
-- A boundary is only a gap if the next span starts more than one day after
-- the last one ended. Round two's `gap_days <= 30` bucket counted the
-- contiguous case (gap_days = 0) as bridged, which is why 160,945 of 202,108
-- boundaries landed in it. This splits them properly and counts MEMBERS,
-- which is the unit censoring applies to.
--
-- `members_with_a_break_over_30` is the population Q13 moves: censor at
-- disenrollment and they lose their follow-up from that point; bridge and
-- they keep it.

WITH spans AS (
  SELECT cast(e.PATID as string) AS PATID,
         cast(e.ELIGEFF as date) AS s,
         lag(cast(e.ELIGEND as date)) OVER (PARTITION BY e.PATID
                                            ORDER BY e.ELIGEFF) AS prev_end
  FROM       hive_metastore.clnprw_optum.t_member_enrollment_2026q1 e
  INNER JOIN mm_pts m ON m.PATID = cast(e.PATID as string)
),
gaps AS (
  SELECT PATID, datediff(s, prev_end) - 1 AS gap_days
  FROM   spans WHERE prev_end IS NOT NULL AND s > prev_end
)
SELECT n.mm_members,
       count(DISTINCT CASE WHEN g.gap_days = 0 THEN g.PATID END)          AS members_contiguous_only,
       count(DISTINCT CASE WHEN g.gap_days BETWEEN 1 AND 30
                           THEN g.PATID END)                             AS members_with_a_bridged_gap,
       count(DISTINCT CASE WHEN g.gap_days > 30 THEN g.PATID END)        AS members_with_a_break_over_30,
       sum(CASE WHEN g.gap_days BETWEEN 1 AND 30 THEN 1 ELSE 0 END)      AS n_bridged_gaps,
       sum(CASE WHEN g.gap_days > 30 THEN 1 ELSE 0 END)                  AS n_breaks_over_30,
       round(avg(CASE WHEN g.gap_days > 30 THEN g.gap_days END), 1)      AS mean_break_days
FROM      (SELECT count(*) AS mm_members FROM mm_pts) n
LEFT JOIN gaps g ON true
GROUP BY n.mm_members;


-- =========================================================================
-- BLOCK 2 — I4 / N2. Does anyone actually have 12 months of CE?  ~3 minutes
-- =========================================================================
-- The criterion this package applies itself, and whose pass rate nobody has
-- ever measured. Spans are merged across gaps of 30 days or fewer - the
-- protocol's own bridging rule - and the merged span must cover the whole
-- baseline year. Gaps-and-islands, so the merge is done once rather than
-- pairwise.
--
-- If this passes 90% the criterion is a formality. If it passes 50% it is the
-- single largest attrition step in the funnel and the study team should know
-- that before the first build, not after.
--
-- max(ELIGEND) within an island assumes spans do not nest. For a feasibility
-- count that is the standard simplification and is noted rather than hidden.

WITH spans AS (
  SELECT cast(e.PATID as string) AS PATID,
         cast(e.ELIGEFF as date) AS s, cast(e.ELIGEND as date) AS e_end,
         lag(cast(e.ELIGEND as date)) OVER (PARTITION BY e.PATID
                                            ORDER BY e.ELIGEFF) AS prev_end
  FROM       hive_metastore.clnprw_optum.t_member_enrollment_2026q1 e
  INNER JOIN mm_pts m ON m.PATID = cast(e.PATID as string)
),
marked AS (
  SELECT PATID, s, e_end,
         CASE WHEN prev_end IS NULL OR datediff(s, prev_end) - 1 > 30
              THEN 1 ELSE 0 END AS new_island
  FROM spans
),
islands AS (
  SELECT PATID, s, e_end,
         sum(new_island) OVER (PARTITION BY PATID ORDER BY s
                               ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS grp
  FROM marked
),
merged AS (
  SELECT PATID, grp, min(s) AS cov_start, max(e_end) AS cov_end
  FROM   islands GROUP BY PATID, grp
),
per_member AS (
  SELECT i.PATID,
         max(CASE WHEN m.cov_start <= date_sub(i.ix, 365)
                   AND m.cov_end   >= date_sub(i.ix, 1)
                  THEN 1 ELSE 0 END)                             AS ce_12mo_bridged,
         max(CASE WHEN m.cov_start <= date_sub(i.ix, 365)
                   AND m.cov_end   >= i.ix
                  THEN 1 ELSE 0 END)                             AS ce_12mo_incl_index
  FROM      mm_idx i
  LEFT JOIN merged m ON m.PATID = i.PATID
  GROUP BY i.PATID
)
SELECT count(*)                     AS mm_members,
       sum(ce_12mo_bridged)         AS pass_12mo_ce_before_index,
       sum(ce_12mo_incl_index)      AS pass_12mo_ce_through_index,
       count(*) - sum(ce_12mo_bridged) AS lost_to_this_criterion,
       round(100.0 * sum(ce_12mo_bridged) / count(*), 1) AS pct_passing
FROM per_member;


-- =========================================================================
-- BLOCK 3 — Q25. Where the denied claims actually fall.        ~4 minutes
-- =========================================================================
-- 17.4% of medical lines among myeloma patients are denied. That number on
-- its own does not decide anything. What decides it is whether the denials sit
-- in the claims this study counts as events.

SELECT CASE WHEN m.CONF_ID IS NOT NULL                          THEN 'inpatient-linked'
            WHEN trim(m.POS) = '23'
              OR upper(regexp_replace(trim(m.RVNU_CD),'[^A-Za-z0-9]','')) RLIKE '^(045[0-9]|0981)$'
              OR trim(m.PROC_CD) BETWEEN '99281' AND '99285'    THEN 'ED-shaped'
            ELSE 'other outpatient' END                         AS claim_shape,
       count(*)                                                 AS lines,
       sum(CASE WHEN upper(trim(coalesce(m.PAID_STATUS,''))) = 'D'
                THEN 1 ELSE 0 END)                              AS denied,
       round(100.0 * sum(CASE WHEN upper(trim(coalesce(m.PAID_STATUS,''))) = 'D'
                              THEN 1 ELSE 0 END) / count(*), 2) AS pct_denied
FROM       hive_metastore.clnprw_optum.t_medical_2026q1 m
INNER JOIN mm_pts p ON p.PATID = cast(m.PATID as string)
WHERE  cast(m.FST_DT as date) >= date('2018-01-01')
GROUP BY 1 ORDER BY lines DESC;

-- And the sharper form: an ED visit is a patient-DAY, not a line. A day
-- survives if any of its lines was paid. `ed_days_every_line_denied` is the
-- count of ED visits that would disappear from Table 7 - the real cost of
-- CLAIM_STATUS=paid_only, as against the line-level percentage above.

WITH ed AS (
  SELECT cast(m.PATID as string) AS PATID, cast(m.FST_DT as date) AS dt,
         CASE WHEN upper(trim(coalesce(m.PAID_STATUS,''))) = 'D' THEN 0 ELSE 1 END AS kept
  FROM       hive_metastore.clnprw_optum.t_medical_2026q1 m
  INNER JOIN mm_pts p ON p.PATID = cast(m.PATID as string)
  WHERE  cast(m.FST_DT as date) >= date('2018-01-01')
    AND (trim(m.POS) = '23'
      OR upper(regexp_replace(trim(m.RVNU_CD),'[^A-Za-z0-9]','')) RLIKE '^(045[0-9]|0981)$'
      OR trim(m.PROC_CD) BETWEEN '99281' AND '99285')
),
per_day AS (
  SELECT PATID, dt, max(kept) AS any_kept FROM ed GROUP BY PATID, dt
)
SELECT count(*)                                        AS ed_days_all,
       sum(any_kept)                                   AS ed_days_with_a_paid_line,
       sum(1 - any_kept)                               AS ed_days_every_line_denied,
       round(100.0 * sum(1 - any_kept) / count(*), 2)  AS pct_ed_days_lost
FROM per_day;


-- =========================================================================
-- BLOCK 4 — age. How many patients get one.                    ~2 minutes
-- =========================================================================
-- YRDOB is 0 on some rows and CAPPED at 89 on others. Unguarded, year(index)
-- minus 0 is an age of about 2,020 and lands in the 75+ band, which is the
-- transplant-eligibility split. The package guards it; this says what the
-- guard costs and what Table 4's age distribution will look like.

-- Taking max(YRDOB) rather than the row covering the index date does two
-- things at once: it is immune to the tiebreak question (Q16), and where a
-- member carries both 0 and a real year it picks the real one - which is what
-- a guard should do anyway. `members_with_two_birth_years` tests an assumption
-- nobody has checked: that YRDOB is constant within a member.
WITH one_row AS (
  SELECT i.PATID, i.ix,
         max(cast(e.YRDOB as int))  AS yrdob,
         count(DISTINCT e.YRDOB)    AS n_yrdob
  FROM       mm_idx i
  INNER JOIN hive_metastore.clnprw_optum.t_member_enrollment_2026q1 e
          ON cast(e.PATID as string) = i.PATID
  GROUP BY i.PATID, i.ix
)
SELECT count(*)                                                     AS mm_members,
       sum(CASE WHEN n_yrdob > 1 THEN 1 ELSE 0 END)                 AS members_with_two_birth_years,
       sum(CASE WHEN yrdob IS NULL THEN 1 ELSE 0 END)               AS yrdob_null,
       sum(CASE WHEN yrdob = 0 THEN 1 ELSE 0 END)                   AS yrdob_zero,
       sum(CASE WHEN yrdob BETWEEN 1900 AND year(ix)
                 AND year(ix) - yrdob BETWEEN 0 AND 120
                THEN 1 ELSE 0 END)                                  AS age_usable,
       sum(CASE WHEN year(ix) - yrdob < 18 AND yrdob BETWEEN 1900 AND year(ix)
                THEN 1 ELSE 0 END)                                  AS under_18_at_index,
       sum(CASE WHEN year(ix) - yrdob BETWEEN 18 AND 64
                THEN 1 ELSE 0 END)                                  AS age_18_64,
       sum(CASE WHEN year(ix) - yrdob BETWEEN 65 AND 74
                THEN 1 ELSE 0 END)                                  AS age_65_74,
       sum(CASE WHEN year(ix) - yrdob BETWEEN 75 AND 120
                THEN 1 ELSE 0 END)                                  AS age_75_plus
FROM one_row;


-- =========================================================================
-- BLOCK 5 — death dates, for overall survival.                 ~2 minutes
-- =========================================================================
-- OS is a secondary objective and nothing has ever checked the death dates
-- against the claims. A death date BEFORE the patient's last claim is either a
-- bad link or a bad date, and either way it produces a negative or truncated
-- survival time. `deaths_before_last_claim` is the number of patients that
-- would happen to.

WITH last_claim AS (
  SELECT cast(m.PATID as string) AS PATID, max(cast(m.FST_DT as date)) AS last_dt
  FROM       hive_metastore.clnprw_optum.t_medical_2026q1 m
  INNER JOIN mm_pts p ON p.PATID = cast(m.PATID as string)
  GROUP BY cast(m.PATID as string)
),
dead AS (
  -- YMDOD is a CCYYMM STRING, not a date - round one confirmed all 11.5M rows
  -- are six characters, 200005 to 202603. cast(YMDOD as date) would be NULL on
  -- every row and this block would report zero deaths as a finding. The build
  -- takes the 15th of the known month; so does this.
  SELECT cast(d.PATID as string) AS PATID,
         min(to_date(concat(substr(trim(d.YMDOD), 1, 6), '15'), 'yyyyMMdd')) AS dod
  FROM       hive_metastore.clnprw_optum.t_dod_2026q1 d
  INNER JOIN mm_pts p ON p.PATID = cast(d.PATID as string)
  WHERE  d.YMDOD IS NOT NULL AND length(trim(d.YMDOD)) >= 6
  GROUP BY cast(d.PATID as string)
)
SELECT count(DISTINCT i.PATID)                                          AS mm_members,
       count(DISTINCT dd.PATID)                                         AS with_a_death_date,
       count(DISTINCT CASE WHEN dd.dod < i.ix THEN i.PATID END)         AS death_before_index,
       count(DISTINCT CASE WHEN dd.dod < lc.last_dt THEN i.PATID END)   AS deaths_before_last_claim,
       round(avg(CASE WHEN dd.dod >= lc.last_dt
                      THEN datediff(dd.dod, lc.last_dt) END), 1)        AS mean_days_last_claim_to_death
FROM      mm_idx i
LEFT JOIN dead dd      ON dd.PATID = i.PATID
LEFT JOIN last_claim lc ON lc.PATID = i.PATID;


-- =========================================================================
-- BLOCK 6 — the two truncated tails from round two.            ~2 minutes
-- =========================================================================

-- The STATE values the census crosswalk does not map. Asked the other way
-- round from round two: not the top 70 by count, but everything OUTSIDE the
-- 51 the crosswalk carries. Whatever comes back is becoming region Unknown.
SELECT coalesce(STATE, '(null)')      AS unmapped_state,
       count(*)                       AS enrolment_rows,
       count(DISTINCT PATID)          AS members
FROM   hive_metastore.clnprw_optum.t_member_enrollment_2026q1
WHERE  STATE IS NULL
   OR  upper(trim(STATE)) NOT IN (
         'CT','ME','MA','NH','RI','VT','NJ','NY','PA',
         'IL','IN','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD',
         'DE','DC','FL','GA','MD','NC','SC','VA','WV','AL','KY','MS',
         'TN','AR','LA','OK','TX',
         'AZ','CO','ID','MT','NV','NM','UT','WY','AK','CA','HI','OR','WA')
GROUP BY 1 ORDER BY enrolment_rows DESC;

-- DIAG_POSITION came back with 26 distinct values for a documented range of
-- 1-25, and the listing was cut at 13. The package casts this to int; a value
-- that does not cast becomes NULL and the row is read as no position at all.
SELECT coalesce(DIAG_POSITION, '(null)') AS odd_position, count(*) AS n
FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
WHERE  DIAG_POSITION IS NULL
   OR  try_cast(DIAG_POSITION as int) IS NULL
   OR  try_cast(DIAG_POSITION as int) NOT BETWEEN 1 AND 25
GROUP BY 1 ORDER BY n DESC LIMIT 20;


-- Round two's code profile was capped at 40 rows and every one of the top
-- thirteen was ICD-10. If any member carries ONLY an ICD-9 myeloma code, an
-- ICD-10-only mm_dx.csv misses them entirely - which is a concrete
-- requirement on a code list that has yet to be authored, not a preference.
SELECT sum(CASE WHEN has10 = 1 THEN 1 ELSE 0 END)               AS members_with_icd10,
       sum(CASE WHEN has9  = 1 THEN 1 ELSE 0 END)               AS members_with_icd9,
       sum(CASE WHEN has9 = 1 AND has10 = 0 THEN 1 ELSE 0 END)  AS members_icd9_only
FROM (
  SELECT cast(PATID as string) AS PATID,
         max(CASE WHEN upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
                  THEN 1 ELSE 0 END)                            AS has10,
         max(CASE WHEN upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE '2030%'
                  THEN 1 ELSE 0 END)                            AS has9
  FROM   hive_metastore.clnprw_optum.t_med_diagnosis_2026q1
  WHERE  cast(FST_DT as date) >= date('2016-01-01')
    AND (upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE 'C90%'
      OR upper(regexp_replace(DIAG,'[^A-Za-z0-9]','')) LIKE '2030%')
  GROUP BY cast(PATID as string)
) t;


-- =========================================================================
-- BLOCK 7 — RX. LAST, because an error is the answer.          ~3 minutes
-- =========================================================================
-- The package reads RX.FILL_DT and RX.PATID in build_fu_claims(), and neither
-- has been confirmed present: round one's probe was a DESCRIBE whose output
-- was truncated on screen, which is exactly how CONFINEMENT.ICD_FLAG and
-- MEDICAL.PAID_STATUS went unconfirmed until round two had to re-ask.
--
-- If this errors, the column is absent and 01_cohorts.R needs the real name.

SELECT count(*)                            AS rx_lines,
       count(DISTINCT cast(PATID as string)) AS members,
       min(cast(FILL_DT as date))          AS earliest_fill,
       max(cast(FILL_DT as date))          AS latest_fill,
       sum(CASE WHEN FILL_DT IS NULL THEN 1 ELSE 0 END) AS fill_dt_null,
       sum(CASE WHEN NDC IS NULL THEN 1 ELSE 0 END)     AS ndc_null
FROM       hive_metastore.clnprw_optum.t_rx_2026q1 r
INNER JOIN mm_pts p ON p.PATID = cast(r.PATID as string);

-- And whether a denied PHARMACY claim can be excluded at all. The V9.0 field
-- list gives RX no PAID_STATUS, which would mean CLAIM_STATUS=paid_only can
-- only ever apply to the medical side - an asymmetry the study team should
-- know about before choosing it. An error here IS that answer.
SELECT PAID_STATUS, count(*) AS n
FROM   hive_metastore.clnprw_optum.t_rx_2026q1
GROUP BY PAID_STATUS ORDER BY n DESC;
