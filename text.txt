-- Databricks notebook source
-- MAGIC %md
-- MAGIC # Patient LOT journey drill-down (raw claims -> MAP -> assigned LOT)
-- MAGIC
-- MAGIC Reproduces the `LOT1_Base_exmpls.xlsx` view in SQL: for a set of example
-- MAGIC patients, trace them through the three layers the pipeline builds.
-- MAGIC
-- MAGIC | Layer | Cell | Work table | What it shows |
-- MAGIC |------|------|-----------|----------------|
-- MAGIC | 0 (optional) | 5 | CDM `t_medical` / `t_rx` | The absolutely-raw claim rows (pre-dedup) |
-- MAGIC | 1 RAW | 4 | `MMA_MED_PROCESSED` | Each mapped med claim (code -> MED_ABBR) that feeds MAP-building |
-- MAGIC | 2 MAP | 3 | `MAP_STACKED` | How claims are grouped into coverage episodes (MAP start/end, +90d runout) |
-- MAGIC | 3 LOT | 2 | `LOT_LONG` | How the grouped MAPs roll up into assigned lines (the color-coded table) |
-- MAGIC
-- MAGIC Cell 2 is the headline: it re-creates the color-coded sheet and adds a
-- MAGIC transparent `ROW_ROLE` / `COLOR` derived from the engine's own `LOT_LONG`.
-- MAGIC
-- MAGIC **How to run:** set the widgets (Cell 0), run Cell 1 to pick example
-- MAGIC patients, then run any layer. Each layer cell is independent once Cell 1
-- MAGIC has run. Nothing is written; all cells are read-only SELECTs.

-- COMMAND ----------

-- MAGIC %md
-- MAGIC ## Cell 0 - parameters
-- MAGIC Fully-qualified schemas are pre-filled with the pipeline defaults
-- MAGIC (`hive_metastore` catalog, Optum CDM `clnprw_optum`, work schema
-- MAGIC `gsk_mm_lot_work`, quarter `2025q2` from STUDY_END 2025-06-30). Change
-- MAGIC `work` if your Domino work schema is your username. Set `lot_table` to
-- MAGIC `NDMM_LOT_LONG_FILT` for the NDMM cohort. Leave `patids` empty to
-- MAGIC auto-select example patients, or paste a comma-separated list.

-- COMMAND ----------

CREATE WIDGET TEXT work      DEFAULT 'hive_metastore.gsk_mm_lot_work';
CREATE WIDGET TEXT cdm       DEFAULT 'hive_metastore.clnprw_optum';
CREATE WIDGET TEXT quarter   DEFAULT '2025q2';
CREATE WIDGET TEXT lot_table DEFAULT 'LOT_LONG';       -- or NDMM_LOT_LONG_FILT
CREATE WIDGET TEXT patids    DEFAULT '';               -- e.g. 'A0001,A0002' ; empty = auto-select

-- COMMAND ----------

-- MAGIC %md
-- MAGIC ## Cell 1 - choose example patients (`journey_patids`)
-- MAGIC Uses the `patids` widget if set; otherwise auto-selects the patients with
-- MAGIC the longest journeys (most lines / most meds) so the examples show real
-- MAGIC line switches, like the workbook. Deterministic (ties broken by PATID).

-- COMMAND ----------

CREATE OR REPLACE TEMP VIEW journey_patids AS
WITH manual AS (
  SELECT trim(p) AS PATID
  FROM (SELECT explode(split('${patids}', ',')) AS p) s
  WHERE trim(p) <> ''
),
ranked AS (
  SELECT
    cast(ll.PATID as string) AS PATID,
    count(*)                 AS n_lots,
    sum(ll.LOT_MED_CNT)      AS n_base_meds,
    max(ll.LOT_NUM)          AS max_lot
  FROM ${work}.${lot_table} ll
  GROUP BY cast(ll.PATID as string)
  HAVING count(*) >= 2                       -- at least one line switch to be illustrative
),
auto AS (
  SELECT PATID
  FROM ranked
  ORDER BY n_lots DESC, n_base_meds DESC, PATID
  LIMIT 8
)
SELECT PATID FROM manual
UNION
SELECT PATID FROM auto
WHERE NOT EXISTS (SELECT 1 FROM manual);       -- auto only when no manual list given

-- COMMAND ----------

SELECT * FROM journey_patids ORDER BY PATID;   -- inspect the chosen patients

-- COMMAND ----------

-- MAGIC %md
-- MAGIC ## Cell 2 - LAYER 3: how LOT is assigned  (reproduces the color-coded sheet)
-- MAGIC One row per MAP episode, attached to the line in effect at the MAP start,
-- MAGIC classified against the engine's own `LOT_LONG`. Steroids are excluded here
-- MAGIC because the LOT engine ignores them (they never sit in a base regimen);
-- MAGIC remove the `<> 'STEROID'` filter to see them.
-- MAGIC
-- MAGIC **LOT Base Logic key (from the workbook):**
-- MAGIC - GREEN  = LOT start / continuation of therapy (med is in the line's base regimen)
-- MAGIC - ORANGE = new drug, different class OR the same med > 90 days later (starts / ends a line)
-- MAGIC - Exception: LENA -> POMA (same class) ends the LOT
-- MAGIC
-- MAGIC `ROW_ROLE` / `COLOR` are derived transparently from `LOT_LONG` (not a
-- MAGIC re-implementation of the engine). `LOT_BASE_END_REASON` is the engine's
-- MAGIC OWN reason the active line ended - the authoritative "why".

-- COMMAND ----------

WITH pats AS (SELECT PATID FROM journey_patids),
lots AS (
  SELECT
    cast(PATID as string)               AS PATID,
    LOT_NUM,
    cast(LOT_START_DT   as date)        AS LOT_START_DT,
    cast(LOT_BASE_END_DT as date)       AS LOT_BASE_END_DT,
    upper(LOT_BASE_MEDS)                AS LOT_BASE_MEDS,
    LOT_MED_CNT,
    LOT_START_TYPE,
    LOT_BASE_END_REASON,
    LOT_BASE_1ST_ADD_MED,
    cast(LOT_BASE_1ST_ADD_MED_DT as date) AS LOT_BASE_1ST_ADD_MED_DT
  FROM ${work}.${lot_table}
  WHERE cast(PATID as string) IN (SELECT PATID FROM pats)
),
-- patient med -> class lookup (from the MAP layer), used to test "different class"
pmc AS (
  SELECT DISTINCT cast(PATID as string) AS PATID,
         upper(MAP_MED_TYPE)  AS MED,
         upper(MAP_MED_CLASS) AS CLS
  FROM ${work}.MAP_STACKED
  WHERE cast(PATID as string) IN (SELECT PATID FROM pats)
),
-- the set of drug classes represented in each line's base regimen
base_classes AS (
  SELECT l.PATID, l.LOT_NUM, collect_set(pmc.CLS) AS BASE_CLASSES
  FROM lots l
  JOIN pmc
    ON pmc.PATID = l.PATID
   AND array_contains(split(l.LOT_BASE_MEDS, ' '), pmc.MED)
  GROUP BY l.PATID, l.LOT_NUM
),
maps AS (
  SELECT
    cast(PATID as string)          AS PATID,
    MAP_CNT,
    upper(MAP_MED_TYPE)            AS MED,
    upper(MAP_MED_CLASS)           AS MED_CLASS,
    cast(MAP_START_DT as date)     AS MAP_START_DT,
    cast(MAP_END_DT   as date)     AS MAP_END_DT,
    date_add(cast(MAP_END_DT as date), 90) AS MAP_END_PLUS_90,
    -- gap (days) from the previous episode of the SAME med -> the ">90 days" rule
    datediff(
      cast(MAP_START_DT as date),
      lag(cast(MAP_END_DT as date)) OVER (
        PARTITION BY cast(PATID as string), upper(MAP_MED_TYPE)
        ORDER BY cast(MAP_START_DT as date)
      )
    ) AS GAP_DAYS_PREV_SAME_MED
  FROM ${work}.MAP_STACKED
  WHERE cast(PATID as string) IN (SELECT PATID FROM pats)
    AND upper(MAP_MED_CLASS) <> 'STEROID'      -- engine ignores steroids; remove to include
),
-- attach the line in effect at the MAP start = latest line started on/before it
map_lot AS (
  SELECT m.*, l.LOT_NUM AS ACTIVE_LOT, l.LOT_BASE_MEDS AS ACTIVE_BASE,
         l.LOT_BASE_END_REASON, l.LOT_START_TYPE,
         l.LOT_BASE_1ST_ADD_MED, l.LOT_BASE_1ST_ADD_MED_DT
  FROM maps m
  JOIN lots l
    ON l.PATID = m.PATID
   AND l.LOT_START_DT <= m.MAP_START_DT
  QUALIFY row_number() OVER (
    PARTITION BY m.PATID, m.MED, m.MAP_START_DT, m.MAP_CNT
    ORDER BY l.LOT_START_DT DESC
  ) = 1
),
-- does this MAP start fall exactly on a line's start date?
starts AS (
  SELECT DISTINCT PATID, LOT_START_DT, LOT_NUM FROM lots
),
enriched AS (
  SELECT
    ml.PATID,
    ml.ACTIVE_LOT,
    ml.ACTIVE_BASE                                                   AS LOT_BASE_MEDS,
    ml.LOT_BASE_1ST_ADD_MED                                          AS EARLIEST_ADDED_MED,
    ml.LOT_BASE_1ST_ADD_MED_DT                                       AS EARLIEST_ADDED_MED_DT,
    ml.MED                                                           AS MEDICATION,
    ml.MED_CLASS                                                     AS MEDICATION_CLASS,
    ml.MAP_START_DT,
    ml.MAP_END_DT,
    ml.MAP_END_PLUS_90,
    ml.GAP_DAYS_PREV_SAME_MED,
    ml.LOT_BASE_END_REASON,
    ml.LOT_START_TYPE,
    array_contains(split(ml.ACTIVE_BASE, ' '), ml.MED)              AS IS_IN_BASE,
    st.LOT_NUM                                                       AS STARTS_LOT_NUM,
    coalesce(array_contains(bc.BASE_CLASSES, ml.MED_CLASS), false)  AS SAME_CLASS_AS_BASE,
    (ml.MED = 'POMA' AND array_contains(split(ml.ACTIVE_BASE, ' '), 'LENA')) AS LEN_POM_SWITCH
  FROM map_lot ml
  LEFT JOIN starts st
    ON st.PATID = ml.PATID AND st.LOT_START_DT = ml.MAP_START_DT
  LEFT JOIN base_classes bc
    ON bc.PATID = ml.PATID AND bc.LOT_NUM = ml.ACTIVE_LOT
)
SELECT
  PATID,
  ACTIVE_LOT,
  LOT_BASE_MEDS,
  EARLIEST_ADDED_MED,
  EARLIEST_ADDED_MED_DT,
  MEDICATION,
  MEDICATION_CLASS,
  MAP_START_DT,
  MAP_END_DT,
  MAP_END_PLUS_90,
  -- transparent classification (mirrors the workbook Key) -----------------
  CASE
    WHEN IS_IN_BASE AND STARTS_LOT_NUM IS NOT NULL THEN concat('LOT', STARTS_LOT_NUM, ' START')
    WHEN IS_IN_BASE                                THEN 'CONTINUATION'
    ELSE                                                'NEW DRUG (outside base)'
  END AS ROW_ROLE,
  CASE WHEN IS_IN_BASE THEN 'GREEN' ELSE 'ORANGE' END AS COLOR,
  CASE
    WHEN IS_IN_BASE                       THEN NULL
    WHEN LEN_POM_SWITCH                   THEN 'Len -> Pom (same class) - ends the LOT (exception)'
    WHEN NOT SAME_CLASS_AS_BASE           THEN 'New drug, different class'
    WHEN GAP_DAYS_PREV_SAME_MED >= 90     THEN 'Same class but > 90-day gap'
    ELSE                                       'Not part of the base regimen'
  END AS NEW_DRUG_REASON,
  -- supporting facts (what the analyst colored by) ------------------------
  IS_IN_BASE,
  SAME_CLASS_AS_BASE,
  LEN_POM_SWITCH,
  GAP_DAYS_PREV_SAME_MED,
  LOT_BASE_END_REASON,      -- engine's OWN reason the active line ended
  LOT_START_TYPE
FROM enriched
ORDER BY PATID, MAP_START_DT, MEDICATION;

-- COMMAND ----------

-- MAGIC %md
-- MAGIC ## Cell 3 - LAYER 2: how MAPs are created and grouped  (`MAP_STACKED`)
-- MAGIC One row per coverage episode: consecutive claims of the same drug are
-- MAGIC merged into a MAP that runs from the first claim (`MAP_START_DT`) to the
-- MAGIC last day of coverage (`MAP_END_DT`); a new episode opens when a claim
-- MAGIC lands beyond both runout dates. `MAP_END_DT + 90` is the discontinuation
-- MAGIC horizon (`MAP_DISCON_FLG = 1` when the next same-drug MAP or the
-- MAGIC observation end is >= 90 days out). Steroids kept here (flagged) so you
-- MAGIC can see them; they are dropped from the LOT layer.

-- COMMAND ----------

SELECT
  cast(PATID as string)              AS PATID,
  MAP_MED_TYPE                       AS MEDICATION,
  MAP_MED_CLASS                      AS MEDICATION_CLASS,
  MAP_CNT                            AS EPISODE_NUM,
  cast(MAP_START_DT as date)         AS MAP_START_DT,
  cast(MAP_END_DT   as date)         AS MAP_END_DT,
  date_add(cast(MAP_END_DT as date), 90) AS MAP_END_PLUS_90,
  cast(MAP_RX_RUNOUT_DT  as date)    AS MAP_RX_RUNOUT_DT,
  cast(MAP_MED_RUNOUT_DT as date)    AS MAP_MED_RUNOUT_DT,
  datediff(cast(MAP_END_DT as date), cast(MAP_START_DT as date)) + 1 AS MAP_LEN_DAYS,
  MAP_DISCON_FLG,
  CASE WHEN upper(MAP_MED_CLASS) = 'STEROID' THEN 'yes' ELSE 'no' END AS IS_STEROID
FROM ${work}.MAP_STACKED
WHERE cast(PATID as string) IN (SELECT PATID FROM journey_patids)
ORDER BY PATID, MAP_START_DT, MEDICATION, EPISODE_NUM;

-- COMMAND ----------

-- MAGIC %md
-- MAGIC ## Cell 4 - LAYER 1: mapped raw claims  (`MMA_MED_PROCESSED`)
-- MAGIC The per-claim medication layer that FEEDS the MAP algorithm: each row is a
-- MAGIC claim (one code, one date, one claim type) already mapped to a MM med via
-- MAGIC the codelist, deduped to one row per (patient, med, date, claim type).
-- MAGIC This is the "even more raw than the workbook" layer - sort by date to see
-- MAGIC exactly what claims grouped into each MAP above.

-- COMMAND ----------

SELECT
  cast(PATID as string)  AS PATID,
  DATE_SERVICE,
  CLAIM_TYPE,            -- pharmacy (rx NDC) or medical (HCPCS / bill-proc / med NDC)
  MED_ABBR              AS MEDICATION,
  MED_CLASS             AS MEDICATION_CLASS,
  CODE,
  CODE_TYPE,            -- NDC or HCPCS
  DAY_SUPPLY,
  date_add(cast(DATE_SERVICE as date), cast(DAY_SUPPLY as int) - 1) AS COVERAGE_THROUGH
FROM ${work}.MMA_MED_PROCESSED
WHERE cast(PATID as string) IN (SELECT PATID FROM journey_patids)
ORDER BY PATID, DATE_SERVICE, MEDICATION, CLAIM_TYPE;

-- COMMAND ----------

-- MAGIC %md
-- MAGIC ## Cell 5 (optional) - LAYER 0: the absolutely-raw CDM claims (pre-dedup)
-- MAGIC The individual `medical` / `rx` rows before de-duplication, for the target
-- MAGIC patients, restricted to the codes the pipeline already identified for them
-- MAGIC (from `MMA_MED_PROCESSED`) and to each patient's observation window. Shows
-- MAGIC same-day / duplicate raw claims that collapse into one row in Layer 1.
-- MAGIC Uses the quarterly CDM tables (`t_<table>_${quarter}`).

-- COMMAND ----------

WITH pats AS (SELECT PATID FROM journey_patids),
win AS (   -- observation window per patient (bounds the raw pull like the pipeline)
  SELECT cast(PATID as string) AS PATID,
         cast(INDEX_DATE as date) AS INDEX_DATE,
         cast(OBS_END_DT as date) AS OBS_END_DT
  FROM ${work}.ELIG_COH_FINAL
  WHERE cast(PATID as string) IN (SELECT PATID FROM pats)
),
codes AS (   -- codes the pipeline mapped for these patients (NDC padded to 11)
  SELECT DISTINCT
    cast(PATID as string) AS PATID,
    CODE_TYPE,
    CASE WHEN CODE_TYPE = 'NDC'
         THEN lpad(regexp_replace(CODE, '[^0-9]', ''), 11, '0')
         ELSE upper(regexp_replace(CODE, '[^A-Za-z0-9]', '')) END AS CODE_N,
    MED_ABBR, MED_CLASS
  FROM ${work}.MMA_MED_PROCESSED
  WHERE cast(PATID as string) IN (SELECT PATID FROM pats)
),
-- one row per (claim, code-field), mirroring the pipeline's three medical
-- sources (PROC_CD / BILL_PROC_CD as HCPCS, NDC as NDC) so each code can match
med_raw AS (
  SELECT cast(m.PATID as string) AS PATID, cast(m.FST_DT as date) AS CLAIM_DT,
         'medical' AS CLAIM_TYPE, cast(m.PROC_CD as string) AS RAW_CODE_RAW,
         upper(regexp_replace(cast(m.PROC_CD as string), '[^A-Za-z0-9]', '')) AS CODE_N,
         'HCPCS' AS CODE_TYPE
  FROM ${cdm}.t_medical_${quarter} m
  WHERE cast(m.PATID as string) IN (SELECT PATID FROM pats)
    AND m.PROC_CD IS NOT NULL AND trim(cast(m.PROC_CD as string)) <> ''
  UNION ALL
  SELECT cast(m.PATID as string), cast(m.FST_DT as date),
         'medical', cast(m.BILL_PROC_CD as string),
         upper(regexp_replace(cast(m.BILL_PROC_CD as string), '[^A-Za-z0-9]', '')),
         'HCPCS'
  FROM ${cdm}.t_medical_${quarter} m
  WHERE cast(m.PATID as string) IN (SELECT PATID FROM pats)
    AND m.BILL_PROC_CD IS NOT NULL AND trim(cast(m.BILL_PROC_CD as string)) <> ''
  UNION ALL
  SELECT cast(m.PATID as string), cast(m.FST_DT as date),
         'medical', cast(m.NDC as string),
         lpad(regexp_replace(cast(m.NDC as string), '[^0-9]', ''), 11, '0'),
         'NDC'
  FROM ${cdm}.t_medical_${quarter} m
  WHERE cast(m.PATID as string) IN (SELECT PATID FROM pats)
    AND m.NDC IS NOT NULL AND trim(cast(m.NDC as string)) <> ''
),
rx_raw AS (
  SELECT cast(r.PATID as string) AS PATID, cast(r.FILL_DT as date) AS CLAIM_DT,
         'pharmacy' AS CLAIM_TYPE,
         cast(r.NDC as string) AS RAW_CODE_RAW,
         lpad(regexp_replace(coalesce(cast(r.NDC as string), ''), '[^0-9]', ''), 11, '0') AS CODE_N,
         'NDC' AS CODE_TYPE,
         cast(r.DAYS_SUP as int) AS DAYS_SUP
  FROM ${cdm}.t_rx_${quarter} r
  WHERE cast(r.PATID as string) IN (SELECT PATID FROM pats)
)
SELECT mr.PATID, mr.CLAIM_DT, mr.CLAIM_TYPE, mr.RAW_CODE_RAW AS RAW_CODE,
       mr.CODE_TYPE, c.MED_ABBR AS MEDICATION, c.MED_CLASS AS MEDICATION_CLASS,
       cast(NULL as int) AS DAYS_SUP
FROM med_raw mr
JOIN codes c ON c.PATID = mr.PATID AND c.CODE_TYPE = mr.CODE_TYPE AND c.CODE_N = mr.CODE_N
JOIN win w  ON w.PATID = mr.PATID AND mr.CLAIM_DT BETWEEN w.INDEX_DATE AND w.OBS_END_DT
UNION ALL
SELECT rr.PATID, rr.CLAIM_DT, rr.CLAIM_TYPE, rr.RAW_CODE_RAW AS RAW_CODE,
       rr.CODE_TYPE, c.MED_ABBR AS MEDICATION, c.MED_CLASS AS MEDICATION_CLASS,
       rr.DAYS_SUP
FROM rx_raw rr
JOIN codes c ON c.PATID = rr.PATID AND c.CODE_TYPE = rr.CODE_TYPE AND c.CODE_N = rr.CODE_N
JOIN win w  ON w.PATID = rr.PATID AND rr.CLAIM_DT BETWEEN w.INDEX_DATE AND w.OBS_END_DT
ORDER BY PATID, CLAIM_DT, MEDICATION, CLAIM_TYPE;
