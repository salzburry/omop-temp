#!/usr/bin/env Rscript
# LOT follow-up questions -> one Excel workbook (one tab per question + a Read Me).
#
#   Rscript lot_followup_qs.R
#
# A sibling of lot1_studyteam_qs.R / poma_studyteam_qs.R. It answers the study
# team's follow-ups on steroids, regimen mix and CAR-T, reading the MM tables on
# Databricks. It reuses the shared CAR-T helpers in R/validation_qs.R.
#
# Cohort: the NDMM study cohort by default (NDMM_LOT_LONG_FILT), which the study
# team asked to see first. Set LOT_COHORT=FULL for the whole LOT cohort. Every
# query reads this one table. MAP_STACKED and LOT1_SCT are shared, joined by PATID.
#
# Questions:
#   Q1  Steroids: show the LOT already leaves steroids out of every rule, with an
#       audit that no known steroid token is in any regimen, plus a note on where
#       steroids still surface (the display / timing outputs, and possibly the
#       mapped medication data, depending on the production codelist).
#   Q2  Among 1L DARA+BORT patients (just those two agents), how far apart are the
#       two start dates - same day, or one then the other?
#   Q3  Share of patients on LENA+DARA in 1L and 2L (exact pair, plus a wider
#       "contains both" count).
#   Q4  Melphalan in 2L by start year - was it phased out after 2017?
#   Q5  CAR-T: the CAR-T-vs-LOT1 table plus plain answers to the five CAR-T
#       questions.
#   D1-D3  Deep-dives on the study team's concerns: is Melphalan-in-2L really
#       transplant conditioning (D1); top-15 1L/2L regimens with LENA+DARA in
#       context (D2); raw-claim journeys behind the DARA+BORT same-day result (D3).
#
# Writes no permanent tables (only session temp views). A run writes one Excel
# workbook, a log, and the output folder if missing, and may set env defaults
# from pipeline_inputs.csv. Safe to run any time.

.script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0)
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  getwd()
})

source_dir <- file.path(.script_dir, "R")
if (file.exists(file.path(source_dir, "load_inputs.R"))) {
  source(file.path(source_dir, "load_inputs.R"))
  load_pipeline_inputs(c(.script_dir, dirname(.script_dir)))
}
source(file.path(source_dir, "config_lot.R"))
source(file.path(source_dir, "db_utils_lot.R"))
source(file.path(source_dir, "codelists_lot.R"))        # load_codelist_csv
source(file.path(source_dir, "validation_qs.R"))        # vqs_* helpers (shared)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ===========================================================================
# Writes the workbook with openxlsx. A "sheet" is a list of name, title,
# optional subtitle, narrative lines, and named tables (each a data.frame, or a
# list of caption + df). openxlsx must be installed; main() checks that first.
# ===========================================================================
wbx_write_workbook <- function(sheets, xlsx_path) {
  ox <- function(f) getExportedValue("openxlsx", f)
  wb <- ox("createWorkbook")()
  st_title <- ox("createStyle")(fontSize = 14, textDecoration = "bold",
                                fontColour = "#FFFFFF", fgFill = "#1F3864")
  st_sub   <- ox("createStyle")(fontColour = "#FFFFFF", fgFill = "#2E5496",
                                textDecoration = "italic")
  st_narr  <- ox("createStyle")(wrapText = TRUE, valign = "top")
  st_cap   <- ox("createStyle")(textDecoration = "bold", fgFill = "#D6E0F0")
  st_hdr   <- ox("createStyle")(textDecoration = "bold", fontColour = "#FFFFFF",
                                fgFill = "#2E5496", border = "TopBottomLeftRight",
                                halign = "left")
  for (s in sheets) {
    sn <- substr(gsub("[\\/?*:\\[\\]]", "", s$name), 1, 31)
    ox("addWorksheet")(wb, sn)
    r <- 1L
    ox("writeData")(wb, sn, s$title, startRow = r, startCol = 1)
    ox("addStyle")(wb, sn, st_title, rows = r, cols = 1:10, gridExpand = TRUE)
    r <- r + 1L
    if (!is.null(s$subtitle)) {
      ox("writeData")(wb, sn, s$subtitle, startRow = r, startCol = 1)
      ox("addStyle")(wb, sn, st_sub, rows = r, cols = 1:10, gridExpand = TRUE)
      r <- r + 1L
    }
    r <- r + 1L
    for (line in s$narrative %||% character()) {
      ox("writeData")(wb, sn, line, startRow = r, startCol = 1)
      ox("addStyle")(wb, sn, st_narr, rows = r, cols = 1, gridExpand = TRUE)
      r <- r + 1L
    }
    r <- r + 1L
    for (nm in names(s$tables %||% list())) {
      entry <- s$tables[[nm]]
      cap <- nm; df <- entry
      if (is.list(entry) && !is.data.frame(entry)) { cap <- entry$caption %||% nm; df <- entry$df }
      ox("writeData")(wb, sn, cap, startRow = r, startCol = 1)
      ox("addStyle")(wb, sn, st_cap, rows = r, cols = 1:10, gridExpand = TRUE)
      r <- r + 1L
      if (is.data.frame(df) && nrow(df) > 0) {
        ox("writeData")(wb, sn, df, startRow = r, startCol = 1,
                        headerStyle = st_hdr, withFilter = FALSE)
        r <- r + nrow(df) + 2L
      } else {
        ox("writeData")(wb, sn, "(no rows / not available)", startRow = r, startCol = 1)
        r <- r + 2L
      }
    }
    ox("setColWidths")(wb, sn, cols = 1:14, widths = "auto")
  }
  ox("saveWorkbook")(wb, xlsx_path, overwrite = TRUE)
  log_msg("wrote workbook -> ", xlsx_path, " (", length(sheets), " sheets)")
  invisible(TRUE)
}

# Run a pull and return its data, or a one-row "status" table naming the failure.
# This keeps a visible "unavailable" row in the workbook instead of a silent gap
# (a NULL would just drop out). is_status_table() later spots these status rows.
best_effort <- function(expr, label) {
  r <- tryCatch(expr, error = function(e) {
    log_msg("  NOTE: '", label, "' unavailable - ", conditionMessage(e))
    data.frame(status = sprintf("'%s' unavailable: %s", label, conditionMessage(e)),
               stringsAsFactors = FALSE)
  })
  if (is.null(r))
    data.frame(status = sprintf("'%s' returned no data (unavailable this run)", label),
               stringsAsFactors = FALSE)
  else r
}

# TRUE if x is a best_effort() failure row (one column named "status") - i.e. a
# "could not build" placeholder rather than a real answer table.
is_status_table <- function(x)
  is.data.frame(x) && identical(names(x), "status")

num <- function(x) suppressWarnings(as.numeric(x))
pct1 <- function(x, d) if (isTRUE(num(d) > 0)) round(100 * num(x) / num(d), 1) else NA_real_

# ---------------------------------------------------------------------------
# Look up the drug short-codes (DARA/BORT/LENA/MELP) from cl_mma_codelist.csv by
# full drug name, so a code change in the codelist does not quietly break a count.
# ---------------------------------------------------------------------------
resolve_lot_tokens <- function(con) {
  # resolved = TRUE only when the codes came from the codelist. If we fall back to
  # the standard defaults, the caller flags it (any token-based answer - Q2/Q3/Q4
  # and D1-D3 - could miscount if the real codes differ).
  out <- list(dara = "DARA", bort = "BORT", lena = "LENA", melp = "MELP",
              notes = character(0), resolved = FALSE)
  ok <- tryCatch({ vqs_build_mma_codelist(con); TRUE }, error = function(e) FALSE)
  if (!ok) {
    out$notes <- "mma_codelist unavailable; using default tokens (DARA/BORT/LENA/MELP)."
    return(out)
  }
  fell_back <- character()
  # pick() returns the code from the codelist, or the default if the drug name
  # is not found - noting each miss so even one fallback marks the run incomplete.
  pick <- function(name, like, dflt) {
    df <- tryCatch(db_q(con, glue("
      SELECT CL_MED_ABBR, count(*) AS n
      FROM mma_codelist
      WHERE lower(CL_MEDICATION_FULL) LIKE '%{like}%'
      GROUP BY CL_MED_ABBR ORDER BY n DESC")), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) { fell_back <<- c(fell_back, name); return(dflt) }
    toupper(trimws(df$CL_MED_ABBR[1]))
  }
  out$dara <- pick("DARA", "daratumumab", "DARA")
  out$bort <- pick("BORT", "bortezomib",  "BORT")
  out$lena <- pick("LENA", "lenalidomid", "LENA")
  out$melp <- pick("MELP", "melphalan",   "MELP")
  out$resolved <- length(fell_back) == 0
  if (length(fell_back))
    out$notes <- sprintf("Some tokens not found in cl_mma_codelist.csv (fell back to defaults for: %s). Tokens: DARA=%s, BORT=%s, LENA=%s, MELP=%s.",
                         paste(fell_back, collapse = ", "), out$dara, out$bort, out$lena, out$melp)
  else out$notes <- sprintf("Resolved tokens from cl_mma_codelist.csv: DARA=%s, BORT=%s, LENA=%s, MELP=%s.",
                       out$dara, out$bort, out$lena, out$melp)
  out
}

# The drug codes in a regimen string, dropping blanks. The engine already keeps
# steroids out of LOT_BASE_MEDS, so this is the list of MM agents.
MEDS_ARR <- "filter(split(LOT_BASE_MEDS, ' '), x -> length(x) > 0)"

# The steroid short-codes used across the repo's codelists (dashboard, steroid_
# codes.csv, engine rollup). Q1 checks regimens against this fixed list rather
# than the MAP steroid class, which may be empty for cl_mma_codelist.csv; the
# token table backs it up by showing every code that does appear.
STEROID_TOKENS <- c("DEX", "DEXA", "DEXAMETHASONE", "DEXAMETH",
                    "PRED", "PREDNISONE", "PREDNISOLONE",
                    "METHYLPRED", "METHYLPREDNISOLONE", "MPRED")

# ===========================================================================
# Q1 - steroids are already left out of the LOT rules. Two tables:
#   - audit: does any steroid code show up in a regimen? (should be 0)
#   - token list: every drug code that does appear, so a reader can see the
#     agents and confirm none is a steroid.
# ===========================================================================
q1_steroid_audit <- function(con, lot_long) {
  ster_arr <- paste(sprintf("'%s'", STEROID_TOKENS), collapse = ", ")

  audit <- best_effort(db_q(con, glue("
    WITH ll AS (
      SELECT {MEDS_ARR} AS meds
      FROM {lot_long}
      WHERE LOT_BASE_MEDS IS NOT NULL AND trim(LOT_BASE_MEDS) <> ''
    )
    SELECT count(*)                                                              AS n_lot_regimen_rows,
           sum(CASE WHEN size(array_intersect(meds, array({ster_arr}))) > 0
                    THEN 1 ELSE 0 END)                                           AS n_rows_with_steroid_token
    FROM ll")), "steroid-in-regimen audit")

  vocab <- best_effort(db_q(con, glue("
    WITH base AS (
      SELECT cast(PATID as string) AS PATID, {MEDS_ARR} AS meds
      FROM {lot_long}
      WHERE LOT_BASE_MEDS IS NOT NULL AND trim(LOT_BASE_MEDS) <> ''
    )
    SELECT upper(tok)             AS agent_token,
           count(*)              AS n_regimen_rows,
           count(DISTINCT PATID) AS n_patients,
           CASE WHEN array_contains(array({ster_arr}), upper(tok))
                THEN 'steroid - not expected here' ELSE '' END AS note
    FROM base LATERAL VIEW explode(meds) t AS tok
    GROUP BY upper(tok) ORDER BY n_patients DESC")), "LOT regimen token vocabulary")

  list(audit = audit, vocab = vocab)
}

# ===========================================================================
# Q2 - 1L DARA+BORT: how far apart are the two start dates?
# We look at LOT1 patients whose regimen is just DARA and BORT. Each agent's
# start is its first MAP segment inside the LOT1 induction window (the window
# that decides the regimen). gap = BORT start - DARA start, in days:
# >0 = DARA first, <0 = BORT first, 0 = same day.
# ===========================================================================
q2_dara_bort_gap <- function(con, lot_long, map_tbl, dara, bort, w1) {
  base_cte <- glue("
    WITH l1 AS (
      SELECT cast(PATID as string) AS PATID, cast(LOT_START_DT as date) AS L1,
             {MEDS_ARR} AS meds
      FROM {lot_long}
      WHERE LOT_NUM = 1 AND LOT_BASE_MEDS IS NOT NULL AND trim(LOT_BASE_MEDS) <> ''
    ),
    dual AS (
      SELECT PATID, L1 FROM l1
      WHERE size(meds) = 2 AND array_contains(meds, '{dara}') AND array_contains(meds, '{bort}')
    ),
    dara_dt AS (
      SELECT cast(m.PATID as string) AS PATID, min(cast(m.MAP_START_DT as date)) AS d_dara
      FROM {map_tbl} m JOIN dual d ON cast(m.PATID as string) = d.PATID
      WHERE upper(trim(m.MAP_MED_TYPE)) = '{dara}'
        AND cast(m.MAP_START_DT as date) BETWEEN d.L1 AND date_add(d.L1, {w1} - 1)
      GROUP BY cast(m.PATID as string)
    ),
    bort_dt AS (
      SELECT cast(m.PATID as string) AS PATID, min(cast(m.MAP_START_DT as date)) AS d_bort
      FROM {map_tbl} m JOIN dual d ON cast(m.PATID as string) = d.PATID
      WHERE upper(trim(m.MAP_MED_TYPE)) = '{bort}'
        AND cast(m.MAP_START_DT as date) BETWEEN d.L1 AND date_add(d.L1, {w1} - 1)
      GROUP BY cast(m.PATID as string)
    ),
    g AS (
      SELECT d.PATID,
             datediff(bd.d_bort, dd.d_dara)      AS gap_bort_minus_dara,
             abs(datediff(bd.d_bort, dd.d_dara)) AS abs_gap
      FROM dual d JOIN dara_dt dd USING (PATID) JOIN bort_dt bd USING (PATID)
    )")

  # Count the DARA+BORT patients in a separate query so no single SELECT mixes a
  # sub-query with aggregates (safer across Spark).
  n_dual <- num(db_q(con, glue("{base_cte} SELECT count(*) AS n FROM dual"))$n[1])

  summ <- db_q(con, glue("{base_cte}
    SELECT
      count(*)                                                     AS n_with_both_start_dates,
      sum(CASE WHEN abs_gap = 0 THEN 1 ELSE 0 END)                 AS n_same_day,
      sum(CASE WHEN gap_bort_minus_dara > 0 THEN 1 ELSE 0 END)     AS n_dara_first,
      sum(CASE WHEN gap_bort_minus_dara < 0 THEN 1 ELSE 0 END)     AS n_bort_first,
      round(avg(abs_gap), 1)                                       AS mean_abs_gap_days,
      percentile_approx(abs_gap, 0.5)                              AS median_abs_gap_days,
      percentile_approx(abs_gap, 0.25)                             AS p25_abs_gap_days,
      percentile_approx(abs_gap, 0.75)                             AS p75_abs_gap_days,
      percentile_approx(abs_gap, 0.9)                              AS p90_abs_gap_days,
      max(abs_gap)                                                 AS max_abs_gap_days
    FROM g"))

  buckets <- db_q(con, glue("{base_cte}
    SELECT
      sum(CASE WHEN abs_gap = 0            THEN 1 ELSE 0 END) AS d_same_day,
      sum(CASE WHEN abs_gap BETWEEN 1 AND 7   THEN 1 ELSE 0 END) AS d_1_7,
      sum(CASE WHEN abs_gap BETWEEN 8 AND 30  THEN 1 ELSE 0 END) AS d_8_30,
      sum(CASE WHEN abs_gap > 30              THEN 1 ELSE 0 END) AS d_gt_30
    FROM g"))

  n_both <- num(summ$n_with_both_start_dates[1])

  overview <- data.frame(
    metric = c(
      "1L DARA+BORT dual-therapy patients (denominator)",
      "  ... with a start date for both agents in the induction window",
      "Started on the same day",
      "DARA started first (BORT added later)",
      "BORT started first (DARA added later)",
      "Mean absolute gap (days)",
      "Median absolute gap (days)",
      "25th percentile absolute gap (days)",
      "75th percentile absolute gap (days)",
      "90th percentile absolute gap (days)",
      "Max absolute gap (days)"),
    value = c(
      as.integer(n_dual), as.integer(n_both),
      as.integer(num(summ$n_same_day[1])),
      as.integer(num(summ$n_dara_first[1])),
      as.integer(num(summ$n_bort_first[1])),
      num(summ$mean_abs_gap_days[1]),
      num(summ$median_abs_gap_days[1]),
      num(summ$p25_abs_gap_days[1]),
      num(summ$p75_abs_gap_days[1]),
      num(summ$p90_abs_gap_days[1]),
      num(summ$max_abs_gap_days[1])),
    pct_of_dual = c(
      NA_real_, pct1(n_both, n_dual),
      pct1(summ$n_same_day[1],  n_dual), pct1(summ$n_dara_first[1], n_dual),
      pct1(summ$n_bort_first[1], n_dual),
      NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE)

  dist <- data.frame(
    start_date_gap = c("Same day (0)", "1-7 days", "8-30 days", "more than 30 days"),
    n_patients = as.integer(c(num(buckets$d_same_day[1]), num(buckets$d_1_7[1]),
                              num(buckets$d_8_30[1]), num(buckets$d_gt_30[1]))),
    stringsAsFactors = FALSE)
  dist$pct_of_pairs <- vapply(dist$n_patients, function(x) pct1(x, n_both), numeric(1))

  list(overview = overview, distribution = dist, n_dual = n_dual, n_both = n_both)
}

# ===========================================================================
# Q3 - share on LENA+DARA in 1L and 2L. "exact pair" = the regimen is just DARA
# and LENA; "contains both" = both are there, maybe with other agents too.
# ===========================================================================
q3_lena_dara <- function(con, lot_long, dara, lena) {
  # Denominator = every patient reaching each line, including transplant-only
  # lines with an empty regimen, so it matches the Read Me's per-line counts. An
  # empty regimen never matches the DARA/LENA test, so it adds to the denominator
  # but not the numerator.
  r <- db_q(con, glue("
    WITH l AS (
      SELECT LOT_NUM, cast(PATID as string) AS PATID, {MEDS_ARR} AS meds
      FROM {lot_long}
      WHERE LOT_NUM IN (1, 2)
    )
    SELECT LOT_NUM,
           count(DISTINCT PATID) AS n_lot_patients,
           count(DISTINCT CASE WHEN size(meds) = 2 AND array_contains(meds, '{dara}')
                                AND array_contains(meds, '{lena}') THEN PATID END) AS n_dara_lena_dual,
           count(DISTINCT CASE WHEN array_contains(meds, '{dara}')
                                AND array_contains(meds, '{lena}') THEN PATID END) AS n_contains_both_any
    FROM l GROUP BY LOT_NUM ORDER BY LOT_NUM"))
  if (nrow(r) == 0) return(data.frame(status = "No LOT1/LOT2 regimen rows found.", stringsAsFactors = FALSE))
  data.frame(
    line = paste0("LOT", r$LOT_NUM),
    n_line_patients          = as.integer(num(r$n_lot_patients)),
    n_dara_lena_dual         = as.integer(num(r$n_dara_lena_dual)),
    pct_dara_lena_dual       = mapply(pct1, r$n_dara_lena_dual, r$n_lot_patients),
    n_contains_dara_and_lena = as.integer(num(r$n_contains_both_any)),
    pct_contains_both_any    = mapply(pct1, r$n_contains_both_any, r$n_lot_patients),
    stringsAsFactors = FALSE)
}

# ===========================================================================
# Q4 - Melphalan in 2L over time: MELP share by LOT2 start year, a before/after-
# 2017 summary, and the most common MELP-containing 2L regimens.
# ===========================================================================
q4_melp_2l <- function(con, lot_long, melp) {
  # Denominator = all 2L lines that year (any start type, incl. transplant-only
  # lines with an empty regimen), so pct_melp is the share of all 2L patients on
  # MELP. MELP only shows up in drug regimens, so empty ones add to the
  # denominator but not the MELP count.
  by_year <- db_q(con, glue("
    WITH l2 AS (
      SELECT year(cast(LOT_START_DT as date)) AS yr, {MEDS_ARR} AS meds
      FROM {lot_long}
      WHERE LOT_NUM = 2 AND LOT_START_DT IS NOT NULL
    )
    SELECT yr AS lot2_start_year,
           count(*)                                                    AS n_lot2_total,
           sum(CASE WHEN array_contains(meds, '{melp}') THEN 1 ELSE 0 END) AS n_lot2_with_melp,
           round(100.0 * sum(CASE WHEN array_contains(meds, '{melp}') THEN 1 ELSE 0 END)
                 / count(*), 1)                                        AS pct_melp
    FROM l2 GROUP BY yr ORDER BY yr"))

  era <- db_q(con, glue("
    WITH l2 AS (
      SELECT year(cast(LOT_START_DT as date)) AS yr, {MEDS_ARR} AS meds
      FROM {lot_long}
      WHERE LOT_NUM = 2 AND LOT_START_DT IS NOT NULL
    ),
    melp AS (SELECT yr FROM l2 WHERE array_contains(meds, '{melp}'))
    SELECT
      (SELECT count(*) FROM melp)                                    AS n_lot2_with_melp_total,
      (SELECT count(*) FROM melp WHERE yr <= 2017)                   AS n_melp_2017_and_earlier,
      (SELECT count(*) FROM melp WHERE yr >= 2018)                   AS n_melp_2018_and_later,
      (SELECT percentile_approx(yr, 0.5) FROM melp)                  AS median_melp_lot2_year,
      (SELECT count(*) FROM l2)                                      AS n_lot2_total"))
  n_melp <- num(era$n_lot2_with_melp_total[1])
  era_df <- data.frame(
    metric = c(
      "MELP-containing 2L regimens (all years)",
      "  ... with LOT2 starting in 2017 or earlier",
      "  ... with LOT2 starting in 2018 or later",
      "Median LOT2 start year among MELP-containing 2L regimens",
      "All 2L lines (all start types), all years"),
    value = c(as.integer(n_melp),
              as.integer(num(era$n_melp_2017_and_earlier[1])),
              as.integer(num(era$n_melp_2018_and_later[1])),
              as.integer(num(era$median_melp_lot2_year[1])),
              as.integer(num(era$n_lot2_total[1]))),
    pct_of_melp = c(NA_real_,
                    pct1(era$n_melp_2017_and_earlier[1], n_melp),
                    pct1(era$n_melp_2018_and_later[1], n_melp),
                    NA_real_, NA_real_),
    stringsAsFactors = FALSE)

  top_reg <- db_q(con, glue("
    WITH l2 AS (
      SELECT cast(PATID as string) AS PATID, LOT_BASE_MEDS, {MEDS_ARR} AS meds
      FROM {lot_long}
      WHERE LOT_NUM = 2 AND LOT_BASE_MEDS IS NOT NULL AND trim(LOT_BASE_MEDS) <> ''
    )
    SELECT LOT_BASE_MEDS AS lot2_regimen, count(DISTINCT PATID) AS n_patients
    FROM l2 WHERE array_contains(meds, '{melp}')
    GROUP BY LOT_BASE_MEDS ORDER BY n_patients DESC LIMIT 10"))

  list(by_year = by_year, era = era_df, top_regimens = top_reg)
}

# ===========================================================================
# Q5 - answers to the five CAR-T questions, using the first CAR-T date from
# LOT1_SCT (always on/after LOT1 start) and the same during/closing window as
# vqs_q6_cart (up to the LOT1 end, plus one day when a CAR-T closed LOT1).
# w1 = LOT1 induction window.
# ===========================================================================
q5_cart_clarifications <- function(con, lot_long, sct_tbl, w1) {
  during_ub <- "CASE WHEN END_REASON IN ('SCT_CART','CART_INIT') THEN date_add(L1_END, 1) ELSE L1_END END"

  # (a)+(c)+(d): how the CAR-T windows overlap, in one query.
  rel <- db_q(con, glue("
    WITH l1 AS (
      SELECT cast(PATID as string) AS PATID, cast(LOT_START_DT as date) AS L1,
             cast(LOT_BASE_END_DT as date) AS L1_END, LOT_BASE_END_REASON AS END_REASON
      FROM {lot_long} WHERE LOT_NUM = 1
    ),
    sct AS (
      SELECT cast(PATID as string) AS PATID, min(cast(FIRST_CART_DT as date)) AS CART_DT
      FROM {sct_tbl} WHERE FIRST_CART_DT IS NOT NULL GROUP BY cast(PATID as string)
    ),
    j AS (
      SELECT l.PATID, l.L1, l.L1_END, l.END_REASON, s.CART_DT,
             CASE WHEN s.CART_DT IS NOT NULL AND s.CART_DT BETWEEN l.L1 AND date_add(l.L1, {w1} - 1)
                  THEN 1 ELSE 0 END AS in60,
             CASE WHEN s.CART_DT IS NOT NULL AND s.CART_DT BETWEEN l.L1 AND ({during_ub})
                  THEN 1 ELSE 0 END AS during
      FROM l1 l LEFT JOIN sct s ON s.PATID = l.PATID
    )
    SELECT
      sum(in60)                                                       AS n_within_60d_after,
      sum(during)                                                     AS n_during_or_closing,
      sum(CASE WHEN in60 = 1 AND during = 1 THEN 1 ELSE 0 END)        AS n_in_both,
      sum(CASE WHEN in60 = 1 AND during = 0 THEN 1 ELSE 0 END)        AS n_60d_not_during,
      sum(CASE WHEN in60 = 0 AND during = 1 THEN 1 ELSE 0 END)        AS n_during_not_60d,
      sum(CASE WHEN CART_DT IS NOT NULL THEN 1 ELSE 0 END)            AS n_any_cart_on_after_lot1,
      sum(CASE WHEN CART_DT IS NOT NULL AND during = 0 THEN 1 ELSE 0 END) AS n_cart_strictly_later,
      sum(CASE WHEN CART_DT = L1 THEN 1 ELSE 0 END)                   AS n_cart_on_lot1_start
    FROM j"))

  # (e): does the CAR-T become the 2L start date? For patients whose LOT1 ended by
  # CAR-T, check that LOT1 ends the day before the CAR-T and LOT2 starts on it.
  seq <- db_q(con, glue("
    WITH l1 AS (
      SELECT cast(PATID as string) AS PATID, cast(LOT_BASE_END_DT as date) AS L1_END,
             LOT_BASE_END_REASON AS END_REASON
      FROM {lot_long} WHERE LOT_NUM = 1
    ),
    l2 AS (
      SELECT cast(PATID as string) AS PATID, cast(LOT_START_DT as date) AS L2,
             LOT_START_TYPE AS L2_TYPE
      FROM {lot_long} WHERE LOT_NUM = 2
    ),
    sct AS (
      SELECT cast(PATID as string) AS PATID, min(cast(FIRST_CART_DT as date)) AS CART_DT
      FROM {sct_tbl} WHERE FIRST_CART_DT IS NOT NULL GROUP BY cast(PATID as string)
    ),
    e AS (
      SELECT l1.PATID, l1.L1_END, s.CART_DT, l2.L2, l2.L2_TYPE
      FROM l1 JOIN sct s ON s.PATID = l1.PATID
              LEFT JOIN l2 ON l2.PATID = l1.PATID
      WHERE l1.END_REASON IN ('SCT_CART','CART_INIT')
    )
    SELECT
      count(*)                                                             AS n_lot1_ended_by_cart,
      sum(CASE WHEN L1_END = date_sub(CART_DT, 1) THEN 1 ELSE 0 END)       AS n_lot1_ends_day_before_cart,
      sum(CASE WHEN L2 IS NOT NULL THEN 1 ELSE 0 END)                      AS n_with_a_lot2,
      sum(CASE WHEN L2 = CART_DT THEN 1 ELSE 0 END)                        AS n_lot2_starts_on_cart_date,
      sum(CASE WHEN L2_TYPE = 'CART' THEN 1 ELSE 0 END)                    AS n_lot2_start_type_cart
    FROM e"))

  na_i <- function(x) { v <- num(x); if (length(v) == 0 || is.na(v)) NA_integer_ else as.integer(v) }
  wd <- as.integer(w1)
  data.frame(
    check = c(
      sprintf("(a) CAR-T within %dd after LOT1 start", wd),
      "(a) CAR-T during or closing LOT1",
      "(a)   ... in both windows",
      sprintf("(a)   ... within-%dd but not during/closing (subset test: 0 means it is a subset)", wd),
      sprintf("(a)   ... during/closing but not within-%dd (later closing CAR-T)", wd),
      "(c) Any CAR-T on/after LOT1 start (distinct patients; incl. later lines)",
      "(c)   ... occurring strictly after LOT1 ends (i.e. on 2L+, not during LOT1)",
      "(d) CAR-T dated exactly on the LOT1 start date",
      "(e) LOT1 ended by CAR-T (SCT_CART / CART_INIT)",
      "(e)   ... LOT1 ends the day before the CAR-T (first CAR-T date - 1)",
      "(e)   ... has a LOT2 record",
      "(e)   ... whose LOT2 start date equals the CAR-T date",
      "(e)   ... whose LOT2 start type is 'CART'"),
    n_patients = c(
      na_i(rel$n_within_60d_after[1]), na_i(rel$n_during_or_closing[1]),
      na_i(rel$n_in_both[1]), na_i(rel$n_60d_not_during[1]), na_i(rel$n_during_not_60d[1]),
      na_i(rel$n_any_cart_on_after_lot1[1]), na_i(rel$n_cart_strictly_later[1]),
      na_i(rel$n_cart_on_lot1_start[1]),
      na_i(seq$n_lot1_ended_by_cart[1]), na_i(seq$n_lot1_ends_day_before_cart[1]),
      na_i(seq$n_with_a_lot2[1]), na_i(seq$n_lot2_starts_on_cart_date[1]),
      na_i(seq$n_lot2_start_type_cart[1])),
    stringsAsFactors = FALSE)
}

# ===========================================================================
# D1 - Melphalan in 2L: is it transplant conditioning?
# High-dose melphalan is the drug that conditions an autologous transplant. These
# tables report signals consistent with conditioning - not proof: how many MELP-
# in-2L lines carry an autologous transplant (LOT_TX_AUTO_FLG), how short the
# melphalan-only lines are (LOT_BASE_LENGTH), and how many days from the melphalan
# line start (LOT_START_DT, = the melphalan claim date for a MED-started line) to
# the transplant (LOT_TX_AUTO_DT_1). Melphalan a few days BEFORE the transplant is
# the conditioning pattern.
# ===========================================================================
q_melp_conditioning <- function(con, lot_long, melp) {
  base <- glue("
    WITH l2 AS (
      SELECT cast(PATID as string) AS PATID, LOT_START_TYPE,
             {MEDS_ARR} AS meds,
             cast(LOT_BASE_LENGTH as int) AS len,
             coalesce(LOT_TX_AUTO_FLG, 0) AS auto_flg,
             cast(LOT_START_DT as date) AS lot2_start,
             cast(LOT_TX_AUTO_DT_1 as date) AS auto_dt1
      FROM {lot_long}
      WHERE LOT_NUM = 2 AND LOT_BASE_MEDS IS NOT NULL AND trim(LOT_BASE_MEDS) <> ''
    ),
    m AS (SELECT * FROM l2 WHERE array_contains(meds, '{melp}'))")

  agg <- db_q(con, glue("{base}
    SELECT
      count(*)                                             AS n_melp_2l_lines,
      sum(CASE WHEN size(meds) = 1 THEN 1 ELSE 0 END)      AS n_melp_mono,
      sum(auto_flg)                                        AS n_with_transplant,
      sum(CASE WHEN size(meds) = 1 AND auto_flg = 1 THEN 1 ELSE 0 END) AS n_mono_with_transplant,
      sum(CASE WHEN LOT_START_TYPE = 'SCT_AUTO' THEN 1 ELSE 0 END)     AS n_start_type_auto
    FROM m"))
  n_melp <- num(agg$n_melp_2l_lines[1])
  signal <- data.frame(
    metric = c(
      "MELP-containing 2L lines (denominator)",
      "  ... melphalan monotherapy (regimen is MELP only)",
      "  ... with an autologous transplant flagged in the line",
      "  ... monotherapy with a transplant (consistent with conditioning)",
      "  ... start type = SCT_AUTO (transplant-started line)"),
    n = c(as.integer(n_melp),
          as.integer(num(agg$n_melp_mono[1])),
          as.integer(num(agg$n_with_transplant[1])),
          as.integer(num(agg$n_mono_with_transplant[1])),
          as.integer(num(agg$n_start_type_auto[1]))),
    pct_of_melp_2l = c(NA_real_,
          pct1(agg$n_melp_mono[1], n_melp), pct1(agg$n_with_transplant[1], n_melp),
          pct1(agg$n_mono_with_transplant[1], n_melp), pct1(agg$n_start_type_auto[1], n_melp)),
    stringsAsFactors = FALSE)

  len <- db_q(con, glue("{base}
    SELECT
      sum(CASE WHEN size(meds) = 1 THEN 1 ELSE 0 END)                  AS n_mono,
      percentile_approx(CASE WHEN size(meds)=1 THEN len END, 0.5)      AS median_len_days,
      percentile_approx(CASE WHEN size(meds)=1 THEN len END, 0.25)     AS p25_len_days,
      percentile_approx(CASE WHEN size(meds)=1 THEN len END, 0.75)     AS p75_len_days,
      sum(CASE WHEN size(meds)=1 AND len <= 7 THEN 1 ELSE 0 END)       AS n_le_7d
    FROM m"))
  n_mono <- num(len$n_mono[1])
  length_tbl <- data.frame(
    metric = c("MELP-monotherapy 2L lines", "Median line length (days)",
               "25th percentile length (days)", "75th percentile length (days)",
               "Lines lasting 7 days or less"),
    value = c(as.integer(n_mono), as.integer(num(len$median_len_days[1])),
              as.integer(num(len$p25_len_days[1])), as.integer(num(len$p75_len_days[1])),
              as.integer(num(len$n_le_7d[1]))),
    pct_of_mono = c(NA_real_, NA_real_, NA_real_, NA_real_, pct1(len$n_le_7d[1], n_mono)),
    stringsAsFactors = FALSE)

  # Days from the melphalan line start to the autologous transplant. Restricted to
  # MED-started lines (LOT_START_TYPE='MED'), where the line start IS the melphalan
  # claim date. SCT_AUTO-started lines are excluded, because there LOT_START_DT is
  # the transplant date itself (the gap would be ~0 by construction, not a real
  # melphalan-to-transplant measure). A small positive gap = melphalan a few days
  # before the transplant = the conditioning pattern.
  timing <- db_q(con, glue("{base},
    md AS (SELECT datediff(auto_dt1, lot2_start) AS gap
           FROM m WHERE size(meds) = 1 AND auto_flg = 1 AND auto_dt1 IS NOT NULL
             AND LOT_START_TYPE = 'MED')
    SELECT count(*)                                                  AS n_mono_with_dated_transplant,
           percentile_approx(gap, 0.5)                               AS median_days_melp_to_transplant,
           percentile_approx(gap, 0.25)                              AS p25_days,
           percentile_approx(gap, 0.75)                              AS p75_days,
           sum(CASE WHEN gap BETWEEN 0 AND 14 THEN 1 ELSE 0 END)     AS n_transplant_0_14d_after_melp
    FROM md"))
  n_dated <- num(timing$n_mono_with_dated_transplant[1])
  timing_tbl <- data.frame(
    metric = c("MED-started MELP-only lines with a dated transplant",
               "Median days from melphalan start to transplant",
               "25th percentile days", "75th percentile days",
               "Transplant 0-14 days after the melphalan start (conditioning pattern)"),
    value = c(as.integer(n_dated), as.integer(num(timing$median_days_melp_to_transplant[1])),
              as.integer(num(timing$p25_days[1])), as.integer(num(timing$p75_days[1])),
              as.integer(num(timing$n_transplant_0_14d_after_melp[1]))),
    pct_of_dated = c(NA_real_, NA_real_, NA_real_, NA_real_,
                     pct1(timing$n_transplant_0_14d_after_melp[1], n_dated)),
    stringsAsFactors = FALSE)

  by_type <- db_q(con, glue("{base}
    SELECT coalesce(LOT_START_TYPE, '(null)') AS lot2_start_type,
           count(*) AS n_lines,
           sum(auto_flg) AS n_with_transplant
    FROM m GROUP BY coalesce(LOT_START_TYPE, '(null)') ORDER BY n_lines DESC"))

  list(signal = signal, length = length_tbl, timing = timing_tbl, by_type = by_type)
}

# ===========================================================================
# D2 - Top regimens in 1L and 2L, so LENA+DARA can be seen in context. Ranked by
# distinct patients on each regimen string (steroids excluded, as everywhere).
# ===========================================================================
q_top_regimens <- function(con, lot_long, dara, lena, n_lot1, n_lot2, topn = 15L) {
  top_one <- function(lnum, denom) {
    d <- db_q(con, glue("
      WITH l AS (SELECT cast(PATID as string) AS PATID, LOT_BASE_MEDS, {MEDS_ARR} AS meds
                 FROM {lot_long}
                 WHERE LOT_NUM = {lnum} AND LOT_BASE_MEDS IS NOT NULL AND trim(LOT_BASE_MEDS) <> '')
      SELECT LOT_BASE_MEDS AS regimen, count(DISTINCT PATID) AS n_patients,
             max(CASE WHEN array_contains(meds, '{dara}') AND array_contains(meds, '{lena}')
                      THEN 1 ELSE 0 END) AS has_dara_lena
      FROM l GROUP BY LOT_BASE_MEDS ORDER BY n_patients DESC LIMIT {as.integer(topn)}"))
    if (nrow(d) == 0) return(data.frame(status = sprintf("No LOT%d regimens found.", lnum),
                                        stringsAsFactors = FALSE))
    data.frame(
      rank = seq_len(nrow(d)),
      regimen = d$regimen,
      n_patients = as.integer(num(d$n_patients)),
      pct_of_line = vapply(num(d$n_patients), function(x) pct1(x, denom), numeric(1)),
      contains_DARA_LENA = ifelse(num(d$has_dara_lena) == 1, "yes", ""),
      stringsAsFactors = FALSE)
  }
  list(lot1 = top_one(1L, n_lot1), lot2 = top_one(2L, n_lot2))
}

# ===========================================================================
# D3 - DARA+BORT journeys: for a few same-day and a few staggered dual patients,
# show each agent's MAP start dates and the DARA/BORT claims behind them, so the
# same-service-date result can be checked against the source data. The raw-claim
# pull is limited to DARA and BORT, and is written only when the observation
# window is known (bounds_available) - never an unbounded full claim history.
# ===========================================================================
q_dara_bort_examples <- function(con, lot_long, map_tbl, dara, bort, w1, bounds, bounds_available, n_each = 4L) {
  picks <- db_q(con, glue("
    WITH l1 AS (
      SELECT cast(PATID as string) AS PATID, cast(LOT_START_DT as date) AS L1, {MEDS_ARR} AS meds
      FROM {lot_long} WHERE LOT_NUM = 1 AND LOT_BASE_MEDS IS NOT NULL AND trim(LOT_BASE_MEDS) <> ''
    ),
    dual AS (SELECT PATID, L1 FROM l1
             WHERE size(meds) = 2 AND array_contains(meds, '{dara}') AND array_contains(meds, '{bort}')),
    dd AS (SELECT cast(m.PATID as string) AS PATID, min(cast(m.MAP_START_DT as date)) AS d_dara
           FROM {map_tbl} m JOIN dual d ON cast(m.PATID as string) = d.PATID
           WHERE upper(trim(m.MAP_MED_TYPE)) = '{dara}'
             AND cast(m.MAP_START_DT as date) BETWEEN d.L1 AND date_add(d.L1, {w1} - 1)
           GROUP BY cast(m.PATID as string)),
    bd AS (SELECT cast(m.PATID as string) AS PATID, min(cast(m.MAP_START_DT as date)) AS d_bort
           FROM {map_tbl} m JOIN dual d ON cast(m.PATID as string) = d.PATID
           WHERE upper(trim(m.MAP_MED_TYPE)) = '{bort}'
             AND cast(m.MAP_START_DT as date) BETWEEN d.L1 AND date_add(d.L1, {w1} - 1)
           GROUP BY cast(m.PATID as string)),
    g AS (SELECT d.PATID, cast(d.L1 as string) AS lot1_start,
                 cast(dd.d_dara as string) AS dara_start, cast(bd.d_bort as string) AS bort_start,
                 datediff(bd.d_bort, dd.d_dara) AS gap_bort_minus_dara,
                 abs(datediff(bd.d_bort, dd.d_dara)) AS abs_gap
          FROM dual d JOIN dd USING (PATID) JOIN bd USING (PATID))
    (SELECT 'same-day' AS grp, PATID, lot1_start, dara_start, bort_start, gap_bort_minus_dara
       FROM g WHERE abs_gap = 0 ORDER BY PATID LIMIT {as.integer(n_each)})
    UNION ALL
    (SELECT 'staggered' AS grp, PATID, lot1_start, dara_start, bort_start, gap_bort_minus_dara
       FROM g WHERE abs_gap > 0 ORDER BY abs_gap DESC, PATID LIMIT {as.integer(n_each)})"))
  ids <- unique(as.character(picks$PATID))
  out <- list(picks = picks)
  if (length(ids) == 0) { out$note <- "No DARA+BORT dual patients to show."; return(out) }
  only_db <- function(df) {   # keep only DARA/BORT rows (skip a status table)
    if (is.data.frame(df) && "MED_ABBR" %in% names(df))
      df[toupper(trimws(df$MED_ABBR)) %in% c(dara, bort), , drop = FALSE] else df
  }
  out$journey <- only_db(best_effort(vqs_map_journey(con, map_tbl, lot_long, ids), "MAP segments"))
  # Raw claims are patient-level, so pull them only when the observation window is
  # known, and keep just DARA/BORT. Otherwise skip the raw table (the picks table
  # already shows each agent's start date).
  # (name is skip_raw, not raw_skipped, so out$raw cannot partial-match it)
  if (isTRUE(bounds_available))
    out$raw <- only_db(best_effort(vqs_raw_mma_claims(con, ids, bounds = bounds), "raw DARA/BORT claims"))
  else
    out$skip_raw <- TRUE
  out
}

# ===========================================================================
main <- function() {
  stop_if_blank(cfg$pwd, "DATABRICKS_PWD environment variable is not set.")

  # The output is Excel, so check for openxlsx up front (before any warehouse
  # work) and stop with an install hint if it is missing.
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("openxlsx is required to build the Excel workbook. Install it with ",
         "install.packages('openxlsx') and re-run.")

  con <- DBI::dbConnect(odbc::odbc(), dsn = cfg$dsn, pwd = cfg$pwd, timeout = 120)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  out_dir <- cfg$output_dir
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # ---- Cohort selection --------------------------------------------------
  # NDMM study cohort by default (the study team asked to see it first);
  # LOT_COHORT=FULL uses the whole LOT cohort.
  cohort_mode <- toupper(Sys.getenv("LOT_COHORT", unset = "NDMM"))
  if (cohort_mode == "FULL") {
    lot_long <- wrk("LOT_LONG")
    cohort_label <- "full LOT cohort (all LOT1 patients)"
  } else {
    cohort_mode <- "NDMM"
    lot_long <- wrk("NDMM_LOT_LONG_FILT")
    cohort_label <- "NDMM newly-diagnosed 1L study cohort"
  }
  map_tbl <- wrk("MAP_STACKED")
  sct_tbl <- wrk("LOT1_SCT")

  log_msg(SEP); log_msg("LOT follow-up study-team questions [", cohort_label, "] -> single Excel workbook"); log_msg(SEP)
  if (!vqs_readable(con, lot_long)) {
    if (cohort_mode == "NDMM")
      stop("Cannot read ", lot_long, ". Run 06_ndmm_dashboard.R first to persist ",
           "NDMM_LOT_LONG_FILT (LOT_LONG restricted to the NDMM study cohort), or set ",
           "LOT_COHORT=FULL to run on LOT_LONG.")
    stop("Cannot read ", lot_long, ". Build the LOT pipeline (02_lot1.R / 03_lot2_5.R) first.")
  }
  have_map <- vqs_readable(con, map_tbl)
  have_sct <- vqs_readable(con, sct_tbl)
  if (!have_map) log_msg("WARNING: ", map_tbl, " not readable - Q2 (DARA/BORT start dates) cannot be built.")
  if (!have_sct) log_msg("WARNING: ", sct_tbl, " not readable - Q5 (CAR-T) cannot be built.")

  tok <- resolve_lot_tokens(con)
  log_msg(paste(tok$notes, collapse = " "))
  bounds   <- vqs_obs_bounds_src(con)
  cart_raw <- if (have_sct) tryCatch(vqs_build_raw_cart_dates(con, lot_long, bounds$sql),
                                     error = function(e) NULL) else NULL

  n_lot1 <- num(db_q(con, glue(
    "SELECT count(DISTINCT PATID) n FROM {lot_long} WHERE LOT_NUM = 1"))$n)
  n_lot2 <- num(db_q(con, glue(
    "SELECT count(DISTINCT PATID) n FROM {lot_long} WHERE LOT_NUM = 2"))$n)
  log_msg(sprintf("Cohort denominators: LOT1 = %s patients, LOT2 = %s patients.",
                  format(n_lot1, big.mark = ","), format(n_lot2, big.mark = ",")))

  sheets <- list()
  add_sheet <- function(...) sheets[[length(sheets) + 1L]] <<- list(...)

  # Extra reasons a run counts as incomplete, on top of the failed-table check
  # below: a steroid found in a regimen, a question that could not be answered,
  # or drug codes that fell back to defaults. The write step reads this list.
  extra_gaps <- character()
  if (!isTRUE(tok$resolved))
    extra_gaps <- c(extra_gaps,
      "Agent tokens could not be resolved from cl_mma_codelist.csv - fell back to defaults (DARA/BORT/LENA/MELP); every token-based answer (Q2, Q3, Q4 and D1-D3) is unverified")

  # ---- Read Me -----------------------------------------------------------
  add_sheet(name = "Read Me", title = paste0("LOT follow-up study-team questions - ", cohort_label),
    subtitle = paste0("Generated ", stamp, " by lot_followup_qs.R against ", cfg$work_schema),
    narrative = c(
      sprintf("Cohort: %s. LOT1 = %s patients; LOT2 = %s patients. Switch with LOT_COHORT=FULL / NDMM.",
              cohort_label, format(n_lot1, big.mark = ","), format(n_lot2, big.mark = ",")),
      tok$notes,
      "Q1 = steroids are already excluded from the LOT rules; no LOT re-run is needed. The audit checks whether any known steroid token appears in a LOT regimen (see the Q1 tab). Only the display / steroid-timing outputs use steroid_codes.csv.",
      "Q2 = among 1L DARA+BORT dual-therapy patients (exactly two agents), the difference between the DARA and BORT start dates (same-day vs staggered; which comes first; gap distribution).",
      "Q3 = LENA+DARA dual-therapy share in 1L and 2L (exact dual + a 'contains both, any combination' context column).",
      "Q4 = Melphalan in 2L by LOT2 start year, with a pre-/post-2017 summary and the top MELP-containing 2L regimens.",
      "Q5 = CAR-T clarifications: the CAR-T-relative-to-LOT1 metric table recomputed on this cohort, then plain answers to the five CAR-T questions.",
      "D1-D3 = deep-dives that follow up the study team's concerns: D1 tests whether Melphalan-in-2L is really transplant conditioning; D2 shows the top-15 1L/2L regimens so LENA+DARA can be seen in context; D3 traces same-day and staggered DARA+BORT patients back to their raw claims.",
      "Regimen strings (LOT_BASE_MEDS) are space-separated, alphabetically-sorted MM-agent tokens; steroids are excluded by construction, so 'DARA+BORT dual therapy, no other agents' means no other MM agent (a backbone steroid does not change the pairing).",
      "The CAR-T metrics and the induction-window setting are reused from R/validation_qs.R."),
    tables = list())

  # ---- Q1: steroids ------------------------------------------------------
  q1 <- q1_steroid_audit(con, lot_long)
  # Let the audit result pick the wording, so the tab never contradicts the table:
  #  - query failed : could not run (the gap scan marks the run incomplete).
  #  - count > 0     : a steroid got into a regimen -> incomplete.
  #  - count == 0    : the expected result -> "no known steroid token was found".
  #  - NA / empty    : ran but no usable count (no regimen rows) -> inconclusive.
  q1_failed <- is_status_table(q1$audit)
  q1_hits <- if (q1_failed) NA_integer_
             else suppressWarnings(as.integer(q1$audit$n_rows_with_steroid_token[1]))
  q1_inconclusive <- !q1_failed && is.na(q1_hits)
  if (q1_failed) {
    q1_notes <- c(
      "The steroid audit could not run (see the status table). This workbook is incomplete.")
  } else if (isTRUE(q1_hits > 0)) {
    extra_gaps <- c(extra_gaps, sprintf("Q1 Steroids off / audit failed: %d regimen row(s) contain a steroid token", q1_hits))
    q1_notes <- c(
      sprintf("Audit failed: %d LOT regimen row(s) contain a steroid token. This is unexpected - steroids should never enter a regimen. Investigate before using this workbook (see the audit and token tables).", q1_hits),
      "A non-zero count means a steroid reached a regimen string, which the LOT rules are meant to prevent.")
  } else if (q1_inconclusive) {
    extra_gaps <- c(extra_gaps, "Q1 Steroids off / audit inconclusive - no usable count (no LOT regimen rows?)")
    q1_notes <- c(
      "The steroid audit returned no usable count this run (no LOT regimen rows), so it is inconclusive. Re-run once the cohort's LOT_LONG is populated.")
  } else {
    q1_notes <- c(
      "Steroids are already excluded from LOT assignment. They never set a line start, join a regimen, or trigger a new line, so turning steroids off needs no change to the LOT rules and no re-run.",
      paste0("No known steroid token was found in any LOT regimen. The audit checks against the known steroid abbreviations (",
             paste(STEROID_TOKENS, collapse = ", "),
             "); the token table lists every agent that actually appears in the regimens, so an unlisted abbreviation would still be visible for a reader to catch."),
      "Steroids do not affect LOT assignment. The dashboard and steroid-timing outputs use steroid_codes.csv, and steroid rows may still appear in the mapped medication data depending on the production codelist - but neither reaches a LOT. To drop steroids from the descriptive outputs too, empty steroid_codes.csv; the LOT results are unaffected.")
  }
  add_sheet(name = "Q1 Steroids off", title = "Q1 - Steroids are already excluded from the LOT",
    subtitle = paste0("Regimen audit + agent-token list. Cohort: ", cohort_label, "."),
    narrative = q1_notes,
    tables = list(
      "Steroid tokens in any LOT regimen (expected: 0)" = q1$audit,
      "Agent tokens appearing in LOT regimens"          = q1$vocab))

  # ---- Q2: DARA+BORT start-date difference -------------------------------
  q2_tables <- list(); q2_notes <- character()
  if (have_map) {
    q2 <- best_effort(q2_dara_bort_gap(con, lot_long, map_tbl, tok$dara, tok$bort, VQS_W1),
                      "DARA+BORT start-date gap")
    if (is.data.frame(q2)) {
      q2_tables[["DARA+BORT dual therapy - start-date difference (overview)"]] <- q2
      q2_notes <- c(q2_notes, "The Q2 analysis could not run - see the status table.")
    } else {
      q2_tables[["DARA+BORT dual therapy - start-date difference (overview)"]] <- q2$overview
      q2_tables[["Absolute start-date gap distribution"]] <- q2$distribution
      q2_notes <- c(q2_notes,
        sprintf("Denominator: %s 1L patients whose regimen is exactly DARA + BORT (no other MM agent).",
                format(as.integer(q2$n_dual), big.mark = ",")),
        sprintf("Each agent's start = the first MAP_STACKED segment of that agent inside the %d-day LOT1 induction window (the window that defines regimen membership). gap = BORT start - DARA start (days).", VQS_W1),
        "A large same-day count means both agents have the same first observed start date; a spread toward staggered starts (DARA first or BORT first) means one was started and the other added later, within the induction window. Note these are observed claim dates, not the prescriber's intent.")
    }
  } else {
    q2_notes <- paste0(map_tbl, " not readable - per-agent start dates need MAP_STACKED; Q2 could not be built.")
    # Show a status row (not an empty tab) so the skipped Q2 marks the run incomplete.
    q2_tables[["DARA+BORT dual therapy - start-date difference"]] <- data.frame(
      status = paste0(map_tbl, " not readable - MAP_STACKED is required for per-agent start dates."),
      stringsAsFactors = FALSE)
  }
  add_sheet(name = "Q2 DARA+BORT dates", title = "Q2 - 1L DARA+BORT: difference in agent start dates",
    subtitle = paste0("Among patients whose 1L regimen is exactly DARA + BORT (dual therapy, no other agents). Cohort: ",
                      cohort_label, "."),
    narrative = q2_notes, tables = q2_tables)

  # ---- Q3: LENA+DARA dual therapy ---------------------------------------
  q3_df <- best_effort(q3_lena_dara(con, lot_long, tok$dara, tok$lena), "LENA+DARA dual share")
  add_sheet(name = "Q3 LENA+DARA", title = "Q3 - LENA+DARA dual therapy share in 1L and 2L",
    subtitle = paste0("Exact dual = regimen is exactly DARA + LENA; 'contains both' allows other agents. Cohort: ",
                      cohort_label, "."),
    narrative = c(
      "The first percentage is the share of each line's patients whose regimen is exactly DARA + LENA (dual therapy). The second is the broader share whose regimen contains both DARA and LENA in any combination (e.g. DARA + LENA + another agent).",
      "Denominators are the distinct patients reaching each line (1L, then 2L)."),
    tables = list("LENA+DARA share by line" = q3_df))

  # ---- Q4: Melphalan in 2L timing ---------------------------------------
  q4 <- best_effort(q4_melp_2l(con, lot_long, tok$melp), "Melphalan-in-2L timing")
  if (is.data.frame(q4)) {
    q4_tables <- list("Melphalan in 2L" = q4)
  } else {
    q4_tables <- list(
      "MELP-containing 2L regimens by LOT2 start year" = q4$by_year,
      "Pre-/post-2017 summary"                         = q4$era,
      "Top MELP-containing 2L regimen strings"         = q4$top_regimens)
  }
  add_sheet(name = "Q4 MELP in 2L", title = "Q4 - Melphalan in 2L: calendar timing",
    subtitle = paste0("Is MELP-in-2L concentrated before 2018? Cohort: ", cohort_label, "."),
    narrative = c(
      "n_lot2_with_melp / pct_melp = share of all 2L lines that year (any start type) whose regimen contains MELP. The study team expects MELP to fade as a common 2L option after 2017.",
      "The summary table splits MELP-containing 2L regimens into LOT2-start <=2017 vs >=2018 and gives the median LOT2 start year.",
      "The regimen table lists the most common MELP-containing 2L regimen strings for context (e.g. transplant-conditioning vs oral combinations)."),
    tables = q4_tables)

  # ---- Q5: CAR-T clarifications -----------------------------------------
  q5_tables <- list(); q5_notes <- character()
  if (have_sct) {
    q5_tables[["CAR-T relative to LOT1 (recomputed on this cohort)"]] <-
      best_effort(vqs_q6_cart(con, lot_long, sct_tbl, w1 = VQS_W1, cart_raw_tbl = cart_raw),
                  "CAR-T-relative-to-LOT1 metric table")
    q5_tables[["Empirical answers to the five CAR-T questions"]] <-
      best_effort(q5_cart_clarifications(con, lot_long, sct_tbl, VQS_W1), "CAR-T clarifications")
    if (is.null(cart_raw))
      extra_gaps <- c(extra_gaps, "Q5 CAR-T / (b) pre-LOT1 CAR-T scan unavailable - question (b) unanswered")
    q5_notes <- c(
      if (cohort_mode == "NDMM")
        sprintf("The first table recomputes the CAR-T-relative-to-LOT1 measures for this cohort (%s, LOT1 = %s patients). The study team's earlier table showed 11,148 LOT1 patients and 124 with any CAR-T on/after LOT1 - compare those to the table below. Set LOT_COHORT=FULL for the whole LOT cohort.",
                cohort_label, format(n_lot1, big.mark = ","))
      else
        sprintf("The first table recomputes the CAR-T-relative-to-LOT1 measures for this cohort (%s, LOT1 = %s patients). The study team's earlier table (11,148 LOT1 patients; 124 with any CAR-T) was the NDMM default cohort, so this full-cohort run will differ. Run without LOT_COHORT for the NDMM view.",
                cohort_label, format(n_lot1, big.mark = ",")),
      if (is.null(cart_raw))
        "The pre-LOT1 CAR-T rows need the raw claims scan, which was not available this run, so 'CAR-T before LOT1' and 'prior-to-or-during' show as NA. The during/closing timing is still valid. This run is marked incomplete for question (b)."
      else
        "The raw CAR-T scan ran, so 'CAR-T before LOT1' is populated from raw claim dates.",
      "Answers (counts are in the second table):",
      sprintf("(a) The two windows are not the same. One counts a CAR-T in the first %d days after LOT1 starts; the other counts a CAR-T any time up to when LOT1 ends. A CAR-T can fall in one and not the other, so the subset-test rows show how much they overlap here.", VQS_W1),
      if (is.null(cart_raw))
        "(b) 'Prior-to-or-during LOT1' means a CAR-T before LOT1 or during/closing it. The before-LOT1 count needs the raw scan, which was not available, so (b) cannot be answered this run. What still holds: every patient counted in 'during or closing' had the CAR-T on or after LOT1 start."
      else
        "(b) 'Prior-to-or-during LOT1' means a CAR-T before LOT1 or during/closing it. When 'CAR-T before LOT1' is 0, every such patient had the CAR-T during LOT1; if it is above 0, that many had a CAR-T before LOT1 started.",
      "(c) 'Any CAR-T on/after LOT1 start' counts patients (one per patient), not CAR-T events, and it includes later lines (2L and beyond). The 'strictly after LOT1 ends' row shows how many of those were on a later line rather than during LOT1.",
      "(d) A CAR-T on the LOT1 start date is possible (the first CAR-T date can equal the LOT1 start); the count row shows how often it happens here.",
      "(e) When a CAR-T closes LOT1, LOT1 ends the day before the CAR-T and LOT2 starts on the CAR-T date. If another, higher-priority transplant event lands on that same date, it can change the LOT2 start type but not the date.")
  } else {
    q5_notes <- paste0(sct_tbl, " not readable - CAR-T analysis needs LOT1_SCT (FIRST_CART_DT). Q5 could not be built.")
    # Show a status row (not an empty tab) so the skipped Q5 marks the run incomplete.
    q5_tables[["CAR-T relative to LOT1"]] <- data.frame(
      status = paste0(sct_tbl, " not readable - LOT1_SCT (FIRST_CART_DT) is required."),
      stringsAsFactors = FALSE)
  }
  add_sheet(name = "Q5 CAR-T", title = "Q5 - CAR-T relative to LOT1: metrics + clarifications",
    subtitle = paste0("Metric table recomputed on this cohort + empirical answers to the five CAR-T questions. Cohort: ",
                      cohort_label, "."),
    narrative = q5_notes, tables = q5_tables)

  # ---- D1: Melphalan-in-2L = transplant conditioning? --------------------
  d1 <- best_effort(q_melp_conditioning(con, lot_long, tok$melp), "Melphalan conditioning check")
  d1_tables <- if (is.data.frame(d1)) list("Melphalan in 2L" = d1) else list(
    "MELP-in-2L transplant signal"                 = d1$signal,
    "MELP-monotherapy 2L line length"              = d1$length,
    "Days from melphalan start to the transplant"  = d1$timing,
    "MELP-in-2L by LOT2 start type"                = d1$by_type)
  add_sheet(name = "D1 Melphalan check", title = "D1 - Is Melphalan-in-2L transplant conditioning?",
    subtitle = paste0("Signals consistent with conditioning (not a proof). Cohort: ", cohort_label, "."),
    narrative = c(
      "The study team flagged Melphalan as an odd top-10 2L regimen. High-dose melphalan is the drug that conditions an autologous stem-cell transplant, so MELP-in-2L may be conditioning rather than second-line treatment. These tables show whether the signals point that way in this cohort - they are supporting evidence, not proof.",
      "Table 1: how many MELP-in-2L lines carry an autologous transplant (transplant flagged, and monotherapy-plus-transplant).",
      "Table 2: how long the melphalan-only lines last. A very short line is more consistent with conditioning than with ongoing therapy, but length alone does not prove intent.",
      "Table 3: for MED-started melphalan-only lines (where the line start is the melphalan claim date), the days from the melphalan start to the transplant. Melphalan a few days before the transplant (a small positive gap) is the conditioning pattern. SCT_AUTO-started lines are excluded here because their line start is the transplant date itself.",
      "If the signals line up, the study-team decision is whether to fold transplant-conditioning melphalan into 1L rather than open a 2L line (a spec choice, not changed here)."),
    tables = d1_tables)

  # ---- D2: Top regimens in 1L and 2L (LENA+DARA in context) --------------
  d2 <- best_effort(q_top_regimens(con, lot_long, tok$dara, tok$lena, n_lot1, n_lot2), "Top regimens")
  d2_tables <- if (is.data.frame(d2)) list("Top regimens" = d2) else list(
    "Top 15 regimens in 1L" = d2$lot1,
    "Top 15 regimens in 2L" = d2$lot2)
  add_sheet(name = "D2 Top regimens", title = "D2 - Top 15 regimens in 1L and 2L",
    subtitle = paste0("Ranked by distinct patients; the 'contains_DARA_LENA' flag marks DARA+LENA regimens. Cohort: ", cohort_label, "."),
    narrative = c(
      "This puts LENA+DARA in context. Steroids are excluded from the regimen strings, so real-world DARA+LEN+dexamethasone shows here as 'DARA LENA', and DARA+LEN+bortezomib as 'BORT DARA LENA'.",
      "The 'contains_DARA_LENA' column marks every regimen with both agents, so you can see where the DARA+LENA combinations actually rank instead of only the exact pair."),
    tables = d2_tables)

  # ---- D3: DARA+BORT journeys (is the same-service-date result real?) ----
  # d3_raw_partial stays FALSE unless D3 built its tables but had to skip the
  # source-claim check (see below); the completeness scan reads it later.
  d3_tables <- list(); d3_notes <- character(); d3_raw_partial <- FALSE
  if (have_map) {
    d3 <- best_effort(q_dara_bort_examples(con, lot_long, map_tbl, tok$dara, tok$bort, VQS_W1,
                                           bounds$sql, bounds$available), "DARA+BORT journeys")
    if (is.data.frame(d3)) {
      d3_tables[["DARA+BORT examples"]] <- d3
    } else {
      d3_tables[["Example patients (same-day and staggered)"]] <- d3$picks
      if (!is.null(d3$journey)) d3_tables[["MAP segments - DARA and BORT start dates"]] <- d3$journey
      if (!is.null(d3$raw))     d3_tables[["Raw DARA/BORT claims (bounded to the study window)"]] <- d3$raw
      if (!is.null(d3$note))    d3_notes <- c(d3_notes, d3$note)
    }
    # A skipped raw-claim pull means D3 could not check the source claims. Flag
    # it so the run is reported as a partial deep dive, not a full one.
    d3_raw_partial <- is.list(d3) && !is.data.frame(d3) && isTRUE(d3$skip_raw)
    d3_notes <- c(d3_notes,
      "A few same-day and a few staggered DARA+BORT patients, traced from their DARA/BORT claims to the MAP start dates. This checks whether both agents were recorded on the same service date (the claims cannot show the same visit or the prescriber's intent).",
      if (d3_raw_partial)
        "The raw-claim table is left out this run because the observation window (ELIG_COH_FINAL) was unavailable, so an unbounded claim pull is avoided. The per-agent start dates above still answer the question."
      else NULL)
  } else {
    d3_notes <- paste0(map_tbl, " not readable - journeys need MAP_STACKED; D3 could not be built.")
    d3_tables[["DARA+BORT journeys"]] <- data.frame(
      status = paste0(map_tbl, " not readable - MAP_STACKED is required."), stringsAsFactors = FALSE)
  }
  add_sheet(name = "D3 DARA+BORT journeys", title = "D3 - DARA+BORT: same-service-date check from the source claims",
    subtitle = paste0("Same-day and staggered example patients (patient-level, DARA/BORT only). Cohort: ", cohort_label, "."),
    narrative = d3_notes, tables = d3_tables)

  # ---- flag anything that makes the run incomplete -------------------------
  # Keep the two deliverables separate: only a CORE gap (a Q1-Q5 answer or a
  # token fallback that also affects them) makes the workbook INCOMPLETE and gets
  # the filename suffix. A DEEP-DIVE gap (D1-D3) is noted but does not invalidate
  # the core answers. extra_gaps are all core (token / steroid audit / Q5b).
  core_gaps <- extra_gaps; deep_gaps <- character()
  for (s in sheets) for (nm in names(s$tables %||% list())) {
    entry <- s$tables[[nm]]; df <- entry
    if (is.list(entry) && !is.data.frame(entry)) df <- entry$df
    if (is_status_table(df)) {
      g <- sprintf("%s / %s", s$name, nm)
      if (grepl("^D[0-9]", s$name)) deep_gaps <- c(deep_gaps, g) else core_gaps <- c(core_gaps, g)
    }
  }
  # D3 built its tables but skipped the source-claim check (no observation
  # bounds). That is a partial deep dive, not a core gap, so note it here
  # without making the workbook incomplete.
  if (isTRUE(d3_raw_partial))
    deep_gaps <- c(deep_gaps,
      "D3 DARA+BORT journeys / source claims not checked (observation bounds unavailable; per-agent start dates still shown)")
  incomplete <- length(core_gaps) > 0
  readme_extra <- character()
  if (incomplete) {
    log_msg("WARNING: core answers are INCOMPLETE:")
    for (g in core_gaps) log_msg("  - ", g)
    readme_extra <- c(readme_extra,
      paste0("INCOMPLETE run: ", length(core_gaps),
             " core issue(s) - see the affected tabs and re-run once resolved. ",
             paste(core_gaps, collapse = "; "), "."))
  }
  if (length(deep_gaps) > 0) {
    log_msg("NOTE: ", length(deep_gaps), " deep-dive item(s) (D1-D3) are incomplete or unavailable (core answers unaffected):")
    for (g in deep_gaps) log_msg("  - ", g)
    readme_extra <- c(readme_extra,
      paste0("Note: ", length(deep_gaps), " deep-dive item(s) (D1-D3) are incomplete or unavailable - the core Q1-Q5 answers are unaffected. ",
             paste(deep_gaps, collapse = "; "), "."))
  }
  if (length(readme_extra) > 0)
    sheets[[1]]$narrative <- c(readme_extra, sheets[[1]]$narrative)

  # ---- write --------------------------------------------------------------
  suffix <- if (incomplete) "_INCOMPLETE" else ""
  xlsx <- file.path(out_dir, paste0("lot_followup_qs_", tolower(cohort_mode), "_", stamp, suffix, ".xlsx"))
  wbx_write_workbook(sheets, xlsx)
  log_msg(SEP)
  if (incomplete)
    log_msg("LOT follow-up study-team questions COMPLETED WITH GAPS (", length(core_gaps),
            " core issue(s)) - INCOMPLETE workbook -> ", xlsx)
  else
    log_msg("LOT follow-up study-team questions complete", if (length(deep_gaps) > 0)
            paste0(" (", length(deep_gaps), " deep-dive item(s) incomplete or unavailable)") else "",
            ". Excel workbook -> ", xlsx)
  log_msg(SEP)
}

if (!interactive()) main()
