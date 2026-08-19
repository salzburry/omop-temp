#!/usr/bin/env Rscript
# One short workbook of the study-team answers, filled from the files the
# runs already wrote. Reads only; no connection.
#
#   Rscript reporting/summarize_results.R [results_dir]
#
# results_dir defaults to OUTPUT_DIR, then /mnt/artifacts/results. The
# melphalan, fold-in, QC, dashboard and audit outputs are also looked for in
# their own out/ folders, so it works whether or not OUTPUT_DIR was set for
# those runs. A missing file becomes a "run X first" note, never a stop.
#
# Writes results_summary_<stamp>.xlsx (one small sheet per topic; needs
# openxlsx) and results_summary_<stamp>.txt (same content as plain text,
# always written - paste it into an email as is).

.script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a[1]),
                                            fixed = TRUE))) else getwd()
})
STUDY <- dirname(.script_dir)

argv <- commandArgs(trailingOnly = TRUE)
res_dir <- if (length(argv)) argv[1] else {
  d <- trimws(Sys.getenv("OUTPUT_DIR", unset = ""))
  if (nzchar(d)) d else "/mnt/artifacts/results"
}
DIRS <- unique(c(res_dir,
                 file.path(STUDY, "exploration", "melphalan", "out"),
                 file.path(STUDY, "exploration", "lot", "out"),
                 file.path(STUDY, "lot", "qc", "out"),
                 "/mnt/artifacts/dashboard/csv",
                 file.path(res_dir, "csv")))

find_file <- function(pattern) {
  hits <- unlist(lapply(DIRS[dir.exists(DIRS)], function(d)
    list.files(d, pattern, full.names = TRUE)))
  if (!length(hits)) return(NULL)
  hits[order(file.mtime(hits), decreasing = TRUE)][1]
}
read_any <- function(pattern) {
  f <- find_file(pattern)
  if (is.null(f)) return(NULL)
  tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
}
n1 <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "?" else
  format(as.numeric(x[1]), big.mark = ",")

# ---- gather everything first ------------------------------------------------
status_f <- find_file("^aug15_qs_run_status_.*\\.txt$")
stamp <- if (!is.null(status_f))
  sub("^aug15_qs_run_status_(.*)\\.txt$", "\\1", basename(status_f)) else
  format(Sys.time(), "%Y%m%d_%H%M%S")
run_line <- "run not identified - no aug15 run-status file found"
warn_lines <- character(0)
if (!is.null(status_f)) {
  st <- readLines(status_f, warn = FALSE)
  r <- grep("LOT run:", st, value = TRUE)
  if (length(r)) run_line <- trimws(r[1])
  warn_lines <- trimws(grep("WAIVED|UNPROVEN", st, value = TRUE))
}

aff <- read_any("^aug15_qs_map_splitting_affected_.*\\.csv$")
map_tbl <- NULL
if (!is.null(aff)) {
  a <- aff[aff$LINE_THAT_ENDED == "ALL", ]
  pick <- function(basis, cls) {
    r <- a[a$MATCH_BASIS == basis & a$CLASS == cls, ]
    if (nrow(r)) r$N_PATIENTS[1] else 0
  }
  map_tbl <- data.frame(
    `How we match the drug` = c("Same drug only", "Drug or its biosimilar"),
    `Split goes away`  = c(pick("EXACT_TOKEN", "AFFECTED"),
                           pick("SUBSTITUTE_FAMILY", "AFFECTED")),
    `Split stays (new drug same day)` =
                         c(pick("EXACT_TOKEN", "SAME_DAY_NEW_AGENT"),
                           pick("SUBSTITUTE_FAMILY", "SAME_DAY_NEW_AGENT")),
    check.names = FALSE, stringsAsFactors = FALSE)
}
roster_f <- find_file("^aug15_qs_map_splitting_review_roster_.*\\.csv$")

# When does the old drug come back? Two clocks per affected patient, banded:
# how long they were OFF the drug (from its last covered day to its return),
# and how far past the line's regimen window the return landed. One row per
# patient - their first affected line - on the same-drug reading.
map_gap_tbl <- NULL
gap_head <- NULL
ros <- read_any("^aug15_qs_map_splitting_review_roster_.*\\.csv$")
if (!is.null(ros) &&
    all(c("MATCH_BASIS", "CLASS", "PATID", "GAP_FROM_PRIOR_EPISODE_END_DAYS",
          "BOUNDARY_MEDS_START_DT", "INDUCTION_WINDOW_END") %in% names(ros))) {
  r <- ros[ros$MATCH_BASIS == "EXACT_TOKEN" & ros$CLASS == "AFFECTED", ]
  r <- r[!duplicated(r$PATID), ]
  if (nrow(r)) {
    off <- suppressWarnings(as.numeric(r$GAP_FROM_PRIOR_EPISODE_END_DAYS))
    aft <- suppressWarnings(as.numeric(
      as.Date(r$BOUNDARY_MEDS_START_DT) - as.Date(r$INDUCTION_WINDOW_END)))
    bands <- c("up to 3 months", "3-6 months", "6-12 months",
               "1-2 years", "over 2 years")
    cnt <- function(v) {
      b <- cut(v, c(-Inf, 90, 180, 365, 730, Inf), labels = bands)
      c(as.integer(table(factor(b, levels = bands))), sum(is.na(v)))
    }
    map_gap_tbl <- data.frame(
      `Time band` = c(bands, "unknown"),
      `How long they were off the drug` = cnt(off),
      `How far past the line's window it came back` = cnt(aft),
      check.names = FALSE, stringsAsFactors = FALSE)
    gap_head <- paste0(
      n1(sum(off <= 180, na.rm = TRUE)), " come back within 6 months of ",
      "stopping the drug; ", n1(sum(off > 365, na.rm = TRUE)),
      " after more than a year.")
  }
}

fp <- read_any("^foldin_patients\\.csv$")
fold_lines_f <- find_file("^foldin_changed_lines\\.csv$")

mv <- read_any("^melp_vs_reference\\.csv$")
mp <- read_any("^melp_modes_patients\\.csv$")
sp <- read_any("^melp_simple_patients\\.csv$")
melp_tbl <- NULL
if (!is.null(mv)) {
  g <- function(metric) {
    r <- mv[mv$cell == "as_asked" & mv$metric == metric, ]
    if (nrow(r)) c(r$reference[1], r$observed[1]) else c(NA, NA)
  }
  m1 <- g("n_lines"); m2 <- g("n_melp_mono_lines"); m3 <- g("n_melp_mono_adv")
  melp_tbl <- data.frame(
    What = c("Total lines", "Melphalan-only lines",
             "Lines melphalan alone started"),
    Today = c(m1[1], m2[1], m3[1]),
    `Under the July rule` = c(m1[2], m2[2], m3[2]),
    check.names = FALSE, stringsAsFactors = FALSE)
}

fu <- read_any("^aug15_qs_discontinued_then_12mo_ce_.*\\.csv$")
fu_tbl <- NULL
if (!is.null(fu))
  fu_tbl <- data.frame(
    `For` = fu$COHORT,
    `Stopped the prior line` = fu$N_DISCONTINUED_PRIOR,
    `Went on to the next line` = fu$N_ALSO_HAS_NEXT_LINE,
    `Also 12 months cover before it` = fu$N_ALSO_12MO_CE_BEFORE_IT,
    check.names = FALSE, stringsAsFactors = FALSE)
attr_f <- find_file("^aug15_qs_subsequent_cohort_attrition_.*\\.csv$")

# The counts are per LINE: a patient with the shape at two lines is counted
# at each. Shown line by line for that reason - the total says how often the
# shape happens, and one patient can be behind more than one of them, so it
# is NOT a patient count and can be larger than the cohort.
scen_pivot <- function(d) {
  d <- d[!is.na(d$N_PATIENTS), ]
  d$N_PATIENTS <- as.numeric(d$N_PATIENTS)
  d$LOT_NUM <- suppressWarnings(as.integer(d$LOT_NUM))
  wide <- data.frame(Shape = sort(unique(d$ID)), stringsAsFactors = FALSE)
  for (ln in sort(unique(d$LOT_NUM[!is.na(d$LOT_NUM)]))) {
    v <- d[!is.na(d$LOT_NUM) & d$LOT_NUM == ln, c("ID", "N_PATIENTS")]
    wide[[paste0("Line ", ln)]] <- v$N_PATIENTS[match(wide$Shape, v$ID)]
  }
  ln_cols <- setdiff(names(wide), "Shape")
  wide[["Times it happens (all lines)"]] <-
    rowSums(wide[ln_cols], na.rm = TRUE)
  if ("WHAT_HAPPENS_TO_THE_PATIENT" %in% names(d)) {
    story <- d[!duplicated(d$ID), c("ID", "WHAT_HAPPENS_TO_THE_PATIENT")]
    wide[["What happens"]] <- story[[2]][match(wide$Shape, story$ID)]
  }
  wide[order(-wide[["Times it happens (all lines)"]]), ]
}
scen_tbl <- NULL
wb_f <- find_file("^lot_scenarios\\.xlsx$")
if (!is.null(wb_f) && requireNamespace("openxlsx", quietly = TRUE)) {
  for (sh in tryCatch(openxlsx::getSheetNames(wb_f), error = function(e) character(0))) {
    d <- tryCatch(openxlsx::read.xlsx(wb_f, sheet = sh), error = function(e) NULL)
    if (!is.null(d) && all(c("ID", "LOT_NUM", "N_PATIENTS") %in% names(d))) {
      scen_tbl <- scen_pivot(d)
      break
    }
  }
}

qc <- read_any("^lot_qc_results\\.csv$")
ar <- read_any("^all_regimens\\.csv$")
aud <- read_any("^lot_audit_counts\\.csv$")
aud_line <- NULL
if (!is.null(aud)) {
  b <- aud[aud$finding == "transplant-belonging-to-no-line", ]
  if (nrow(b))
    aud_line <- paste(unlist(lapply(unique(b$row), function(r) {
      rr <- b[b$row == r, ]
      paste(paste0(rr$metric, "=", rr$value), collapse = " ")
    })), collapse = "; ")
}

decisions <- data.frame(
  Topic = c("MAP rule", "MAP rule", "MAP rule", "MAP rule",
            "MELP", "MELP", "MELP", "MELP", "Tandems"),
  Question = c(
    "Use it or not?",
    "Any old line's drugs, or just the last line's?",
    "Add the returning drug to the regimen, or just extend the line?",
    "Fold it in even after the line already ended?",
    "The July rule or the simple 28-day one?",
    "What happens to a dose right next to a transplant?",
    "28 or 30 days for the course cap?",
    "Is melphalan+DEX 'melphalan alone'?",
    paste0("Today a treatment stop between two transplants does not break ",
           "the pair, but a new drug does. Is that right?")),
  stringsAsFactors = FALSE)

# ---- the one-line answers ---------------------------------------------------
sum_rows <- list()
add_sum <- function(q, a) sum_rows[[length(sum_rows) + 1L]] <<-
  data.frame(Question = q, Answer = a, stringsAsFactors = FALSE)
if (!is.null(map_tbl))
  add_sum("How many patients does the MAP-splitting rule affect?",
          paste0(n1(map_tbl[1, 2]), " lose the split (same drug); ",
                 n1(map_tbl[2, 2]), " counting biosimilars. ",
                 n1(map_tbl[1, 3]), " more keep it - a new drug started the same day."))
if (!is.null(gap_head))
  add_sum("When does the old drug come back?", gap_head)
if (!is.null(fp))
  add_sum("What if we apply the rule? (test copy, study numbers untouched)",
          paste0(n1(fp$N_DIFFERENT), " of ", n1(fp$N_PATIENTS),
                 " patients change; ", n1(fp$N_LINE_COUNT_DIFFERENT),
                 " get a different number of lines."))
if (!is.null(melp_tbl))
  add_sum("MELP - the July rule (test copy)",
          paste0("Melphalan-only lines ", n1(melp_tbl[2, 2]), " -> ",
                 n1(melp_tbl[2, 3]), "; lines it alone started ",
                 n1(melp_tbl[3, 2]), " -> ", n1(melp_tbl[3, 3]), "."))
if (!is.null(sp))
  add_sum("MELP - the simple 28-day version",
          paste0("Changes ", n1(sp$N_DIFFERENT), " of ", n1(sp$N_PATIENTS),
                 " patients."))
if (!is.null(fu_tbl)) for (i in seq_len(nrow(fu_tbl)))
  add_sum(paste0("Stopped prior line + 12 months cover, for ", fu_tbl[i, 1]),
          paste0(n1(fu_tbl[i, 2]), " stopped; ", n1(fu_tbl[i, 3]),
                 " reached the line; ", n1(fu_tbl[i, 4]),
                 " also had the 12 months."))
summary_tbl <- if (length(sum_rows)) do.call(rbind, sum_rows) else
  data.frame(Question = "No result files found under these folders",
             Answer = paste(DIRS, collapse = "; "), stringsAsFactors = FALSE)

notes <- c(
  paste0("Built on ", run_line, "."),
  if (length(warn_lines)) paste0("WARNING: ", warn_lines,
                                 " - fix before sending out."),
  "The rule tests are separate copies. The study's own numbers are unchanged.",
  paste0("'Affected' means: a drug from an old line came back after the new ",
         "line's window, and that alone is what splits the line today."),
  paste0("12 months = enrolled for the full 365 days before the line starts ",
         "(small gaps merged)."),
  paste0("Melphalan with a steroid counts as melphalan alone - steroids are ",
         "not in the captured drug list."),
  paste0("Scenario counts are per LINE: a patient with the shape at two ",
         "lines is counted at each, so the totals can be larger than the ",
         "number of patients."),
  if (!is.null(map_gap_tbl)) paste0(
    "The time-gap sheet uses two clocks: days from the drug's last covered ",
    "day to its return, and days from the end of the line's regimen window ",
    "to the return. One row per affected patient, same-drug reading."),
  if (!is.null(roster_f)) paste0("Patient list for the MAP rule: ", basename(roster_f)),
  if (!is.null(fold_lines_f)) paste0("Before/after lines per changed patient: ",
                                     basename(fold_lines_f)),
  if (!is.null(attr_f)) paste0("Official 2L/3L cohort funnel: ", basename(attr_f)),
  if (!is.null(aud_line)) paste0("Audit - transplants outside every line ",
                                 "(want 0): ", aud_line),
  if (!is.null(qc)) paste0("QC: ", nrow(qc), " checks ran, ",
                           sum(!(tolower(qc$result) %in% c("ok", "pass", ""))),
                           " to look at (lot_qc_report.md)."),
  if (!is.null(ar)) paste0("Full regimen list: all_regimens.csv, ", nrow(ar),
                           " rows, nothing cut off."))

# ---- write ------------------------------------------------------------------
dest_dir <- if (dir.exists(res_dir)) res_dir else "."
sheets <- list("Answers"  = summary_tbl,
               "Notes"    = data.frame(Note = notes, stringsAsFactors = FALSE),
               "Decisions to make" = decisions)
if (!is.null(map_tbl))  sheets[["MAP rule"]] <- map_tbl
if (!is.null(map_gap_tbl)) sheets[["MAP rule - time gaps"]] <- map_gap_tbl
if (!is.null(melp_tbl)) sheets[["MELP"]] <- melp_tbl
if (!is.null(fu_tbl))   sheets[["12-month cover"]] <- fu_tbl
if (!is.null(scen_tbl)) sheets[["Scenario counts"]] <- scen_tbl

# Strip control characters from every text cell - a warehouse string with
# one in it makes Excel "repair" the workbook down to nothing.
sheets <- lapply(sheets, function(d) {
  for (nm in names(d)) if (is.character(d[[nm]]))
    d[[nm]] <- gsub("[\x01-\x08\x0B\x0C\x0E-\x1F\x7F]", "", d[[nm]],
                    useBytes = TRUE)
  d
})

xlsx_dest <- file.path(dest_dir, paste0("results_summary_", stamp, ".xlsx"))
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets[[nm]])
    openxlsx::setColWidths(wb, nm, cols = seq_along(sheets[[nm]]),
                           widths = "auto")
  }
  openxlsx::saveWorkbook(wb, xlsx_dest, overwrite = TRUE)
  cat("Wrote ", xlsx_dest, "\n", sep = "")
} else {
  cat("openxlsx is not installed, so no workbook - the text file has the ",
      "same content.\n", sep = "")
}

txt <- character(0)
for (nm in names(sheets)) {
  txt <- c(txt, nm, strrep("-", nchar(nm)))
  d <- sheets[[nm]]
  for (i in seq_len(nrow(d)))
    txt <- c(txt, paste0("  ", paste(paste0(names(d), ": ", as.character(d[i, ])),
                                     collapse = "  |  ")))
  txt <- c(txt, "")
}
txt_dest <- file.path(dest_dir, paste0("results_summary_", stamp, ".txt"))
writeLines(txt, txt_dest)
cat(paste(txt, collapse = "\n"), "\nWrote ", txt_dest, "\n", sep = "")
