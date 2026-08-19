#!/usr/bin/env Rscript
# One plain-text summary of the study-team answers, filled from the files the
# runs already wrote. Reads only; no connection.
#
#   Rscript reporting/summarize_results.R [results_dir]
#
# results_dir defaults to OUTPUT_DIR, then /mnt/artifacts/results. The
# melphalan, fold-in and audit outputs are also looked for in their own out/
# folders, so it works whether or not OUTPUT_DIR was set for those runs.
# A section whose files are missing says so instead of stopping.
#
# Writes results_summary_<stamp>.txt next to the August answers and prints
# the same text.

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
                 file.path(STUDY, "exploration", "lot", "out")))

# Newest file matching the pattern across the candidate dirs, or NULL.
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

out <- character(0)
say <- function(...) out <<- c(out, paste0(...))

say("NDMM lines of therapy - answers for the study team")
say("Made ", format(Sys.time(), "%d %b %Y %H:%M"), " from the finished runs.")
say("")

# ---- which run this all comes from ------------------------------------------
status_f <- find_file("^aug15_qs_run_status_.*\\.txt$")
stamp <- if (!is.null(status_f))
  sub("^aug15_qs_run_status_(.*)\\.txt$", "\\1", basename(status_f)) else
  format(Sys.time(), "%Y%m%d_%H%M%S")
if (!is.null(status_f)) {
  st <- readLines(status_f, warn = FALSE)
  run_ln <- grep("LOT run:", st, value = TRUE)
  if (length(run_ln)) say("Built on", sub(".*LOT run:", " LOT run", run_ln[1]), ".")
  waived <- grep("WAIVED|UNPROVEN", st, value = TRUE)
  for (w in waived)
    say("WARNING: ", trimws(w), " - fix this before sending numbers out.")
} else {
  say("NOTE: no aug15 run-status file found under ", res_dir,
      " - run analysis/questions/aug15_studyteam_qs.R first.")
}
say("")

# ---- 1. the MAP splitting rule ----------------------------------------------
say("1) HOW MANY PATIENTS DOES THE MAP-SPLITTING RULE AFFECT?")
aff <- read_any("^aug15_qs_map_splitting_affected_.*\\.csv$")
if (is.null(aff)) {
  say("   (no screen file found - run aug15_studyteam_qs.R)")
} else {
  all_rows <- aff[aff$LINE_THAT_ENDED == "ALL", ]
  pick <- function(basis, cls) {
    r <- all_rows[all_rows$MATCH_BASIS == basis & all_rows$CLASS == cls, ]
    if (nrow(r)) r$N_PATIENTS[1] else 0
  }
  say("   Counting the same drug only:")
  say("     ", n1(pick("EXACT_TOKEN", "AFFECTED")),
      " patients would LOSE a line boundary under the rule.")
  say("     ", n1(pick("EXACT_TOKEN", "SAME_DAY_NEW_AGENT")),
      " keep it - a new drug started the same day.")
  say("   Counting the drug or its biosimilar:")
  say("     ", n1(pick("SUBSTITUTE_FAMILY", "AFFECTED")), " would lose it; ",
      n1(pick("SUBSTITUTE_FAMILY", "SAME_DAY_NEW_AGENT")), " keep it.")
  say("   What 'affected' means: a drug from the LAST line came back after the")
  say("   current line's window, and it is the only reason a new line starts.")
  roster <- find_file("^aug15_qs_map_splitting_review_roster_.*\\.csv$")
  if (!is.null(roster))
    say("   Patient-by-patient detail: ", basename(roster))
}
fv <- read_any("^foldin_vs_reference\\.csv$")
fp <- read_any("^foldin_patients\\.csv$")
if (!is.null(fp)) {
  say("   If we APPLY the rule (trial rebuild, not the study numbers):")
  say("     ", n1(fp$N_DIFFERENT), " of ", n1(fp$N_PATIENTS),
      " patients change; ", n1(fp$N_LINE_COUNT_DIFFERENT),
      " end up with a different NUMBER of lines.")
  ch <- find_file("^foldin_changed_lines\\.csv$")
  if (!is.null(ch)) say("     Their before/after lines: ", basename(ch))
  say("     Read with its assumptions: any earlier line's drug folds; the drug")
  say("     joins the line's TIME, not its regimen name; a return after the")
  say("     line ran out still folds. All three are open for the team to set.")
}
say("")

# ---- 2. the MELP rules ------------------------------------------------------
say("2) HOW ARE THE MELP RULES WORKING?")
mp <- read_any("^melp_modes_patients\\.csv$")
mv <- read_any("^melp_vs_reference\\.csv$")
if (is.null(mv) && is.null(mp)) {
  say("   (no melphalan comparison files found - run the melphalan package)")
} else {
  vs <- function(d, cellname, metric) {
    r <- d[d$cell == cellname & d$metric == metric, ]
    if (nrow(r)) paste0(n1(r$reference), " -> ", n1(r$observed)) else "?"
  }
  if (!is.null(mv)) {
    say("   Your five-branch rule, as a full trial rebuild (mode 'as_asked'):")
    say("     lines overall: ", vs(mv, "as_asked", "n_lines"),
        "; patients with any line: ", vs(mv, "as_asked", "n_patients"))
    say("     melphalan-only lines: ", vs(mv, "as_asked", "n_melp_mono_lines"),
        "; lines melphalan alone STARTED: ", vs(mv, "as_asked", "n_melp_mono_adv"))
  }
  if (!is.null(mp))
    say("   The two transplant readings differ for ", n1(mp$N_DIFFERENT),
        " patients - that choice is still the team's.")
}
sv <- read_any("^melp_simple_vs_reference\\.csv$")
sp <- read_any("^melp_simple_patients\\.csv$")
if (!is.null(sp))
  say("   The simple 28-day fallback changes ", n1(sp$N_DIFFERENT), " of ",
      n1(sp$N_PATIENTS), " patients vs today's build.")
say("   Notes: none of this touches the study's own numbers. 'Melphalan only'")
say("   counts captured drugs, so melphalan WITH a steroid still reads as")
say("   melphalan only. The 28-vs-30-day question is about course length;")
say("   changing the assumed days supplied is a different change, not built.")
say("")

# ---- 3. the 12-month CE funnel ----------------------------------------------
say("3) DISCONTINUE THE PRIOR LINE, THEN 12 MONTHS COVER BEFORE THE NEXT")
fu <- read_any("^aug15_qs_discontinued_then_12mo_ce_.*\\.csv$")
if (is.null(fu)) {
  say("   (no funnel file found - run aug15_studyteam_qs.R)")
} else {
  for (i in seq_len(nrow(fu))) {
    say("   ", fu$COHORT[i], ": ", n1(fu$N_DISCONTINUED_PRIOR[i]),
        " discontinued ", fu$PRIOR_LINE_DISCONTINUED[i], "; ",
        n1(fu$N_ALSO_HAS_NEXT_LINE[i]), " reached ", fu$COHORT[i], "; ",
        n1(fu$N_ALSO_12MO_CE_BEFORE_IT[i]),
        " also have 12 months cover before it.")
  }
  say("   '12 months' = one merged enrollment span over the 365 days before")
  say("   the line starts. Discontinued = the line's recorded end reason.")
  attr_f <- find_file("^aug15_qs_subsequent_cohort_attrition_.*\\.csv$")
  if (!is.null(attr_f))
    say("   The formally chained 2L/3L study cohort is beside it: ",
        basename(attr_f))
  else
    say("   (The chained study-cohort file was not written - its build did ",
        "not match this LOT run.)")
}
say("")

# ---- checks -----------------------------------------------------------------
say("CHECKS BEHIND THE NUMBERS")
aud <- read_any("^lot_audit_counts\\.csv$")
if (!is.null(aud)) {
  b <- aud[aud$finding == "transplant-belonging-to-no-line", ]
  if (nrow(b)) {
    say("   Transplant-in-no-line audit rows (shape b should be 0 after the ",
        "rebuild):")
    for (r in unique(b$row)) {
      rr <- b[b$row == r, ]
      say("     ", paste(paste0(rr$metric, "=", rr$value), collapse = "  "))
    }
  }
}
say("   QC report and the scenario workbook (lot_scenarios.xlsx) carry the")
say("   rest; the workbook's Open questions sheet lists what is not settled.")
say("")

say("DECISIONS WE NEED FROM THE TEAM")
say("   - MAP fold-in: adopt or not; only the last line's drugs or any earlier")
say("     line's; does the drug join the regimen name or only the line's time;")
say("     does a return after the line ran out still fold?")
say("   - MELP: five-branch rule or the simple fallback; which transplant")
say("     reading; 28 or 30 days; what 'mono' should mean given MELP+DEX.")
say("   - Tandem transplants: today a stop between the two does NOT break the")
say("     pair; a new drug between them does. Confirm or correct.")

txt <- paste(out, collapse = "\n")
dest <- file.path(if (dir.exists(res_dir)) res_dir else ".",
                  paste0("results_summary_", stamp, ".txt"))
writeLines(txt, dest)
cat(txt, "\n\nWrote ", dest, "\n", sep = "")
