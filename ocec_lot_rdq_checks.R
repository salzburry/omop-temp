# =============================================================================
# OC/EC LOT RDQ - checks of the RWDPs for the open questions
# =============================================================================
# One self-contained program. Run on the platform with DATABRICKS_PWD set:
#   source("ocec_lot_rdq_checks.R")
# It reads the EC and OC RWDPs (rwdp_source) and writes
# <out_stem>_question_probes.xlsx to out_dir:
#   Probes  - one row per check: the open question, what was counted, the result
#             and how to read it ("not run: ..." when a column is missing)
#   Tables  - the cross-tabulations the checks refer to
#   Columns - every column of each RWDP: type, % of rows with a value, distinct values
# Aggregates only: no patient IDs or dates; counts 1-4 show as "<5", larger counts
# are rounded to 10 ("all but <5" when the rest is small); percentages only with
# a denominator of at least min_pct_d and when no count, rest or difference
# between the counts shown together is 1-4.
# The checks per question are listed in docs/open_questions.md and the README.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(openxlsx) })

# ---- Parameters ---------------------------------------------------------------
out_dir  <- "/mnt/artifacts/results"
out_stem <- paste0("TFL_OCEC_LOT_FI_RDQ_v0.1_", toupper(format(Sys.Date(), "%d%b%Y")))

# Where each RWDP is read from: a Databricks table (catalog.schema.table, read
# through the RWDE DSN; needs DATABRICKS_PWD), or an .rds, .parquet or .csv file.
# From the RWDP data dictionary (docs/RWDP_data_dictionary_2026m09.pdf, Summary).
# The OC table is v3 in a personal schema; the form names rwdap_ovar_pano_2026m09_v1
# (open question Q1).
rwdp_source <- c(EC = "hive_metastore.rwd_flatiron_dataproducts.rwdap_endo_2026m09_v1",
                 OC = "hive_metastore.am338369.rwdap_oc_pano_2026m09_v3")

# Study period (form); lines starting after its end are not listed
study_period <- list(EC = as.Date(c("2020-03-01", "2026-08-31")),
                     OC = as.Date(c("2020-03-01", "2026-07-31")))
# 1L initiation window, both ends included (EC criterion 4, OC criterion 5)
l1_window <- list(EC = as.Date(c("2021-04-01", "2026-05-31")),
                  OC = as.Date(c("2020-04-01", "2026-04-30")))
min_age        <- 18
oc_l1_days     <- 120     # OC criterion 3: 1L or surgery 0-120 days after initial diagnosis
visit_gap_days <- 180     # exclusion: a visit gap longer than this (a number of days, or the RWDP's 180-day flag)
lines_out      <- 2:5     # LOT tables and counts: 2L up to and including 5L
# PROC at later lines (open question Q16). FALSE: as plat_sen_v1 gives it on each line
# (a line that is not platinum-eligible is "Not Eligible", so PROC = N there even after
# an earlier resistant line). TRUE: once PROC = Y, every later line is PROC = Y, PSOC = N.
plat_carry_forward <- FALSE

# EC criterion 3, "chemotherapy, alone or in combination with IO, as 1L":
#   "chemo_io_only": at least one chemotherapy and nothing but chemotherapy or IO
#   "any_chemo":     at least one chemotherapy, whatever else the line contains
ec_1l_rule <- "chemo_io_only"
chemo_agents <- c("carboplatin", "cisplatin", "oxaliplatin", "paclitaxel", "docetaxel",
                  "doxorubicin", "epirubicin", "ifosfamide", "cyclophosphamide", "gemcitabine",
                  "topotecan", "irinotecan", "etoposide", "vinorelbine", "capecitabine",
                  "fluorouracil", "pemetrexed", "mitomycin", "trabectedin", "eribulin",
                  "ixabepilone", "bleomycin", "dactinomycin", "vincristine", "vinblastine")
io_agents    <- c("pembrolizumab", "dostarlimab", "durvalumab", "atezolizumab", "nivolumab",
                  "avelumab", "ipilimumab", "cemiplimab", "tremelimumab", "retifanlimab",
                  "tislelizumab", "toripalimab")
platinum_agents <- c("carboplatin", "cisplatin", "oxaliplatin")     # OC criterion 4

# ADC payload stems: line names containing one but matching no Appendix A agent are
# listed for review (possible ADCs missing from the list)
adc_stems <- c("vedotin", "deruxtecan", "dxd", "govitecan", "rezetecan", "tirumotecan",
               "sesutecan", "samrotecan", "emtansine", "mertansine", "ravtansine",
               "ozogamicin", "tesirine", "mafodotin", "sunirine", "tazevibulin",
               "rilsodotin", "ecteribulin", "duocarmazine")

min_cell  <- 5           # counts below this show as "<5"
min_pct_d <- 50          # no percentages below this denominator

# ---- RWDP variables -------------------------------------------------------------
# From the RWDP data dictionary (docs/RWDP_data_dictionary_2026m09.pdf; page
# numbers of that PDF) and the OC and EC RWDP specifications (docs/; notes in
# docs/rwdp_dictionary_notes.md). Column names are lower-cased on reading.
# - Layout: one row per patient per cohort; `cohort` holds the row's cohort:
#   overall_row = the patient row, line_rows = the rows indexed at lines 1, 2, ...
#   index_dt = the row's index date. The row of line i is the line row whose
#   index date is line i's start (else line_rows[i]; match_by_index = FALSE:
#   always line_rows[i]). Patient-level variables
#   are read on the patient row, or on the patient's first row if there is none.
#   cohort = NULL means one row per patient.
# - A name with {i} is a line variable repeated on every row (lot{i}, lot{i}sd).
# - visit_gap is read on the row of 2L. With visit_gap_values it is a flag (the
#   values meaning a gap > 180 days, no gap, no value; others stop the run);
#   without, a number of days (gap if > visit_gap_days).
# - Age: year(age_date) - birthyear; without birthyear, the birth year is
#   year(index date) - age on the patient row (as the RWDP defines age).
# - OC criterion 3: surg_dt (surgery date) or first_tx_days (days to the first
#   of surgery and 1L).
# - keep_rows: only rows with these values are read, e.g. list(db = "EDM").
# - lot_setting: line setting (adjuvant, advanced, ...), for the Data checks only.
# - PSOC/PROC: plat_status, read on the row of each line (or per line with {i});
#   psoc_values / proc_values give PSOC = Y / PROC = Y, plat_other_values give
#   N for both; no value gives Missing; any other value stops the run.
rwdp_vars <- list(
  EC = list(patientid   = "patid",
            keep_rows   = list(db = "EDM"),        # form criterion 1 and EC spec: "Overall ENDO if in EDM dataset" (p.21: EDM / DOS; Q14)
            cohort      = "cohortn",               # p.21: 1 Overall ENDO; 2 L1, 3 L1m, 4 L2, 5 L3, 6 L4;
            overall_row = 1,                       # 7 ADJ; 8 AdjX-L1, 9 AdjX-L2, 10 AdjX-L3
            line_rows   = c(8, 9, 10),             # adjuvant-extended lines 1-3 (no AdjX-L4/L5 rows)
            index_dt    = "indexdt",
            age         = "age",                   # EC spec: YEAR(INDEXDT) - birthyear; no birthyear column (p.12, p.31)
            age_date    = "advdxdt",               # age at advanced EC diagnosis (criterion 2); the Overall row's
                                                   # index date is the advanced diagnosis (EC spec), so no move expected
            visit_gap   = "visit_gap_max",         # p.22: days; EC spec: largest visit gap, initial dx to end of index
                                                   # treatment or follow-up; NA if < 2 visits; no 180-day flag
            lot_name    = "adjx_lot{i}",           # p.19: adjuvant-extended lines (form, note 1)
            lot_sd      = "adjx_lot{i}sd",
            lot_ed      = "adjx_lot{i}ed"),         # not in the EC spec (its LOTiED = last drug episode; Q12, Q17)
  OC = list(patientid   = "patid",
            cohort      = "cohortn",               # p.11: 1 Overall OC (index initial dx); 2 L1, 3 L1m,
            overall_row = 1,                       # 4 L2, 5 L2m, 6 L3, 7 L4, 8 L5; 9 2L + PROC
            line_rows   = c(2, 4, 6, 7, 8),
            index_dt    = "indexdt",
            birthyear   = "birthyear",             # p.2
            age_date    = "init",                  # age at initial OC diagnosis (criterion 2)
            init_dx     = "init",                  # initial OC diagnosis date (criterion 3)
            surg_dt     = "surgerydate",           # p.2 (also tt_first_tx_diag, p.11)
            visit_gap   = "visit180_cont_fl_diag", # p.10
            visit_gap_values = c(gap = "No", no_gap = "Yes", missing = "Missing"),
            lot_name    = "lot{i}",                # p.7
            lot_sd      = "lot{i}sd",
            lot_ed      = "lot{i}ed",              # spec: LOTiED = last drug episode of the line (Q12)
            lot_setting = "l{i}setting",           # spec: linesetting as recorded (Q15)
            plat_status = "plat_sen_v1",           # p.10: platinum status at the row's index line (spec: "the main definition")
            psoc_values = "Platinum Sensitive",
            proc_values = c("Platinum Resistant", "Platinum Refractory",
                            "Primary platinum resistant/refractory in a prior LOT"),
            plat_other_values = "Not Eligible"))

# ---- Data loading -------------------------------------------------------------
connect <- function() {
  pwd <- Sys.getenv("DATABRICKS_PWD")
  if (!nzchar(pwd)) stop("Set the DATABRICKS_PWD environment variable.", call. = FALSE)
  con <- DBI::dbConnect(odbc::odbc(), dsn = "RWDE", pwd = pwd, timeout = 120, bigint = "numeric")
  invisible(DBI::dbExecute(con, "USE CATALOG hive_metastore"))
  con
}

# A Databricks table name: schema.table or catalog.schema.table, parts optionally
# `quoted`, and not a file name
is_table_name <- function(src) grepl("^(`[^`]+`|[A-Za-z0-9_]+)(\\.(`[^`]+`|[A-Za-z0-9_]+)){1,2}$", src) &&
  !grepl("\\.(rds|csv|parquet|sas7bdat|xpt|txt|xlsx?|feather|json)$", src, ignore.case = TRUE)

read_rwdp <- function(src) {
  d <- if (grepl("\\.rds$", src, ignore.case = TRUE)) readRDS(src)
       else if (grepl("\\.parquet$", src, ignore.case = TRUE)) arrow::read_parquet(src)
       else if (grepl("\\.csv$", src, ignore.case = TRUE)) utils::read.csv(src, stringsAsFactors = FALSE)
       else if (dir.exists(src) || !is_table_name(src))
         stop("RWDP source ", src, ": not an .rds, .parquet or .csv file, nor a Databricks table schema.table.", call. = FALSE)
       else {
         con <- connect(); on.exit(DBI::dbDisconnect(con))
         DBI::dbGetQuery(con, paste("SELECT * FROM", src))
       }
  d <- as.data.frame(d)
  # 64-bit integers (e.g. saved from an odbc pull with bigint = "integer64") are
  # garbage unless bit64 is loaded: convert them to numbers, or to exact text when a
  # value is too large for a double (e.g. a 64-bit ID)
  i64 <- vapply(d, inherits, logical(1), "integer64")
  if (any(i64)) {
    if (!requireNamespace("bit64", quietly = TRUE))
      stop("RWDP has integer64 columns (", paste(names(d)[i64], collapse = ", "), "): install bit64.", call. = FALSE)
    d[i64] <- lapply(d[i64], function(x) if (all(abs(x) < 2^53, na.rm = TRUE)) as.numeric(x) else as.character(x))
  }
  rename_with(as_tibble(d), tolower)
}

# ---- Helpers ------------------------------------------------------------------
# Dates from Date, POSIXct (its date in its own time zone, UTC if it has none:
# printing it in the session's zone could shift it a day) or "YYYY-MM-DD..." text
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) { tz <- attr(x, "tzone"); return(as.Date(format(x, "%Y-%m-%d", tz = if (length(tz) && nzchar(tz[1])) tz[1] else "UTC"))) }
  as.Date(substr(as.character(x), 1, 10))
}
# Patient IDs as text; numbers without scientific notation (100000, not 1e+05)
id_chr <- function(x) if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
yn <- function(x) {
  x <- toupper(trimws(as.character(x)))
  case_when(x %in% c("Y", "YES", "1", "TRUE")  ~ "Y",
            x %in% c("N", "NO", "0", "FALSE") ~ "N",
            TRUE ~ NA_character_)
}
fmt_date <- function(x) sub("^0", "", format(x, "%d %B %Y"))
`%||%` <- function(a, b) if (is.null(a)) b else a
# Disclosure control: counts as 0, "<5" (min_cell), or rounded to 10 (at least 10)
mask <- function(n) ifelse(is.na(n), NA_character_, ifelse(n == 0, "0", ifelse(n < min_cell, paste0("<", min_cell),
                    format(pmax(10, round(n / 10) * 10), scientific = FALSE, trim = TRUE))))
# Counts in an error message (e.g. "1 extra row(s)") masked the same way
mask_msg <- function(m) {
  r <- gregexpr("\\b[0-9]+(?= (patient|extra))", m, perl = TRUE)
  regmatches(m, r) <- lapply(regmatches(m, r), function(x) mask(as.numeric(x)))
  m
}
# Lower case, each run of other characters to one space: "Ado-Trastuzumab" -> "ado trastuzumab"
norm <- function(x) trimws(gsub("[^a-z0-9]+", " ", tolower(x)))
# The drugs of each line name, normalised: "Carboplatin,Paclitaxel" -> c("carboplatin", "paclitaxel")
drugs_of <- function(name) lapply(strsplit(coalesce(name, ""), ","), function(x) { x <- norm(x); x[x != ""] })
agent_re <- function(agents) paste0("\\b(", paste(agents, collapse = "|"), ")\\b")
has_any  <- function(drugs, agents) any(grepl(agent_re(agents), drugs))
is_platinum <- function(name) vapply(drugs_of(name), has_any, logical(1), agents = platinum_agents)

# EC criterion 3: classify the 1L line
ec_1l_class <- function(name) vapply(drugs_of(name), function(d) {
  chemo <- grepl(agent_re(chemo_agents), d)
  io    <- grepl(agent_re(io_agents), d)
  if (!any(chemo)) "No chemotherapy"
  else if (all(chemo | io)) "Chemotherapy alone or with IO"
  else "Chemotherapy with other agents"
}, character(1))
ec_other_agents <- function(name) vapply(drugs_of(name), function(d)
  paste(d[!grepl(agent_re(c(chemo_agents, io_agents)), d)], collapse = ", "), character(1))

# ---- ADCs (Appendix A) ----------------------------------------------------------
# Each agent is matched on its generic name, aliases and brand/short name, normalised,
# and also with the spaces removed ("HS-20089" also matches "HS20089").
# Appendix A of the form: generic name, aliases, brand/short name
adc_list <- data.frame(
 generic =
  c("Trastuzumab emtansine", "Trastuzumab deruxtecan", "Sacituzumab govitecan", "Datopotamab deruxtecan",
    "Telisotuzumab vedotin", "Enfortumab vedotin", "Tisotumab vedotin", "Mirvetuximab soravtansine",
    "Brentuximab vedotin", "Polatuzumab vedotin", "Inotuzumab ozogamicin", "Gemtuzumab ozogamicin",
    "Loncastuximab tesirine", "Belantamab mafodotin", "Pivekimab sunirine", "Disitamab vedotin",
    "Raludotatug deruxtecan", "Ifinatamab deruxtecan", "Patritumab deruxtecan", "Sacituzumab tirumotecan",
    "Trastuzumab rezetecan", "Risvutatug rezetecan", "Rinatabart sesutecan", "Puxitatug samrotecan",
    "Sigvotatug vedotin", "Zilovertamab vedotin", "Luveltamab tazevibulin", "Upifitamab rilsodotin",
    "Farletuzumab ecteribulin", "HS-20089"),
 aliases =
  c("T-DM1", "T-DXd", NA, "Dato-DXd", "Teliso-V", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "RC48", NA, NA,
    "HER3-DXd", "MK-2870", NA, "GSK5764227; HS-20093", NA, "AZD8205", "SGN-B6A; PF-08046047",
    "MK-2140; VLS-101", "STRO-002", "XMT-1536", "MORAb-202", NA),
 brand_short =
  c("Kadcyla", "Enhertu", "Trodelvy", "Datroway", "Emrelis", "Padcev", "Tivdak", "Elahere", "Adcetris",
    "Polivy", "Besponsa", "Mylotarg", "Zynlonta", "Blenrep", "Decnupaz", "Disitamab vedotin", "R-DXd",
    "I-DXd", "HER3-DXd", "Sac-TMT", "SHR-A1811", "GSK227", "Rina-S", "Puxi-S", "Sigvotatug vedotin",
    "Zilovertamab vedotin", "Luveltamab tazevibulin", "Upifitamab rilsodotin", "Farletuzumab ecteribulin",
    "HS-20089"),
 stringsAsFactors = FALSE)
adc_terms <- adc_list %>%
  transmute(generic, term = paste(generic, coalesce(aliases, ""), coalesce(brand_short, ""), sep = ";")) %>%
  separate_rows(term, sep = ";") %>%
  mutate(term = norm(term)) %>%
  filter(term != "")
adc_terms <- distinct(bind_rows(adc_terms, mutate(adc_terms, term = gsub(" ", "", term))))

# Appendix A agents in each line name ("" if none). A term must be whole words within
# one drug of the line: "fam trastuzumab deruxtecan nxki" holds "trastuzumab
# deruxtecan", while plain trastuzumab matches nothing.
adc_in <- function(name) {
  u <- unique(name)
  hits <- vapply(drugs_of(u), function(d) {
    d <- paste0(" ", d, " ")
    found <- vapply(adc_terms$term, function(t) any(grepl(paste0(" ", t, " "), d, fixed = TRUE)), logical(1))
    paste(unique(adc_terms$generic[found]), collapse = "; ")
  }, character(1))
  hits[match(name, u)]
}
adc_stem_in <- function(name) grepl(paste(adc_stems, collapse = "|"), norm(coalesce(name, "")))
is_study_drug <- function(name) grepl("clinical study drug", norm(coalesce(name, "")))

# ---- Standardise an RWDP: one row per patient, one row per patient per line -----
# Handles both layouts in rwdp_vars: one row per patient, or one row
# per patient per cohort (the patient row plus rows indexed at each line).
# rwdp_vars entries are read with [[ ]] (exact names): $ would partially match,
# e.g. v$age would return age_date.
# pts:   patientid, age, init_dx, surg_days (days from initial diagnosis to the
#        surgery, or to the first of surgery and 1L), visit_gap (TRUE = gap > 180
#        days, FALSE = none, NA = no value)
# lines: patientid, line, name, start, end, plat (raw platinum status), psoc, proc (Y/N/NA)
standardise <- function(d, v, cancer) {
  fail <- function(...) stop(cancer, " RWDP: ", ..., call. = FALSE)
  need <- function(cols) {
    miss <- setdiff(cols, names(d))
    if (length(miss))
      fail("column(s) ", paste(miss, collapse = ", "), " not found. Update rwdp_vars ",
           "(the Columns sheet lists them). Columns in the RWDP:\n  ", paste(sort(names(d)), collapse = ", "))
  }
  is_tmpl  <- function(x) !is.null(x) && grepl("{i}", x, fixed = TRUE)
  line_col <- function(tmpl, i) gsub("{i}", i, tmpl, fixed = TRUE)
  num      <- function(x) suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))
  year_of  <- function(x) as.numeric(format(to_date(x), "%Y"))
  pid <- v[["patientid"]]; cohort <- v[["cohort"]]; line_rows <- v[["line_rows"]]
  idx_dt <- if (is.null(cohort)) NULL else v[["index_dt"]]          # row settings only matter with a cohort layout
  by_idx <- !is.null(idx_dt) && !isFALSE(v[["match_by_index"]])     # find line rows by index date
  plat   <- v[["plat_status"]]
  if (nrow(d) == 0) fail("no rows.")
  # Rows of the sources the RDQ uses (e.g. EC: db = EDM); rows and patients dropped are counted
  kept_rows <- NULL
  if (length(v[["keep_rows"]])) {
    need(names(v[["keep_rows"]]))
    keep <- Reduce(`&`, Map(function(cn, vals) toupper(trimws(as.character(d[[cn]]))) %in% toupper(vals),
                            names(v[["keep_rows"]]), v[["keep_rows"]]))
    all0 <- id_chr(d[[v[["patientid"]]]])
    kept_rows <- c(rows_dropped = sum(!keep), patients_dropped = length(setdiff(all0[!keep], all0[keep])),
                   patients_in_both = length(intersect(all0[!keep], all0[keep])))
    d <- d[keep, , drop = FALSE]
    if (nrow(d) == 0) fail("no rows left after keep_rows (", paste(names(v[["keep_rows"]]), collapse = ", "), ").")
  }
  if (is.null(v[["age"]]) && (is.null(v[["birthyear"]]) || is.null(v[["age_date"]])))
    fail("rwdp_vars needs birthyear and age_date, or age (with age_date and index_dt to move it to age_date).")
  if (is.null(v[["birthyear"]]) && !is.null(v[["age"]]) && !is.null(v[["age_date"]]) && is.null(idx_dt))
    fail("age with age_date needs the cohort layout's index_dt (or give birthyear).")
  if (!is.null(v[["visit_gap_values"]]) && !all(c("gap", "no_gap") %in% names(v[["visit_gap_values"]])))
    fail("visit_gap_values needs c(gap = ..., no_gap = ..., missing = ...), or none for a number of days.")
  if (!all(vapply(v[c("lot_name", "lot_sd", "lot_ed")], is_tmpl, logical(1)))) fail("lot_name, lot_sd and lot_ed need {i}.")
  if (!is.null(plat) && !length(c(v[["psoc_values"]], v[["proc_values"]])))
    fail("plat_status needs psoc_values and proc_values (and plat_other_values for N on both).")
  if (is.null(cohort) && !is.null(plat) && !is_tmpl(plat))
    fail("plat_status without {i} is read on the row of each line, which needs the cohort layout; ",
         "with one row per patient name it per line, e.g. lot{i}plat.")
  if (!is.null(cohort) && length(line_rows) < 2) fail("line_rows needs the values of the rows of lines 1 and 2 at least.")
  if (!is.null(cohort) && !is.null(plat) && !is_tmpl(plat) && length(line_rows) < max(lines_out))
    fail("plat_status without {i} is read on each line's row: line_rows needs the rows of lines 1 to ", max(lines_out), ".")
  need(c(pid, cohort, idx_dt, unlist(v[c("age", "birthyear", "age_date", "init_dx", "surg_dt", "first_tx_days", "visit_gap")]),
         if (!is.null(plat) && !is_tmpl(plat)) plat))

  # The patient row of each patient: the overall_row row, or, for a patient with
  # none, the patient's first row (patient-level variables are the same on every
  # row). With one row per patient, that row.
  coh  <- if (is.null(cohort)) NULL else as.character(d[[cohort]])
  all_ids <- id_chr(d[[pid]])
  if (is.null(coh)) {
    base <- d; ids <- all_ids; no_patient_row <- NULL
    if (anyDuplicated(ids))
      fail(length(unique(ids[duplicated(ids)])), " patient(s) appear more than once (", sum(duplicated(ids)), " extra rows)",
           "; rwdp_vars says one row per patient (cohort = NULL). Resolve this before counting.")
  } else {
    is_pt <- coh %in% as.character(v[["overall_row"]])
    pt_ids <- all_ids[is_pt]
    if (anyDuplicated(pt_ids))
      fail(length(unique(pt_ids[duplicated(pt_ids)])), " patient(s) appear more than once (", sum(duplicated(pt_ids)),
           " extra rows) among the patient rows (", cohort, " = ", v[["overall_row"]], "). Resolve this before counting.")
    if (!any(is_pt)) fail("no patient rows (", cohort, " = ", v[["overall_row"]], "). Check cohort and overall_row in rwdp_vars.")
    k <- which(is_pt | (!all_ids %in% pt_ids & !duplicated(all_ids)))
    base <- d[k, , drop = FALSE]; ids <- all_ids[k]
    no_patient_row <- sum(!is_pt[k])
    for (lr in line_rows) {
      rid <- all_ids[coh %in% as.character(lr)]
      if (anyDuplicated(rid)) fail(sum(duplicated(rid)), " extra row(s) for patients at ", cohort, " = ", lr, ".")
    }
  }

  # Lines 1, 2, ... while a start-date column exists. A {i} variable is repeated on
  # every row of a patient, so it is read on the patient row.
  n_lines <- sum(cumall(vapply(1:50, function(i) line_col(v[["lot_sd"]], i) %in% names(d), logical(1))))
  if (n_lines < 2) fail("no ", line_col(v[["lot_sd"]], 1), " and ", line_col(v[["lot_sd"]], 2),
                        " columns. Update rwdp_vars.")
  start_of <- function(i) to_date(base[[line_col(v[["lot_sd"]], i)]])

  # The row of line i: the line row whose index date is line i's start (with
  # index_dt); if none has it (or without index_dt), the row at line_rows[i].
  # how = "date", "position" or NA (no row).
  line_rows_d <- if (is.null(coh)) NULL else d[coh %in% as.character(line_rows), , drop = FALSE]
  lr_ids <- if (is.null(coh)) NULL else id_chr(line_rows_d[[pid]])
  find_row <- function(i) {
    by_pos <- if (i > length(line_rows)) rep(NA_integer_, length(ids)) else {
      k <- which(as.character(line_rows_d[[cohort]]) == as.character(line_rows[i]))
      k[match(ids, lr_ids[k])]
    }
    st <- start_of(i)
    by_date <- if (!by_idx) rep(NA_integer_, length(ids)) else {
      key <- paste(lr_ids, to_date(line_rows_d[[idx_dt]]))
      pos <- seq_len(nrow(line_rows_d))
      if (i <= length(line_rows))             # same index date at two rows: prefer the row at line_rows[i]
        pos <- pos[order(as.character(line_rows_d[[cohort]]) != as.character(line_rows[i]))]
      replace(pos[match(paste(ids, st), key[pos])], is.na(st), NA_integer_)
    }
    row <- ifelse(is.na(by_date), by_pos, by_date)
    row[is.na(st)] <- NA_integer_                    # no such line: no row
    list(row = row, how = ifelse(!is.na(by_date), "date", ifelse(!is.na(row), "position", NA_character_)))
  }
  rows_cache <- list()
  row_of_line <- function(i) { if (is.null(rows_cache[[as.character(i)]])) rows_cache[[as.character(i)]] <<- find_row(i)
                               rows_cache[[as.character(i)]]$row }
  at_line_row <- function(var, i) if (is.null(coh)) base[[var]] else line_rows_d[[var]][row_of_line(i)]
  row_checks <- if (is.null(coh)) NULL else {
    f2 <- find_row(2); has2 <- !is.na(start_of(2))
    chk <- c(with_2l = sum(has2), no_2l_row = sum(has2 & is.na(f2$row)),
             by_position = if (!by_idx) 0L else sum(f2$how %in% "position"),
             moved_2l_row = sum(!is.na(f2$row) & as.character(line_rows_d[[cohort]][f2$row]) != as.character(line_rows[2])),
             no_patient_row = no_patient_row)
    # Fail closed: if most patients with a 2L have no 2L row, the row settings are wrong
    # and the visit-gap exclusion would silently do nothing
    if (chk[["with_2l"]] >= 10 && chk[["no_2l_row"]] > 0.5 * chk[["with_2l"]])
      fail(chk[["no_2l_row"]], " patient(s) have no 2L row (", if (by_idx) paste0("by ", idx_dt, " or "), "at ",
           cohort, " = ", line_rows[2], "), out of ", chk[["with_2l"]], " patient(s) with a 2L start. The visit-gap ",
           "exclusion would do nothing: check cohort, line_rows and index_dt in rwdp_vars.")
    chk
  }

  # Codes: stop on values the run would otherwise read as no value
  check_codes <- function(x, ok, what) {
    u <- unique(toupper(trimws(as.character(x)))); u <- u[!is.na(u) & u != ""]
    bad <- setdiff(u, toupper(ok))
    if (!length(bad)) return(invisible())
    codes <- length(u) <= 30 && !any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}|^[0-9.]+$", bad))   # not data values
    fail(what, " has ", if (codes) paste0("value(s) ", paste(head(bad, 10), collapse = ", "))
                        else paste(length(bad), "values (not shown: not codes; is it the right column?)"),
         " that rwdp_vars does not recognise. Update rwdp_vars.")
  }
  no_value <- c("", "NA", "MISSING", "<MISSING>", "UNKNOWN")

  line_var <- function(x, i) {
    if (is.null(x)) return(rep(NA, length(ids)))
    if (!is_tmpl(x)) return(at_line_row(x, i))
    cn <- line_col(x, i)
    if (cn %in% names(d)) base[[cn]] else rep(NA, length(ids))       # e.g. no lot1plat: no status at 1L
  }
  no_line <- function(x) { x <- as.character(x); ifelse(toupper(trimws(x)) %in% c("NONE", ""), NA_character_, x) }
  up <- function(x) toupper(trimws(x))
  lines <- bind_rows(lapply(seq_len(n_lines), function(i) {
    need(vapply(v[c("lot_name", "lot_sd", "lot_ed")], line_col, "", i = i))
    if (is_tmpl(plat) && i %in% lines_out) need(line_col(plat, i))            # status of the listed lines
    ps <- trimws(as.character(line_var(plat, i)))
    if (!is.null(plat)) check_codes(ps, c(v[["psoc_values"]], v[["proc_values"]], v[["plat_other_values"]], no_value),
                                    paste0(plat, " (line ", i, ")"))
    known <- !is.na(ps) & !up(ps) %in% no_value
    yn_of <- function(vals) ifelse(!known, NA_character_, ifelse(up(ps) %in% up(vals), "Y", "N"))
    tibble(patientid = ids, line = i, name = no_line(line_var(v[["lot_name"]], i)),
           start = to_date(line_var(v[["lot_sd"]], i)), end = to_date(line_var(v[["lot_ed"]], i)),
           setting = as.character(line_var(v[["lot_setting"]], i)),
           plat = ifelse(known, ps, NA_character_), psoc = yn_of(v[["psoc_values"]]), proc = yn_of(v[["proc_values"]]))
  })) %>%
    filter(!is.na(start) | !is.na(name))              # "None" = no such line

  init <- if (is.null(v[["init_dx"]])) as.Date(rep(NA_character_, length(ids))) else to_date(base[[v[["init_dx"]]]])
  age  <- if (!is.null(v[["birthyear"]])) year_of(base[[v[["age_date"]]]]) - num(base[[v[["birthyear"]]]])
          else if (!is.null(v[["age_date"]]))          # birth year = year(index date) - age, as the RWDP defines age
            year_of(base[[v[["age_date"]]]]) - (year_of(base[[idx_dt]]) - num(base[[v[["age"]]]]))
          else num(base[[v[["age"]]]])
  # Patients whose patient-row index date is not age_date, and whose year differs (the
  # age is moved by year). The EC spec puts the Overall row's index at the advanced
  # diagnosis, so for EC both should be 0.
  age_moved <- if (is.null(v[["birthyear"]]) && !is.null(v[["age_date"]])) {
    a <- to_date(base[[idx_dt]]); b <- to_date(base[[v[["age_date"]]]])
    c(dates = sum(a != b, na.rm = TRUE), years = sum(year_of(a) != year_of(b), na.rm = TRUE))
  }
  surg_days <- if (!is.null(v[["surg_dt"]])) as.numeric(to_date(base[[v[["surg_dt"]]]]) - init)
               else if (!is.null(v[["first_tx_days"]])) num(base[[v[["first_tx_days"]]]])
               else rep(NA_real_, length(ids))
  gv   <- at_line_row(v[["visit_gap"]], 2)                               # on the 2L row
  vals <- v[["visit_gap_values"]]
  visit_gap <- if (!is.null(vals)) {                                     # a flag
    check_codes(gv, c(vals, "Missing"), v[["visit_gap"]])
    gv <- up(as.character(gv))
    ifelse(gv %in% up(vals[["gap"]]), TRUE, ifelse(gv %in% up(vals[["no_gap"]]), FALSE, NA))
  } else {                                                               # a number of days
    g <- num(gv)
    if (inherits(gv, c("Date", "POSIXt")) || any(is.na(g) & !is.na(gv) & !up(as.character(gv)) %in% no_value))
      fail(v[["visit_gap"]], " is not a number of days; give visit_gap_values if it is a flag.")
    g > visit_gap_days
  }
  pts  <- tibble(patientid = ids, age = age, init_dx = init, surg_days = surg_days, visit_gap = visit_gap)
  list(pts = pts, lines = lines, n_lines = n_lines, n_rows = nrow(d), row_checks = row_checks, kept_rows = kept_rows,
       age_moved = age_moved,
       layout = if (is.null(coh)) "one row per patient"
                else paste0("one row per patient per cohort (", cohort, "); patient row ", cohort, " = ", v[["overall_row"]],
                            ", line rows ", paste(line_rows, collapse = ", "),
                            if (by_idx) paste0(", matched to lines by ", idx_dt) else ""))
}

# ---- Eligibility criteria, in the order of the form's attrition tables -----------
# One logical column per criterion; the first is the whole dataset.
criteria <- function(cancer, s, rule = ec_1l_rule, window = l1_window[[cancer]]) {
  if (!rule %in% c("chemo_io_only", "any_chemo")) stop("ec_1l_rule must be chemo_io_only or any_chemo.", call. = FALSE)
  study_end <- study_period[[cancer]][2]
  l1  <- filter(s$lines, line == 1)
  p   <- left_join(s$pts, select(l1, patientid, l1_start = start, l1_name = name), by = "patientid")
  l2  <- filter(s$lines, line == 2, !is.na(start), start <= study_end)
  all_pts <- rep(TRUE, nrow(p))
  age_ok  <- coalesce(p$age >= min_age, FALSE)
  in_win  <- coalesce(p$l1_start >= window[1] & p$l1_start <= window[2], FALSE)
  has_2l  <- p$patientid %in% l2$patientid
  no_gap  <- !coalesce(p$visit_gap, FALSE)          # no value: kept (open question Q4)
  win_lab <- paste0("between ", fmt_date(window[1]), " and ", fmt_date(window[2]), " (inclusive)")
  gap_lab <- paste0("Excluded: patients with any gap in visits longer than ", visit_gap_days,
                    " days between initial diagnosis and end of 2L, per Flatiron LOT rules")

  if (cancer == "EC") {
    cls <- ec_1l_class(p$l1_name)
    chemo_1l <- if (rule == "any_chemo") cls != "No chemotherapy" else cls == "Chemotherapy alone or with IO"
    crit <- setNames(list(all_pts, age_ok, chemo_1l, in_win, has_2l, no_gap), c(
      "Patients with endometrial cancer in the Flatiron endometrial (EDM) dataset (2026 September data cut)",
      paste0("Aged ≥", min_age, " years at the date of advanced EC diagnosis"),
      "Received chemotherapy, alone or in combination with immunotherapy (IO), as 1L treatment",
      paste("1L treatment (adjuvant-extended 1L) initiation date", win_lab),
      "Received 2L treatment (adjuvant-extended 2L)",
      gap_lab))
  } else {
    within <- function(x) coalesce(x >= 0 & x <= oc_l1_days, FALSE)
    l1_or_surg <- within(as.numeric(p$l1_start - p$init_dx)) | within(p$surg_days)
    crit <- setNames(list(all_pts, age_ok, l1_or_surg, is_platinum(p$l1_name), in_win, has_2l, no_gap), c(
      "Patients with ovarian cancer in the Flatiron ovarian cancer Panoramic dataset (2026 September data cut)",
      paste0("Aged ≥", min_age, " years at the date of initial OC diagnosis"),
      paste("Initiated 1L treatment or underwent surgery within", oc_l1_days, "days after initial diagnosis"),
      "Received platinum-containing therapy as 1L treatment",
      paste("1L treatment initiation date", win_lab),
      "Received 2L treatment",
      gap_lab))
  }
  bind_cols(tibble(patientid = p$patientid), as_tibble(crit, .name_repair = "minimal"))
}

# Attrition: patients meeting each criterion and all those above it
attrition <- function(crit) {
  keep <- rep(TRUE, nrow(crit)); n <- integer()
  for (k in names(crit)[-1]) { keep <- keep & crit[[k]]; n <- c(n, sum(keep)) }
  list(table = tibble(`Eligibility criterion` = names(crit)[-1], N = n,
                      `% remaining` = round(100 * n / n[1], 1),
                      `N excluded at this step` = c(NA, -diff(n))),
       ids = crit$patientid[keep])
}

# ---- Settings --------------------------------------------------------------------
# The OC table the form names (Q1a): compared with rwdp_source["OC"] if it can be read
oc_form_table <- "hive_metastore.rwd_flatiron_dataproducts.rwdap_ovar_pano_2026m09_v1"
# Cohort rows other than the patient and line rows (data dictionary p.11, p.21):
# the row indexed at each start-date column
std_rows <- list(EC = c(lot1sd = 2, lot1msd = 3, lot2sd = 4, lot3sd = 5, lot4sd = 6),
                 OC = c(lot1sd = 2, lot1msd = 3, lot2sd = 4, lot2msd = 5, lot3sd = 6, lot4sd = 7, lot5sd = 8))
adj_row  <- c(EC = 7)       # EC "ADJ+ treated" (index at the adjuvant start)
proc_row <- c(OC = 9)       # OC "2L + PROC: index at first platinum resistance"
# Columns of the standard (Flatiron-numbered) lines, in both RWDPs
std_lot  <- list(name = "lot{i}", sd = "lot{i}sd", ed = "lot{i}ed", ld = "lot{i}ld", msd = "lot{i}msd")

# Counts over one denominator d, shown together ("x / y / z of D"): each 0, all,
# "<5", "all but <5", else rounded to 10; with percentages only if d >= min_pct_d
# and no count, rest (d - count) or difference between two counts is 1-4
npcts <- function(ns, d) {
  if (is.na(d) || d == 0) return("n/a (none to count)")
  small  <- function(x) any(x >= 1 & x < min_cell)
  pct_ok <- d >= min_pct_d && !small(ns) && !small(d - ns) && !small(abs(outer(ns, ns, "-")))
  part <- vapply(ns, function(n) if (n == 0) "0" else if (n == d) "all" else if (n < min_cell) paste0("<", min_cell)
                 else if (d - n < min_cell) paste0("all but <", min_cell)
                 else paste0(mask(n), if (pct_ok) paste0(" (", round(100 * n / d, if (d >= 1000) 1 else 0), "%)") else ""), "")
  paste0(paste(part, collapse = " / "), " of ", mask(d))
}

# Every column of an RWDP: type, % of rows with a value (to 5%; "<5 rows" for 1-4)
# and distinct values (exact up to 30, else rounded to 10); no values are shown
column_profile <- function(cancer, d) {
  has <- function(x) if (is.character(x) || is.factor(x)) !is.na(x) & nzchar(trimws(as.character(x))) else !is.na(x)
  tibble(Cancer = cancer, Column = names(d), Type = vapply(d, function(x) class(x)[1], ""),
         `% of rows with a value` = vapply(d, function(x) { k <- sum(has(x))
           if (k > 0 && k < min_cell) paste0("<", min_cell, " rows") else paste0(5 * round(20 * k / max(1, nrow(d))), "%") }, ""),
         `Distinct values` = vapply(d, function(x) { u <- n_distinct(x[has(x)])
           if (u <= 30) as.character(u) else mask(u) }, ""))
}

# All the probes; returns the Probes, Tables and Columns sheets. The probes' working
# variables stay inside this function.
question_probes <- function() {
  # ---- Helpers ---------------------------------------------------------------------
  probes <- list(); tables <- list(); profiles <- list()
  lv    <- function(t, i) gsub("{i}", i, t, fixed = TRUE)                       # lot{i}sd -> lot2sd
  n_of  <- function(d, t) { i <- 0; while (lv(t, i + 1) %in% names(d)) i <- i + 1; i }   # lines 1..n present
  as_n  <- function(x) suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))
  need_cols <- function(d, cn) {
    m <- setdiff(cn, names(d))
    if (length(m)) stop("no column ", paste(m, collapse = ", "), call. = FALSE)
  }
  dcol  <- function(d, cn) { need_cols(d, cn); to_date(d[[cn]]) }
  same  <- function(a, b) !is.na(a) & !is.na(b) & a == b
  lab   <- function(x) { x <- trimws(as.character(x)); ifelse(is.na(x) | x == "", "(no value)", x) }
  npct  <- function(n, d) npcts(n, d)
  add <- function(cancer, q, probe, result, read)
    probes[[length(probes) + 1]] <<- tibble(Cancer = cancer, Question = q, Probe = probe,
                                            Result = paste(result, collapse = "; "), `How to read it` = read)
  # A table of counts (masked) on the Tables sheet; categorical values only
  tab <- function(cancer, q, name, row, col = NULL, max_values = 80) {
    row <- lab(row); col <- if (is.null(col)) rep("", length(row)) else lab(col)
    if (n_distinct(row) > max_values || n_distinct(col) > max_values) stop("too many distinct values to tabulate", call. = FALSE)
    t <- count(tibble(Row = row, Column = col), Row, Column) %>% arrange(Row, Column)
    if (!nrow(t)) return("nothing to tabulate")
    tables[[length(tables) + 1]] <<- tibble(Cancer = cancer, Question = q, Table = name, Row = t$Row, Column = t$Column, N = mask(t$n))
    paste0("see Tables: ", name)
  }
  probe <- function(cancer, q, name, read, expr)
    add(cancer, q, name, tryCatch(expr, error = function(e) paste("not run:", mask_msg(conditionMessage(e)))), read)
  q_order <- c("Layout", "Lines", "ADC", "Q1a", "Q1b", "Q1c", "Q1d", "Q4", "Q1e", "Q5", "Q11", "Q12", "Q13", "Q14",
               "Q15", "Q17")

  raw <- list()
  for (cancer in c("EC", "OC")) {
    v   <- rwdp_vars[[cancer]]
    P   <- function(q, name, read, expr) probe(cancer, q, name, read, expr)
    d_all <- tryCatch(read_rwdp(rwdp_source[[cancer]]), error = function(e) e)
    if (inherits(d_all, "error")) {
      add(cancer, "Layout", "Read the RWDP", paste("not read:", mask_msg(conditionMessage(d_all))), rwdp_source[[cancer]])
      next
    }
    profiles[[cancer]] <- column_profile(cancer, d_all)
    pid <- v[["patientid"]]; coh <- v[["cohort"]]; idx <- v[["index_dt"]]
    miss <- setdiff(c(pid, coh, idx), names(d_all))
    if (is.null(coh) || length(miss)) {
      add(cancer, "Layout", "Layout", if (is.null(coh)) "one row per patient (cohort = NULL)" else paste("no column", paste(miss, collapse = ", ")),
          "the probes need the cohort layout of rwdp_vars")
      next
    }
    raw[[cancer]] <- d_all
    keep <- rep(TRUE, nrow(d_all))                                      # keep_rows (EC: db = EDM)
    for (cn in intersect(names(v[["keep_rows"]]), names(d_all)))
      keep <- keep & toupper(trimws(as.character(d_all[[cn]]))) %in% toupper(v[["keep_rows"]][[cn]])
    d   <- d_all[keep, , drop = FALSE]
    ids <- id_chr(d[[pid]])
    first_row <- function(x) x[!duplicated(id_chr(x[[pid]])), , drop = FALSE]
    rows_of   <- function(k, from = d) first_row(from[from[[coh]] %in% k, , drop = FALSE])
    pr  <- rows_of(v[["overall_row"]])                                  # one patient row per patient
    nl  <- n_of(pr, v[["lot_sd"]])
    sd_of <- function(i, x = pr) dcol(x, lv(v[["lot_sd"]], i))
    gap <- v[["visit_gap"]]

    # ---- Layout: rows, patient and line rows ----------------------------------------
    P("Layout", "Rows / distinct patients: all rows; rows read (keep_rows)", "exact counts, rounded (the dictionary's distinct counts may be approximate)",
      paste0(mask(nrow(d_all)), " / ", mask(n_distinct(id_chr(d_all[[pid]]))), "; read: ", mask(nrow(d)), " / ", mask(n_distinct(ids))))
    P("Layout", paste("Rows by", coh), "the cohort sizes of the dictionary", tab(cancer, "Layout", paste("rows by", coh, "(rows read)"), d[[coh]]))
    P("Layout", "Patients with line i starting in the index window (minindex-maxindex, if present): with no row for line i (line_rows)",
      paste("0 expected: rows exist for index dates in the RWDP's index window (EC from 2013, OC from 2017), so a line starting",
            "earlier has no row; patients without a patient row are not counted (Q1b). The visit gap is read on the 2L row."), {
      win <- all(c("minindex", "maxindex") %in% names(pr))
      paste(vapply(seq_along(v[["line_rows"]]), function(k) {
        s <- sd_of(k); inw <- !is.na(s)
        if (win) inw <- inw & s >= dcol(pr, "minindex") & s <= dcol(pr, "maxindex")
        has_row <- id_chr(pr[[pid]]) %in% id_chr(rows_of(v[["line_rows"]][k])[[pid]])
        paste0("line ", k, " (", lv(v[["lot_sd"]], k), ", row ", v[["line_rows"]][k], "): ", npct(sum(inw & !has_row), sum(inw)))
      }, ""), collapse = "; ")
    })
    P("Layout", "Line rows indexed at their line's start", "all expected: the code finds the row of line i by this index date (else by position, counted on Data checks)",
      paste(vapply(seq_along(v[["line_rows"]]), function(k) {
        r <- rows_of(v[["line_rows"]][k])
        paste0("row ", v[["line_rows"]][k], " at ", lv(v[["lot_sd"]], k), ": ", npct(sum(same(dcol(r, idx), sd_of(k, r))), nrow(r)))
      }, ""), collapse = "; "))
    P("Layout", "Other cohort rows indexed at their start column", "all expected (dictionary cohorts)", {
      sr <- std_rows[[cancer]]
      if (cancer %in% names(adj_row)) { ac <- grep("^adj(t)?sd$", names(d), value = TRUE)[1]; if (!is.na(ac)) sr <- c(sr, setNames(adj_row[[cancer]], ac)) }
      paste(vapply(names(sr), function(cn) {
        r <- rows_of(sr[[cn]])
        if (!cn %in% names(r)) paste0("row ", sr[[cn]], ": no column ", cn)
        else paste0("row ", sr[[cn]], " at ", cn, ": ", npct(sum(same(dcol(r, idx), dcol(r, cn))), nrow(r)))
      }, ""), collapse = "; ")
    })

    # ---- Lines: dates, numbering, names ----------------------------------------------
    P("Lines", "Line columns / patients with each line", "", {
      paste0(nl, " lines (", lv(v[["lot_sd"]], 1), "...); ", paste0(seq_len(min(nl, 8)), "L ",
             vapply(seq_len(min(nl, 8)), function(i) mask(sum(!is.na(sd_of(i)))), ""), collapse = ", "))
    })
    P("Lines", "Lines ending before they start", "0 expected", {
      n <- 0; tot <- 0
      for (i in seq_len(nl)) { s <- sd_of(i); e <- dcol(pr, lv(v[["lot_ed"]], i)); n <- n + sum(e < s, na.rm = TRUE); tot <- tot + sum(!is.na(s) & !is.na(e)) }
      npct(n, tot)
    })
    P("Lines", "Line i+1 starting before line i starts / on or before line i's end", "0 expected for the first (lines numbered in date order); the second shows overlapping lines", {
      a <- b <- tot <- 0
      for (i in seq_len(nl - 1)) { s1 <- sd_of(i); s2 <- sd_of(i + 1); e1 <- dcol(pr, lv(v[["lot_ed"]], i))
        a <- a + sum(s2 < s1, na.rm = TRUE); b <- b + sum(s2 <= e1, na.rm = TRUE); tot <- tot + sum(!is.na(s1) & !is.na(s2)) }
      npcts(c(a, b), tot)
    })
    P("Lines", "Patients with a line but not the line before it (numbering gap)", "0 expected", {
      gap_pt <- rep(FALSE, nrow(pr)); has_any <- rep(FALSE, nrow(pr))
      for (i in seq_len(nl)) { h <- !is.na(sd_of(i)); has_any <- has_any | h; if (i > 1) gap_pt <- gap_pt | (h & is.na(sd_of(i - 1))) }
      npct(sum(gap_pt), sum(has_any))
    })
    P("Lines", "Lines with a start but no name or 'None' / a name but no start", "0 expected", {
      a <- b <- tot <- 0
      for (i in seq_len(nl)) { s <- sd_of(i); need_cols(pr, lv(v[["lot_name"]], i)); nm <- pr[[lv(v[["lot_name"]], i)]]
        no_nm <- is.na(nm) | toupper(trimws(nm)) %in% c("", "NONE")
        a <- a + sum(!is.na(s) & no_nm); b <- b + sum(is.na(s) & !no_nm); tot <- tot + sum(!is.na(s) | !no_nm) }
      npcts(c(a, b), tot)
    })
    P("Lines", "Lines 2-5 starting after the study period end / with no end date", "Q8: 0 expected for the first", {
      a <- b <- tot <- 0
      for (i in intersect(2:5, seq_len(nl))) { s <- sd_of(i); e <- dcol(pr, lv(v[["lot_ed"]], i))
        a <- a + sum(s > study_period[[cancer]][2], na.rm = TRUE); b <- b + sum(!is.na(s) & is.na(e)); tot <- tot + sum(!is.na(s)) }
      npcts(c(a, b), tot)
    })
    # Names of lines 2-5 (with a start) of all patients; an error is reported by the probes using them
    nm25 <- tryCatch(unlist(lapply(intersect(2:5, seq_len(nl)), function(i) {
      need_cols(pr, lv(v[["lot_name"]], i)); nm <- pr[[lv(v[["lot_name"]], i)]]; nm[!is.na(sd_of(i))] })),
      error = function(e) e)
    names25 <- function() if (inherits(nm25, "error")) stop(conditionMessage(nm25), call. = FALSE) else nm25
    P("Lines", "Lines 2-5 with a clinical study drug (all patients)", "ADC status unknown for these lines",
      { x <- names25(); npct(sum(is_study_drug(x)), length(x)) })

    # ---- ADC names in lines 2-5 of all patients -------------------------------------
    P("ADC", "Lines 2-5 matching an Appendix A ADC, by agent (all patients)", "the ADCs seen in the RWDP; an agent never seen may be named differently",
      { a <- adc_in(names25()); tab(cancer, "ADC", "Lines 2-5 by Appendix A agent matched (all patients)", a[a != ""]) })
    P("ADC", "Line names (2-5) with an ADC payload stem but no Appendix A match (all patients)", "possible ADCs missing from Appendix A or spelled differently: please review",
      { x <- names25(); x <- x[adc_stem_in(x) & adc_in(x) == ""]
        if (!length(x)) "none" else tab(cancer, "ADC", "Line names with an ADC stem not in Appendix A", x, max_values = 300) })

    # ---- Q1b: one patient row per patient ---------------------------------------------
    P("Q1b", "Patients with more than one patient row (overall_row), rows read", "0 expected: one row per patient per cohort (both specs)",
      { n <- table(id_chr(d[[pid]][d[[coh]] %in% v[["overall_row"]]])); npct(sum(n > 1), length(n)) })
    if (length(v[["keep_rows"]]))
      P("Q1b", "Patients with more than one patient row, all rows (keep_rows not applied)", "more than in the rows read = patients also in the other rows (Q14)",
        { n <- table(id_chr(d_all[[pid]][d_all[[coh]] %in% v[["overall_row"]]])); npct(sum(n > 1), length(n)) })
    P("Q1b", "Patients with no patient row", "0 expected: every patient is in the Overall cohort",
      npct(sum(!unique(ids) %in% id_chr(pr[[pid]])), n_distinct(ids)))
    P("Q1b", "Patients with more than one row in a line cohort (line_rows)", "0 expected (two rows at one line cohort stop the run)", {
      r <- d[d[[coh]] %in% v[["line_rows"]], , drop = FALSE]; k <- paste(id_chr(r[[pid]]), r[[coh]])
      npct(n_distinct(id_chr(r[[pid]])[duplicated(k)]), n_distinct(id_chr(r[[pid]])))
    })

    # ---- Q1c: age ----------------------------------------------------------------------
    P("Q1c", "Rows where age = year(index date) - birth year", "all expected (both specs: AGE = YEAR(INDEXDT) - birthyear)", {
      need_cols(d, c("age", "birthyear"))
      a <- as_n(d$age); b <- as_n(d$birthyear); y <- as.numeric(format(dcol(d, idx), "%Y"))
      npct(sum(same(a, y - b)), sum(!is.na(a) & !is.na(b) & !is.na(y)))
    })
    P("Q1c", "Birth-year-like columns", "EC spec: birthyear is to be kept (5A. Demos); the dictionary has none for EC",
      { m <- grep("birth|yob|dob", names(d), value = TRUE); if (length(m)) paste(m, collapse = ", ") else "none" })
    if (!is.null(v[["age_date"]]))
      P("Q1c", paste0("Patient rows with ", idx, " = ", v[["age_date"]], " / in another year"),
        if (cancer == "EC") "EC spec: the Overall row is indexed at the advanced diagnosis: all / 0 expected (the code moves age by year otherwise)"
        else "OC spec: the Overall row is indexed at the initial diagnosis: all / 0 expected", {
        a <- dcol(pr, idx); b <- dcol(pr, v[["age_date"]])
        npcts(c(sum(same(a, b)), sum(!is.na(a) & !is.na(b) & format(a, "%Y") != format(b, "%Y"))), nrow(pr))
      })

    # ---- Q1d / Q4: visit gap -------------------------------------------------------------
    gdays <- if (is.null(v[["visit_gap_values"]])) gap else "visit_gap_max_v1"   # the gap in days
    P(if (cancer == "EC") "Q1d" else "Q4", paste(gdays, "by cohort: no value / <= 180 / > 180 (rows read)"), "no value = fewer than 2 visits or a missing window date (specs)",
      { need_cols(d, gdays); g <- as_n(d[[gdays]])
        tab(cancer, if (cancer == "EC") "Q1d" else "Q4", paste(gdays, "by", coh, "(rows read)"), d[[coh]],
            ifelse(is.na(g), "no value", ifelse(g > visit_gap_days, "> 180", "<= 180"))) })
    P(if (cancer == "EC") "Q1d" else "Q4", paste0(gdays, " on the 2L row vs the patient row: 2L < / = / > patient row; then 2L row >= 1L row"),
      paste("All windows start at initial diagnosis and use the same visits. Some patients with 2L < patient row show that the 2L row's window",
            "ends before the patient row's (the end of 2L vs follow-up end, as in the spec's treated cohorts); none among many would mean the",
            "same window. '=' is expected for most either way (the largest gap often comes before 2L). '>' should be 0 (nested windows),",
            "and 2L row >= 1L row all (the 2L window contains the 1L one)."), {
      need_cols(d, gdays)
      g_of <- function(k) { r <- rows_of(k); setNames(as_n(r[[gdays]]), id_chr(r[[pid]])) }
      g0 <- g_of(v[["overall_row"]]); g1 <- g_of(v[["line_rows"]][1]); g2 <- g_of(v[["line_rows"]][2])
      b0 <- intersect(names(g2), names(g0)); b0 <- b0[!is.na(g2[b0]) & !is.na(g0[b0])]
      b1 <- intersect(names(g2), names(g1)); b1 <- b1[!is.na(g2[b1]) & !is.na(g1[b1])]
      paste0(npcts(c(sum(g2[b0] < g0[b0]), sum(g2[b0] == g0[b0]), sum(g2[b0] > g0[b0])), length(b0)), "; ",
             npct(sum(g2[b1] >= g1[b1]), length(b1)))
    })
    if (cancer == "EC")
      P("Q1d", paste0(gap, ": AdjX-L2 row vs the standard line row indexed at the same date (same gap / patients)"),
        paste("The AdjX-L2 line is the standard L2 for most patients, and L1 for those with an adjuvant line numbered first. Same gap",
              "expected for both if the AdjX-L2 row's window ends at the end of its index line, as the standard rows' do; a different",
              "gap = a different window end."), {
        r2 <- rows_of(v[["line_rows"]][2]); g2 <- as_n(r2[[gap]])
        sr <- std_rows$EC[!grepl("msd$", names(std_rows$EC))]
        paste(vapply(names(sr), function(cn) {
          rs <- rows_of(sr[[cn]]); m <- match(id_chr(r2[[pid]]), id_chr(rs[[pid]])); gs <- as_n(rs[[gap]])[m]
          at <- same(dcol(r2, idx), dcol(rs, idx)[m]) & !is.na(g2) & !is.na(gs)
          paste0("with row ", sr[[cn]], " (", cn, "): ", npct(sum(at & g2 == gs), sum(at)))
        }, ""), collapse = "; ")
      })
    if (!is.null(v[["visit_gap_values"]])) {
      for (vg in c("visit_gap_max_v1", "visit_gap_max_v2"))
        P("Q4", paste(gap, "x", vg, "on the L2 rows"),
          "if 'No' = > 180, 'Yes' = <= 180 and 'Missing' = no value, the flag is this version and 'Missing' = fewer than 2 visits", {
          r <- rows_of(v[["line_rows"]][2]); need_cols(r, c(gap, vg)); g <- as_n(r[[vg]])
          tab(cancer, "Q4", paste(gap, "(rows) x", vg, "(columns), L2 rows"), r[[gap]],
              ifelse(is.na(g), "no value", ifelse(g > visit_gap_days, "> 180", "<= 180")))
        })
    }

    # ---- Q1e / Q5: OC surgery ------------------------------------------------------------
    if (!is.null(v[["surg_dt"]])) {
      sc <- v[["surg_dt"]]
      for (cat_col in grep("^(surg|issurg[a-z]*)$", names(pr), value = TRUE))
        P("Q1e", paste(sc, "present by", cat_col, "(patient rows)"), "if the date is present only with debulking surgery (PCS / ICS), it is the debulking date",
          tab(cancer, "Q1e", paste(cat_col, "(rows) x", sc, "present (columns), patient rows"), pr[[cat_col]],
              ifelse(is.na(dcol(pr, sc)), paste("no", sc), sc)))
      P("Q5", paste("Days from", v[["init_dx"]], "to", sc, "(patient rows)"), "the criterion takes 0-120; negative = surgery dated before the recorded diagnosis", {
        dd <- as.numeric(dcol(pr, sc) - dcol(pr, v[["init_dx"]]))
        tab(cancer, "Q5", paste("days from", v[["init_dx"]], "to", sc), cut(dd, c(-Inf, -31, -1, 120, 365, Inf),
            labels = c("< -30", "-30 to -1", "0 to 120", "121 to 365", "> 365")))
      })
      P("Q5", "tt_first_tx_diag = min(surgerydate, lot1sd) - init (patient rows with both); tt_first_tx_diag negative",
        "all expected if tt_first_tx_diag is the spec's formula with negatives kept (the spec sets negatives to missing)", {
        need_cols(pr, "tt_first_tx_diag")
        t <- as_n(pr$tt_first_tx_diag); first <- pmin(dcol(pr, sc), sd_of(1), na.rm = TRUE)
        calc <- as.numeric(first - dcol(pr, v[["init_dx"]]))
        paste0(npct(sum(same(t, calc)), sum(!is.na(t) & !is.na(calc))), "; ", npct(sum(t < 0, na.rm = TRUE), sum(!is.na(t))))
      })
      P("Q5", "Criterion 3 by the code (1L or surgery 0-120 days after init) vs tt_first_tx_diag 0-120: both / code only / tt only / neither",
        "the patients for whom the two rules disagree (patient rows with init)", {
        need_cols(pr, "tt_first_tx_diag")
        init <- dcol(pr, v[["init_dx"]]); ok <- !is.na(init)
        within <- function(x) !is.na(x) & x >= 0 & x <= oc_l1_days
        code <- within(as.numeric(sd_of(1) - init)) | within(as.numeric(dcol(pr, sc) - init)); tt <- within(as_n(pr$tt_first_tx_diag))
        npcts(c(sum(ok & code & tt), sum(ok & code & !tt), sum(ok & !code & tt), sum(ok & !code & !tt)), sum(ok))
      })
    }

    # ---- Q11: maintenance ------------------------------------------------------------------
    for (pre in unique(c(sub("\\{i\\}sd$", "", v[["lot_sd"]]), "lot"))) {
      mt <- paste0(pre, "{i}msd")
      P("Q11", paste0("Patients with maintenance after line i (", mt, "), of those with line i"), "maintenance is not listed (default); how many lines it concerns", {
        need_cols(pr, lv(mt, 1))
        paste(vapply(seq_len(min(5, n_of(pr, mt), n_of(pr, paste0(pre, "{i}sd")))), function(i) {
          s <- dcol(pr, lv(paste0(pre, "{i}sd"), i)); paste0(i, "L ", npct(sum(!is.na(s) & !is.na(dcol(pr, lv(mt, i)))), sum(!is.na(s))))
        }, ""), collapse = "; ")
      })
    }

    # ---- Q12: LOT end date (last administration) vs Flatiron line end -------------------
    for (pre in unique(c(sub("\\{i\\}sd$", "", v[["lot_sd"]]), "lot"))) {
      P("Q12", paste0(pre, "{i}ed vs ", pre, "{i}ld, lines 1-5: = / before / after; median days ed to ld"),
        "spec: ed = last administration, ld = Flatiron's line end, so ed <= ld expected", {
        need_cols(pr, lv(paste0(pre, "{i}ld"), 1))
        paste(vapply(seq_len(min(5, n_of(pr, paste0(pre, "{i}ld")))), function(i) {
          e <- dcol(pr, lv(paste0(pre, "{i}ed"), i)); l <- dcol(pr, lv(paste0(pre, "{i}ld"), i)); b <- !is.na(e) & !is.na(l)
          dd <- as.numeric(l - e)[b & l > e]
          paste0(i, "L ", npcts(c(sum(same(e, l)), sum(e < l, na.rm = TRUE), sum(e > l, na.rm = TRUE)), sum(b)),
                 if (length(dd) >= min_cell) paste0("; median ", median(dd), " days") else "")
        }, ""), collapse = " | ")
      })
    }

    # ---- Q13: platinum status (OC) -----------------------------------------------------------
    plat <- v[["plat_status"]]
    if (!is.null(plat)) {
      P("Q13", paste(plat, "by cohort (rows)"), "the status on each cohort's row",
        { need_cols(d, plat); tab(cancer, "Q13", paste(coh, "(rows) x", plat, "(columns)"), d[[coh]], d[[plat]]) })
      if (cancer %in% names(proc_row)) {
        P("Q13", paste0("2L + PROC rows (", coh, " ", proc_row[[cancer]], "): the line starting at their index date and ", plat, " on that line's row"),
          "spec: the first line row with 'Platinum Resistant'; this checks it (a blank status on the PROC row itself is read on the line row)", {
          r <- rows_of(proc_row[[cancer]]); m <- match(id_chr(r[[pid]]), id_chr(pr[[pid]]))
          at <- rep("not a line start", nrow(r)); st_at <- rep(NA_character_, nrow(r))
          for (i in rev(seq_len(min(nl, 5, length(v[["line_rows"]]))))) {
            hit <- same(dcol(r, idx), sd_of(i)[m]); at[hit] <- paste0(i, "L")
            lr <- rows_of(v[["line_rows"]][i]); st_at[hit] <- as.character(lr[[plat]][match(id_chr(r[[pid]])[hit], id_chr(lr[[pid]]))])
          }
          tab(cancer, "Q13", paste("2L + PROC rows:", plat, "on the line row (rows) x line at the index date (columns)"), st_at, at)
        })
        P("Q13", paste(plat, "on the 2L-5L line rows x whether the patient has a 2L + PROC row"),
          "Refractory or 'prior LOT' lines of patients with no 2L + PROC row: the RWDP's PROC cohort leaves them out (the code's PROC includes them)", {
          ids9 <- id_chr(rows_of(proc_row[[cancer]])[[pid]]); stv <- c(); has9 <- c()
          for (k in v[["line_rows"]][-1]) { lr <- rows_of(k); stv <- c(stv, as.character(lr[[plat]]))
            has9 <- c(has9, ifelse(id_chr(lr[[pid]]) %in% ids9, "has a 2L + PROC row", "no 2L + PROC row")) }
          tab(cancer, "Q13", paste(plat, "on the 2L-5L line rows (rows) x 2L + PROC row (columns)"), stv, has9)
        })
      }
      P("Q13", paste0("elig_", plat, " x ", plat, " on the L2 rows"), "'Not Eligible' = not evaluable (no platinum before, no progression in the window, ...)", {
        r <- rows_of(v[["line_rows"]][2]); need_cols(r, c(paste0("elig_", plat), plat))
        tab(cancer, "Q13", paste0("elig_", plat, " (rows) x ", plat, " (columns), L2 rows"), r[[paste0("elig_", plat)]], r[[plat]])
      })
    }

    # ---- Q14: rows outside keep_rows (EC db = DOS) -----------------------------------------
    for (kc in intersect(names(v[["keep_rows"]]), names(d_all))) {
      P("Q14", paste(kc, "by cohort (all rows)"), "which cohorts the other rows (e.g. DOS) have",
        tab(cancer, "Q14", paste(coh, "(rows) x", kc, "(columns), all rows"), d_all[[coh]], d_all[[kc]]))
      k_ids <- unique(id_chr(d_all[[pid]][keep])); o_ids <- unique(id_chr(d_all[[pid]][!keep]))
      P("Q14", "Patients in the rows read only / in the other rows only / in both", "",
        { tot <- length(union(k_ids, o_ids))
          npcts(c(length(setdiff(k_ids, o_ids)), length(setdiff(o_ids, k_ids)), length(intersect(k_ids, o_ids))), tot) })
      P("Q14", "Patients in both: columns differing between their two patient rows (patients for whom each differs)",
        "none = the other rows copy the read rows; otherwise the columns that make them differ", {
        ko <- first_row(d_all[keep & d_all[[coh]] %in% v[["overall_row"]], , drop = FALSE])
        oo <- first_row(d_all[!keep & d_all[[coh]] %in% v[["overall_row"]], , drop = FALSE])
        both <- intersect(id_chr(ko[[pid]]), id_chr(oo[[pid]]))
        if (length(both) < min_cell) paste0("fewer than ", min_cell, " patients") else {
          a <- ko[match(both, id_chr(ko[[pid]])), , drop = FALSE]; b <- oo[match(both, id_chr(oo[[pid]])), , drop = FALSE]
          nd <- vapply(setdiff(names(d_all), c(pid, kc)), function(x) sum(lab(a[[x]]) != lab(b[[x]])), numeric(1))
          nd <- sort(nd[nd > 0], decreasing = TRUE); top <- seq_len(min(25, length(nd)))
          paste0(mask(length(both)), " patients; ", if (!length(nd)) "no column differs"
                 else paste0(names(nd)[top], " ", vapply(nd[top], npct, "", d = length(both)), collapse = ", "))
        }
      })
      P("Q14", "Patients only in the other rows meeting the criteria if those rows were read", "eligible patients that keep_rows leaves out", {
        vo <- v; vo[["keep_rows"]] <- setNames(list(setdiff(unique(toupper(trimws(as.character(d_all[[kc]][!keep])))), "")), kc)
        el <- attrition(criteria(cancer, standardise(d_all, vo, cancer)))$ids
        npct(length(intersect(el, setdiff(o_ids, k_ids))), length(setdiff(o_ids, k_ids)))
      })
    }

    # ---- Q15: line settings ------------------------------------------------------------------
    st <- v[["lot_setting"]] %||% "l{i}setting"
    P("Q15", "Line setting by line (patient rows, lines with a start)", "lines are numbered whatever the setting (spec): how many 2L-5L are adjuvant / neoadjuvant", {
      need_cols(pr, lv(st, 1)); li <- c(); se <- c()
      for (i in seq_len(min(5, nl, n_of(pr, st)))) { h <- !is.na(sd_of(i)); li <- c(li, rep(paste0(i, "L"), sum(h))); se <- c(se, as.character(pr[[lv(st, i)]][h])) }
      tab(cancer, "Q15", "line (rows) x setting (columns)", li, se)
    })
    P("Q15", "Columns with 'setting' in the name", "", { m <- grep("setting", names(d), value = TRUE); if (length(m)) paste(m, collapse = ", ") else "none" })

    # ---- Q17: EC adjuvant-extended lines vs the standard lines ---------------------------------
    if (!identical(v[["lot_sd"]], std_lot$sd)) {
      nL  <- n_of(pr, std_lot$sd)
      adj <- grep("^adj(t)?sd$", names(pr), value = TRUE)[1]; neo <- grep("^neoadj(t)?sd$", names(pr), value = TRUE)[1]
      P("Q17", paste0("Each ", v[["lot_sd"]], " matched to the line starting on the same date"),
        "AdjX line i = lot i: same numbering; = lot i-1: one adjuvant/neoadjuvant line numbered before; = adjuvant/neoadjuvant start or no line: an adjuvant/neoadjuvant line", {
        if (!nL) stop("no ", std_lot$sd, " columns")
        li <- c(); what <- c()
        for (i in seq_len(nl)) {
          s <- sd_of(i); w <- rep("no line starting then", nrow(pr))
          for (j in rev(seq_len(nL))) w[same(s, dcol(pr, lv(std_lot$sd, j)))] <- paste0("lot", j)
          for (j in rev(seq_len(n_of(pr, std_lot$msd)))) w[w == "no line starting then" & same(s, dcol(pr, lv(std_lot$msd, j)))] <- paste0("lot", j, " maintenance")
          if (!is.na(adj)) w[w == "no line starting then" & same(s, dcol(pr, adj))] <- paste0("adjuvant start (", adj, ")")
          if (!is.na(neo)) w[w == "no line starting then" & same(s, dcol(pr, neo))] <- paste0("neoadjuvant start (", neo, ")")
          h <- !is.na(s); li <- c(li, rep(paste0("AdjX line ", i), sum(h))); what <- c(what, w[h])
        }
        tab(cancer, "Q17", paste(v[["lot_sd"]], "(rows) x the line starting on the same date (columns)"), li, what)
      })
      P("Q17", paste0(v[["lot_ed"]], " of AdjX lines starting with a standard line: = its ", std_lot$ed, " / = its ", std_lot$ld, " only / neither"),
        "= lot ed: the AdjX end date is the last administration, like lot{i}ed (Q12)", {
        if (!nL) stop("no ", std_lot$sd, " columns")
        n_ed <- n_ld <- tot <- 0
        for (i in seq_len(nl)) for (j in seq_len(nL)) {
          e <- dcol(pr, lv(v[["lot_ed"]], i)); m <- same(sd_of(i), dcol(pr, lv(std_lot$sd, j))) & !is.na(e)
          le <- same(e, dcol(pr, lv(std_lot$ed, j)))
          ll <- if (lv(std_lot$ld, j) %in% names(pr)) same(e, dcol(pr, lv(std_lot$ld, j))) else rep(FALSE, nrow(pr))
          tot <- tot + sum(m); n_ed <- n_ed + sum(m & le); n_ld <- n_ld + sum(m & !le & ll)
        }
        npcts(c(n_ed, n_ld, tot - n_ed - n_ld), tot)
      })
      P("Q17", paste0(v[["lot_name"]], " of AdjX lines starting with a standard line: same name (ignoring case)"), "", {
        if (!nL) stop("no ", std_lot$sd, " columns")
        n <- tot <- 0
        for (i in seq_len(nl)) for (j in seq_len(nL)) {
          m <- same(sd_of(i), dcol(pr, lv(std_lot$sd, j))); need_cols(pr, c(lv(v[["lot_name"]], i), lv(std_lot$name, j)))
          n <- n + sum(m & same(toupper(pr[[lv(v[["lot_name"]], i)]]), toupper(pr[[lv(std_lot$name, j)]]))); tot <- tot + sum(m)
        }
        npct(n, tot)
      })
      for (ref in intersect(c(v[["age_date"]], "init"), names(pr)))
        P("Q17", paste0("AdjX lines starting before ", ref, ", by line"),
          if (ref == "init") "0 expected: a line before the initial diagnosis is a date anomaly or treatment of another cancer"
          else "lines numbered before advanced disease (adjuvant, neoadjuvant)", {
          r <- dcol(pr, ref)
          paste(vapply(seq_len(min(nl, 5)), function(i) paste0(i, "L ", npct(sum(sd_of(i) < r, na.rm = TRUE), sum(!is.na(sd_of(i))))), ""), collapse = "; ")
        })
      P("Q17", "Patients with an AdjX line but no standard line", "patients with only adjuvant/neoadjuvant treatment (not 'advanced' lines)", {
        if (!nL) stop("no ", std_lot$sd, " columns")
        npct(sum(!is.na(sd_of(1)) & is.na(dcol(pr, lv(std_lot$sd, 1)))), sum(!is.na(sd_of(1))))
      })
      P("Q17", "Adjuvant-extended column families", "e.g. adjx_lot{i}m... = maintenance kept apart from adjx_lot{i}",
        { f <- unique(gsub("[0-9]+", "{i}", grep("^adj_?x", names(d), value = TRUE))); if (length(f)) paste(f, collapse = ", ") else "none" })
    }
  }

  # ---- Q1a: the OC table the form names ------------------------------------------------
  if (!is.null(raw$OC) && !identical(rwdp_source[["OC"]], oc_form_table))
    probe("OC", "Q1a", paste("The form's table", oc_form_table, "vs the table read"),
          "rows / patients and the trial attrition of both; 'not run' = it cannot be read (no access or not there)", {
      v1 <- read_rwdp(oc_form_table); v <- rwdp_vars[["OC"]]
      att <- function(x) paste(mask(attrition(criteria("OC", standardise(x, v, "OC")))$table$N), collapse = ", ")
      only <- function(a, b) { x <- setdiff(names(a), names(b)); if (length(x)) paste(head(x, 30), collapse = ", ") else "none" }
      paste0("form's table: ", mask(nrow(v1)), " rows / ", mask(n_distinct(id_chr(v1[[v[["patientid"]]]]))), " patients, attrition ",
             tryCatch(att(v1), error = function(e) paste("fails:", mask_msg(conditionMessage(e)))),
             "; table read: ", mask(nrow(raw$OC)), " / ", mask(n_distinct(id_chr(raw$OC[[v[["patientid"]]]]))), ", attrition ", att(raw$OC),
             "; columns only in the form's table: ", only(v1, raw$OC), "; only in the table read: ", only(raw$OC, v1))
    })

  probe_tab <- bind_rows(probes)
  if (nrow(probe_tab)) probe_tab <- arrange(probe_tab, match(Question, q_order), Cancer)
  table_tab <- bind_rows(tables)
  if (nrow(table_tab)) table_tab <- arrange(table_tab, match(Question, q_order), Cancer)
  list(probes = probe_tab, tables = table_tab, columns = bind_rows(profiles))
}

# ---- Write ---------------------------------------------------------------------------
probe_res <- question_probes()
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
probe_file <- file.path(out_dir, paste0(out_stem, "_question_probes.xlsx"))
write.xlsx(list(Probes = if (nrow(probe_res$probes)) probe_res$probes else tibble(Note = "No probes ran"),
                Tables = if (nrow(probe_res$tables)) probe_res$tables else tibble(Note = "No tables"),
                Columns = if (nrow(probe_res$columns)) probe_res$columns else tibble(Note = "No RWDP read")),
           probe_file, headerStyle = createStyle(textDecoration = "bold"), firstRow = TRUE, colWidths = "auto")
if (nrow(probe_res$probes)) print(as.data.frame(select(probe_res$probes, Cancer, Question, Probe, Result)), right = FALSE)
cat("\nWritten:", probe_file, "\n(aggregates only: no patient IDs or dates; counts masked)\n")
