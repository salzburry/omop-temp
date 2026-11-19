# =============================================================================
# OC/EC LOT RDQ: lines of therapy 2L-5L for advanced ovarian (OC) and
# endometrial (EC) cancer. MoRez 61579 and 61664.
#
# Run on the platform with DATABRICKS_PWD set:  source("ocec_lot_rdq_run.R")
# Writes to out_dir:
#   <out_stem>.xlsx         attrition (1a, 1b), patient counts (3), review sheets
#   <out_stem>_LOT_EC.xlsx  patient-level LOT table, EC (output 2)
#   <out_stem>_LOT_OC.xlsx  patient-level LOT table, OC (output 2)
# The run stops, writing nothing, if the data or the outputs fail a check;
# earlier output files with the same names are removed when it starts.
# Open questions behind the settings: docs/open_questions.md.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(openxlsx) })

# ---- Settings ------------------------------------------------------------------
out_dir  <- "/mnt/artifacts/results"
out_stem <- paste0("TFL_OCEC_LOT_FI_RDQ_v0.1_", toupper(format(Sys.Date(), "%d%b%Y")))

src <- c(EC = "hive_metastore.rwd_flatiron_dataproducts.rwdap_endo_2026m09_v1",
         OC = "hive_metastore.am338369.rwdap_oc_pano_2026m09_v3")

study_end <- c(EC = as.Date("2026-08-31"), OC = as.Date("2026-07-31"))   # later lines not listed
l1_window <- list(EC = as.Date(c("2021-04-01", "2026-05-31")),            # 1L start, inclusive
                  OC = as.Date(c("2020-04-01", "2026-04-30")))
min_age    <- 18
oc_l1_days <- 120          # OC: 1L or surgery 0-120 days after initial diagnosis
gap_days   <- 180          # exclusion: visit gap longer than this
lines_out  <- 2:5          # lines listed and counted

ec_1l_rule         <- "chemo_io_only"   # Q2: "chemo_io_only" or "any_chemo"
plat_carry_forward <- FALSE             # Q16: TRUE keeps PROC = Y after the first PROC line

# OC platinum status (plat_sen_v1) to PSOC / PROC (Q13)
psoc_values    <- "Platinum Sensitive"
proc_values    <- c("Platinum Resistant", "Platinum Refractory",
                    "Primary platinum resistant/refractory in a prior LOT")
neither_values <- "Not Eligible"

chemo_agents <- c("carboplatin", "cisplatin", "oxaliplatin", "paclitaxel", "docetaxel",
                  "doxorubicin", "epirubicin", "ifosfamide", "cyclophosphamide", "gemcitabine",
                  "topotecan", "irinotecan", "etoposide", "vinorelbine", "capecitabine",
                  "fluorouracil", "pemetrexed", "mitomycin", "trabectedin", "eribulin",
                  "ixabepilone", "bleomycin", "dactinomycin", "vincristine", "vinblastine")
io_agents    <- c("pembrolizumab", "dostarlimab", "durvalumab", "atezolizumab", "nivolumab",
                  "avelumab", "ipilimumab", "cemiplimab", "tremelimumab", "retifanlimab",
                  "tislelizumab", "toripalimab")
platinum_agents <- c("carboplatin", "cisplatin", "oxaliplatin")

# Appendix A of the form: ADCs (generic name, aliases, brand/short name)
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
# ADC payload stems: names with one but no Appendix A match are listed for review
adc_stems <- c("vedotin", "deruxtecan", "dxd", "govitecan", "rezetecan", "tirumotecan", "sesutecan",
               "samrotecan", "emtansine", "mertansine", "ravtansine", "ozogamicin", "tesirine", "mafodotin",
               "sunirine", "tazevibulin", "rilsodotin", "ecteribulin", "duocarmazine")

# RWDP columns (data dictionary 2026m09). One row per patient per cohort (cohortn):
# the patient row, and one row per line indexed at the line's start (indexdt).
vars <- list(
  EC = list(lot = "adjx_lot",              # adjuvant-extended lines adjx_lot{i}, {i}sd, {i}ed
            line_rows = c(8, 9, 10),       # cohortn of lines 1-3
            db = "EDM",                    # EDM rows only (criterion 1; Q14)
            gap = "visit_gap_max",         # days, on the 2L row
            lotn = "adjx_lotn"),
  OC = list(lot = "lot",
            line_rows = c(2, 4, 6, 7, 8),  # cohortn of lines 1-5
            gap = "visit180_cont_fl_diag", # Yes / No / Missing, on the 2L row
            plat = "plat_sen_v1",          # on each line's row
            lotn = "lotn",
            l1_plat = "l1plat"))

# ---- Helpers ---------------------------------------------------------------------
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) { tz <- attr(x, "tzone")
    return(as.Date(format(x, "%Y-%m-%d", tz = if (length(tz) && nzchar(tz[1])) tz[1] else "UTC"))) }
  as.Date(substr(as.character(x), 1, 10))
}
num  <- function(x) suppressWarnings(as.numeric(as.character(x)))
year <- function(x) as.numeric(format(to_date(x), "%Y"))
same <- function(a, b) !is.na(a) & !is.na(b) & a == b
yn   <- function(x) { x <- toupper(trimws(as.character(x)))
                      ifelse(x %in% c("Y", "YES", "1", "TRUE"), "Y", ifelse(x %in% c("N", "NO", "0", "FALSE"), "N", NA)) }
fmt_date <- function(x) sub("^0", "", format(x, "%d %B %Y"))
fail <- function(...) stop(..., call. = FALSE)

# Drugs of a line name, normalised: "Carboplatin,Paclitaxel" -> "carboplatin", "paclitaxel"
norm     <- function(x) trimws(gsub("[^a-z0-9]+", " ", tolower(x)))
drugs_of <- function(name) lapply(strsplit(coalesce(name, ""), ","), function(x) { x <- norm(x); x[x != ""] })
agent_re <- function(agents) paste0("\\b(", paste(agents, collapse = "|"), ")\\b")
is_platinum <- function(name) vapply(drugs_of(name), function(d) any(grepl(agent_re(platinum_agents), d)), TRUE)
is_study_drug <- function(name) grepl("clinical study drug", norm(coalesce(name, "")))

# EC criterion 3: class of the 1L line
ec_1l_class <- function(name) vapply(drugs_of(name), function(d) {
  chemo <- grepl(agent_re(chemo_agents), d); io <- grepl(agent_re(io_agents), d)
  if (!any(chemo)) "No chemotherapy" else if (all(chemo | io)) "Chemotherapy alone or with IO"
  else "Chemotherapy with other agents"
}, "")
ec_other_agents <- function(name) vapply(drugs_of(name), function(d)
  paste(d[!grepl(agent_re(c(chemo_agents, io_agents)), d)], collapse = ", "), "")

# Appendix A agents in each line name ("" if none): whole words within one drug,
# also without spaces ("HS20089"). Plain trastuzumab matches nothing.
adc_terms <- bind_rows(lapply(seq_len(nrow(adc_list)), function(k) {
  t <- norm(unlist(strsplit(paste(adc_list[k, c("generic", "aliases", "brand_short")], collapse = ";"), ";")))
  t <- unique(c(t, gsub(" ", "", t))); tibble(generic = adc_list$generic[k], term = t[t != "" & t != "na"])
}))
adc_in <- function(name) {
  u <- unique(name)
  hits <- vapply(drugs_of(u), function(d) {
    d <- paste0(" ", d, " ")
    found <- vapply(adc_terms$term, function(t) any(grepl(paste0(" ", t, " "), d, fixed = TRUE)), TRUE)
    paste(unique(adc_terms$generic[found]), collapse = "; ")
  }, "")
  hits[match(name, u)]
}
adc_stem_in <- function(name) grepl(paste(adc_stems, collapse = "|"), norm(coalesce(name, "")))

# ---- Read ----------------------------------------------------------------------------
connect <- function() {
  pwd <- Sys.getenv("DATABRICKS_PWD")
  if (!nzchar(pwd)) fail("Set the DATABRICKS_PWD environment variable.")
  con <- DBI::dbConnect(odbc::odbc(), dsn = "RWDE", pwd = pwd, timeout = 120, bigint = "numeric")
  invisible(DBI::dbExecute(con, "USE CATALOG hive_metastore"))
  con
}
# A Databricks table, or an .rds file (tests)
read_rwdp <- function(x) {
  d <- if (grepl("\\.rds$", x)) readRDS(x) else {
    con <- connect(); on.exit(DBI::dbDisconnect(con)); DBI::dbGetQuery(con, paste("SELECT * FROM", x)) }
  d <- as.data.frame(d); names(d) <- tolower(names(d)); d
}

# ---- Prepare one RWDP: patients and lines --------------------------------------------
prepare <- function(d, cancer) {
  v <- vars[[cancer]]
  stop_if <- function(cond, ...) if (isTRUE(cond)) fail(cancer, ": ", ...)
  need <- function(x) { miss <- setdiff(x, names(d)); stop_if(length(miss) > 0, "column(s) not found: ", paste(miss, collapse = ", ")) }
  need(c("patid", "cohortn", "indexdt", v$gap, v$lotn, v$plat, v$l1_plat,
         if (cancer == "EC") c("db", "age", "advdxdt") else c("birthyear", "init", "surgerydate")))

  # EC: EDM rows only
  dropped <- c(rows = 0, patients = 0)
  if (!is.null(v$db)) {
    keep <- toupper(trimws(d$db)) %in% v$db
    dropped <- c(rows = sum(!keep), patients = length(setdiff(d$patid[!keep], d$patid[keep])))
    d <- d[keep, , drop = FALSE]
  }
  stop_if(nrow(d) == 0, "no rows", if (!is.null(v$db)) paste0(" with db = ", v$db), ".")
  id  <- as.character(d$patid)
  stop_if(any(is.na(id) | trimws(id) == ""), sum(is.na(id) | trimws(id) == ""), " row(s) with no patient ID.")
  coh <- as.character(d$cohortn)
  stop_if(any(is.na(coh) | trimws(coh) == ""), sum(is.na(coh) | trimws(coh) == ""), " row(s) with no cohortn.")

  # Patient rows (cohortn 1): one for every patient
  pr <- d[coh %in% "1", , drop = FALSE]; ids <- id[coh %in% "1"]
  stop_if(anyDuplicated(ids) > 0, "patients with two patient rows.")
  stop_if(!all(id %in% ids), length(setdiff(id, ids)), " patient(s) with no patient row.")

  # Line columns: name, start and end for lines 1 to n, n at least 5
  nums <- function(sfx) as.integer(sub(paste0("^", v$lot, "([0-9]+)", sfx, "$"), "\\1",
                                       grep(paste0("^", v$lot, "[0-9]+", sfx, "$"), names(d), value = TRUE)))
  n <- max(c(0L, nums(""), nums("sd"), nums("ed")))
  need(paste0(v$lot, rep(seq_len(n), 3), rep(c("", "sd", "ed"), each = n)))
  stop_if(n < max(lines_out), "line columns only up to line ", n, ".")

  # Row of line i: cohortn line_rows[i] indexed at the line's start (row_ok)
  row_of <- function(i) {
    k <- which(coh == as.character(v$line_rows[i]))
    stop_if(anyDuplicated(id[k]) > 0, "patients with two rows at cohortn ", v$line_rows[i], ".")
    k[match(ids, id[k])]
  }
  status_ok <- c(psoc_values, proc_values, neither_values)
  lines <- bind_rows(lapply(seq_len(n), function(i) {
    start <- to_date(pr[[paste0(v$lot, i, "sd")]])
    r <- if (i <= length(v$line_rows)) row_of(i) else rep(NA_integer_, length(ids))
    status <- if (is.null(v$plat)) NA_character_ else trimws(as.character(d[[v$plat]][r]))
    status[status %in% ""] <- NA
    bad <- setdiff(unique(na.omit(status)), status_ok)
    stop_if(length(bad) > 0, v$plat, " value(s) not recognised: ", paste(bad, collapse = ", "))
    name <- as.character(pr[[paste0(v$lot, i)]]); name[toupper(trimws(name)) %in% c("NONE", "")] <- NA
    tibble(patientid = ids, line = i, name = name, start = start, end = to_date(pr[[paste0(v$lot, i, "ed")]]),
           setting = if (is.null(pr[[paste0("l", i, "setting")]])) NA_character_ else as.character(pr[[paste0("l", i, "setting")]]),
           plat = status, psoc = ifelse(is.na(status), NA, ifelse(status %in% psoc_values, "Y", "N")),
           proc = ifelse(is.na(status), NA, ifelse(status %in% proc_values, "Y", "N")),
           row_ok = if (i <= length(v$line_rows)) !is.na(r) & same(to_date(d$indexdt[r]), start) else NA)
  })) %>% filter(!is.na(start) | !is.na(name))
  nostart <- with(lines, line <= max(lines_out) & is.na(start))
  stop_if(any(nostart), sum(nostart), " line(s) 1-", max(lines_out), " of ", n_distinct(lines$patientid[nostart]),
          " patient(s) have a name but no start date.")

  # Visit gap on the 2L row: TRUE = gap > 180 days, NA = no value
  g <- d[[v$gap]][row_of(2)]
  gap <- if (cancer == "EC") {
    x <- num(g); bad <- unique(g[!is.na(g) & trimws(g) != "" & !(is.finite(x) & x >= 0)])
    stop_if(length(bad) > 0, v$gap, " value(s) that are not a number of days >= 0: ", paste(head(bad, 5), collapse = ", "))
    num(g) > gap_days
  } else {
    g <- toupper(trimws(g)); g[g %in% ""] <- NA; bad <- setdiff(unique(na.omit(g)), c("YES", "NO", "MISSING"))
    stop_if(length(bad) > 0, v$gap, " value(s) not recognised: ", paste(bad, collapse = ", "))
    ifelse(g %in% "NO", TRUE, ifelse(g %in% "YES", FALSE, NA))
  }
  l2 <- filter(lines, line == 2)
  no_2l_row <- ids %in% l2$patientid[!is.na(l2$start) & !(l2$row_ok %in% TRUE)]

  # Age at advanced (EC) or initial (OC) diagnosis; EC age is at the row's index date
  age <- if (cancer == "EC") num(pr$age) + year(pr$advdxdt) - year(pr$indexdt) else year(pr$init) - num(pr$birthyear)
  init <- if (cancer == "OC") to_date(pr$init) else as.Date(NA)
  pts <- tibble(patientid = ids, age = age, init = init,
                surg_days = if (cancer == "OC") as.numeric(to_date(pr$surgerydate) - init) else NA_real_,
                gap = gap, no_2l_row = no_2l_row, lotn = num(pr[[v$lotn]]),
                l1_plat = if (is.null(v$l1_plat)) NA_character_ else yn(pr[[v$l1_plat]]))
  list(pts = pts, lines = lines, n_lines = n, n_rows = nrow(d), dropped = dropped)
}

# ---- Eligibility criteria, in the form's order ------------------------------------
criteria <- function(cancer, s, rule = ec_1l_rule, window = l1_window[[cancer]]) {
  p  <- left_join(s$pts, select(filter(s$lines, line == 1), patientid, l1_start = start, l1_name = name), by = "patientid")
  l2 <- filter(s$lines, line == 2, !is.na(start), start <= study_end[[cancer]])
  in_win <- coalesce(p$l1_start >= window[1] & p$l1_start <= window[2], FALSE)
  win_lab <- paste0("between ", fmt_date(window[1]), " and ", fmt_date(window[2]), " (inclusive)")
  gap_lab <- paste0("Excluded: patients with any gap in visits longer than ", gap_days,
                    " days between initial diagnosis and end of 2L, per Flatiron LOT rules")
  age_ok <- coalesce(p$age >= min_age, FALSE)
  has_2l <- p$patientid %in% l2$patientid
  no_gap <- !coalesce(p$gap, FALSE)                      # no value: kept (Q4)
  if (cancer == "EC") {
    cls <- ec_1l_class(p$l1_name)
    chemo <- if (rule == "any_chemo") cls != "No chemotherapy" else cls == "Chemotherapy alone or with IO"
    crit <- setNames(list(rep(TRUE, nrow(p)), age_ok, chemo, in_win, has_2l, no_gap), c(
      "Patients with endometrial cancer in the Flatiron endometrial (EDM) dataset (2026 September data cut)",
      paste0("Aged >= ", min_age, " years at the date of advanced EC diagnosis"),
      "Received chemotherapy, alone or in combination with immunotherapy (IO), as 1L treatment",
      paste("1L treatment (adjuvant-extended 1L) initiation date", win_lab),
      "Received 2L treatment (adjuvant-extended 2L)", gap_lab))
  } else {
    within <- function(x) coalesce(x >= 0 & x <= oc_l1_days, FALSE)
    crit <- setNames(list(rep(TRUE, nrow(p)), age_ok,
                          within(as.numeric(p$l1_start - p$init)) | within(p$surg_days),
                          is_platinum(p$l1_name), in_win, has_2l, no_gap), c(
      "Patients with ovarian cancer in the Flatiron ovarian cancer Panoramic dataset (2026 September data cut)",
      paste0("Aged >= ", min_age, " years at the date of initial OC diagnosis"),
      paste("Initiated 1L treatment or underwent surgery within", oc_l1_days, "days after initial diagnosis"),
      "Received platinum-containing therapy as 1L treatment",
      paste("1L treatment initiation date", win_lab),
      "Received 2L treatment", gap_lab))
  }
  # The visit gap is read on the 2L row: it must be there
  bad <- Reduce(`&`, crit[-length(crit)]) & p$no_2l_row
  if (any(bad)) fail(cancer, ": ", sum(bad), " patient(s) reaching the visit-gap step have no 2L row indexed at the 2L start.")
  bind_cols(tibble(patientid = p$patientid), as_tibble(crit, .name_repair = "minimal"))
}

attrition <- function(crit) {
  keep <- rep(TRUE, nrow(crit)); n <- integer()
  for (k in names(crit)[-1]) { keep <- keep & crit[[k]]; n <- c(n, sum(keep)) }
  list(table = tibble(`Eligibility criterion` = names(crit)[-1], N = n, `% remaining` = round(100 * n / n[1], 1),
                      `N excluded at this step` = c(NA, -diff(n))),
       ids = crit$patientid[keep])
}

# ---- LOT table: lines 2L-5L of eligible patients ------------------------------------
# PROC carried forward (Q16): after a PROC line, later lines are PROC = Y, PSOC = N
carry_proc <- function(lines) lines %>% arrange(patientid, line) %>% group_by(patientid) %>%
  mutate(after = cumany(lag(proc %in% "Y", default = FALSE)),
         proc = if_else(after, "Y", proc), psoc = if_else(after, "N", psoc)) %>%
  ungroup() %>% select(-after)

lot_table <- function(cancer, s, ids, carry = plat_carry_forward) {
  listed <- function(l) filter(l, patientid %in% ids, line %in% lines_out, !is.na(start), start <= study_end[[cancer]])
  lot <- listed(s$lines)
  if (cancer == "OC") {                                   # status from the line's own row, Y/N
    bad <- !(lot$row_ok %in% TRUE) | is.na(lot$psoc)
    if (any(bad)) fail("OC: ", sum(bad), " listed line(s) of ", n_distinct(lot$patientid[bad]),
                       " patient(s) have no platinum status on a row indexed at the line's start.")
    if (carry) lot <- listed(carry_proc(s$lines))
  }
  lot %>% mutate(adc_agents = adc_in(name), adc = if_else(adc_agents != "", "Y", "N"),
                 psoc = if (cancer == "EC") "NA" else psoc, proc = if (cancer == "EC") "NA" else proc) %>%
    select(-row_ok) %>% arrange(patientid, line)
}

# The form's output 2
lot_deliverable <- function(lot) transmute(lot,
  `Patient ID` = patientid, `LOT start date` = start, `LOT end date` = end, `LOT number` = as.integer(line),
  `LOT treatment` = name, `PSOC flag (OC only)` = psoc, `PROC flag (OC only)` = proc, `ADC LOT (Y/N)` = adc)

# ---- Patient counts (output 3) -------------------------------------------------------
# A line column counts patients with a line of that number in the stratum;
# Total (2L+) and 3L+ count patients ever in it at lines 2-5 or 3-5.
count_cols <- list(`Total (2L+)` = 2:5, `2L` = 2, `3L` = 3, `4L` = 4, `5L` = 5, `3L+` = 3:5)
strata <- list(
  EC = list("All treated" = function(l) TRUE, "ADC treated" = function(l) l$adc == "Y"),
  OC = list("All treated" = function(l) TRUE,
            "   PROC"     = function(l) l$proc %in% "Y",
            "   PSOC"     = function(l) l$psoc %in% "Y",
            "ADC treated" = function(l) l$adc == "Y",
            "   PROC "    = function(l) l$adc == "Y" & l$proc %in% "Y",
            "   PSOC "    = function(l) l$adc == "Y" & l$psoc %in% "Y"))
count_table <- function(cancer, lot, n_all) {
  bind_rows(lapply(names(strata[[cancer]]), function(st) {
    hit <- rep_len(strata[[cancer]][[st]](lot), nrow(lot)) %in% TRUE
    row <- tibble(Cancer = cancer, `# Unique patients` = sub(" $", "", st))
    for (k in names(count_cols)) {
      n <- n_distinct(lot$patientid[hit & lot$line %in% count_cols[[k]]])
      row[[paste(k, "N")]] <- n; row[[paste(k, "%")]] <- round(100 * n / n_all, 1)
    }
    row
  }))
}

# ---- Review sheets -------------------------------------------------------------------
# 1L line names of patients reaching the 1L-regimen criterion, with their class
regimens <- function(cancer, s, crit) {
  reach <- Reduce(`&`, crit[2:(if (cancer == "EC") 3 else 4)])
  tibble(patientid = crit$patientid[reach]) %>%
    left_join(select(filter(s$lines, line == 1), patientid, name), by = "patientid") %>%
    mutate(Classification = if (cancer == "EC") ec_1l_class(name)
                            else if_else(is_platinum(name), "Platinum-containing", "No platinum"),
           `Agents neither chemotherapy nor IO` = if (cancer == "EC") ec_other_agents(name) else "") %>%
    count(Cancer = cancer, `1L line name` = coalesce(name, "(no 1L line)"), Classification,
          `Agents neither chemotherapy nor IO`, name = "Patients") %>%
    arrange(desc(Patients))
}

# Listed line names that are ADCs, look like ADCs, or hold a clinical study drug
adc_review <- function(cancer, lot) {
  lot %>%
    mutate(Note = case_when(adc == "Y"          ~ "Appendix A ADC",
                            adc_stem_in(name)   ~ "ADC-like name not in Appendix A: please review",
                            is_study_drug(name) ~ "Clinical study drug: ADC status unknown")) %>%
    filter(!is.na(Note)) %>%
    group_by(Cancer = cancer, `LOT treatment` = name, `Appendix A agent(s)` = adc_agents, `ADC LOT` = adc, Note) %>%
    summarise(Lines = n(), Patients = n_distinct(patientid), .groups = "drop") %>%
    arrange(Note, desc(Lines))
}

# ---- Checks --------------------------------------------------------------------------
# The outputs agree: eligible patients = LOT listing (one row per patient and line,
# one 2L each), and table 3 = counts from the listing. Stops if not.
reconcile <- function(cancer, ids, lot, counts) {
  del <- lot_deliverable(lot); key <- paste(del$`Patient ID`, del$`LOT number`)
  if (anyDuplicated(key)) fail(cancer, ": the LOT listing has ", sum(duplicated(key)), " duplicate patient-line row(s).")
  if (!setequal(del$`Patient ID`, ids) || sum(del$`LOT number` == 2) != length(ids))
    fail(cancer, ": the LOT listing does not hold one 2L row for each of the ", length(ids), " eligible patients.")
  again <- count_table(cancer, transmute(del, patientid = `Patient ID`, line = `LOT number`, adc = `ADC LOT (Y/N)`,
                                         psoc = `PSOC flag (OC only)`, proc = `PROC flag (OC only)`), length(ids))
  if (!isTRUE(all.equal(again, counts))) fail(cancer, ": the patient counts do not match the LOT listing.")
}

# The form's footnote to table 3: 2L+ = 2L and 3L+ = 3L except for PROC and ADC
# treated. Patients in 2L+ but not 2L / in 3L+ but not 3L.
footnote_check <- function(cancer, lot) {
  fn <- c(list(`All treated` = rep(TRUE, nrow(lot))), if (cancer == "OC") list(PSOC = lot$psoc %in% "Y"))
  paste(vapply(names(fn), function(k) {
    pts <- function(l) unique(lot$patientid[fn[[k]] & lot$line %in% l])
    paste0(k, ": ", length(setdiff(pts(2:5), pts(2))), " / ", length(setdiff(pts(3:5), pts(3))))
  }, ""), collapse = "; ")
}

data_checks <- function(cancer, s, ids, lot) {
  el <- filter(s$lines, patientid %in% ids); lotn <- s$pts$lotn[s$pts$patientid %in% ids]
  gaps <- vapply(split(el$line, el$patientid), function(x) !all(seq_len(max(x)) %in% x), TRUE)
  chk <- list(
    "Rows read / patients" = paste(s$n_rows, "/", nrow(s$pts)),
    "Rows not read (EC: not EDM) / patients only in those rows" = paste(s$dropped, collapse = " / "),
    "Eligible patients" = length(ids),
    "Line columns in the RWDP (lines 1 to n)" = s$n_lines,
    "Eligible patients with more than 5 lines (lotn; lines after 5L are not listed)" = sum(lotn > max(lines_out), na.rm = TRUE),
    "Eligible patients with more lines (lotn) than line columns" = sum(lotn > s$n_lines, na.rm = TRUE),
    "Eligible patients with a gap in line numbers (e.g. 4L but no 3L)" = sum(gaps),
    "Eligible patients with no visit-gap value (kept)" = sum(is.na(s$pts$gap[s$pts$patientid %in% ids])),
    "Listed lines with no end date" = sum(is.na(lot$end)),
    "Listed lines ending before they start" = sum(lot$end < lot$start, na.rm = TRUE),
    "Listed lines with no name" = sum(is.na(lot$name)),
    "Listed lines with a clinical study drug (ADC status unknown)" = sum(is_study_drug(lot$name)),
    "Form footnote: patients in 2L+ but not 2L / in 3L+ but not 3L (0 / 0 expected)" = footnote_check(cancer, lot))
  if (cancer == "OC") {
    tab <- function(x) { t <- sort(table(coalesce(x, "(no value)")), decreasing = TRUE); paste0(names(t), ": ", t, collapse = "; ") }
    l1 <- filter(s$lines, line == 1); nm <- is_platinum(l1$name); f <- s$pts$l1_plat[match(l1$patientid, s$pts$patientid)]
    chk <- c(chk, list(
      "Listed lines with PSOC and PROC both Y (0 expected)" = sum(lot$psoc %in% "Y" & lot$proc %in% "Y"),
      "Platinum status of the listed lines" = tab(lot$plat),
      "Line setting of the listed lines" = tab(lot$setting),
      "1L platinum by name and l1plat: both / name only / l1plat only / neither / l1plat missing" =
        paste(sum(nm & f %in% "Y"), sum(nm & f %in% "N"), sum(!nm & f %in% "Y"), sum(!nm & f %in% "N"), sum(is.na(f)), sep = " / ")))
  }
  tibble(Cancer = cancer, Check = names(chk), Value = vapply(chk, as.character, ""))
}

# Counts for the open questions that change the numbers (docs/open_questions.md)
question_counts <- function(cancer, s, crit, ids, lot) {
  safe <- function(x) tryCatch(x, error = function(e) paste("not computable:", conditionMessage(e)))
  q <- list()
  if (cancer == "EC") {
    other <- setdiff(c("chemo_io_only", "any_chemo"), ec_1l_rule)
    q[[paste0("Q2: eligible patients if ec_1l_rule were ", other, " (now ", length(ids), ")")]] <-
      safe(length(attrition(criteria("EC", s, rule = other))$ids))
    q[[paste0("Q3: eligible patients if the 1L window started ", fmt_date(l1_window$OC[1]), " (now ", length(ids), ")")]] <-
      safe(length(attrition(criteria("EC", s, window = c(l1_window$OC[1], l1_window$EC[2])))$ids))
  } else {
    fail3 <- crit$patientid[crit[[3]] & !crit[[4]]]        # aged >= 18, failing criterion 3
    l1 <- filter(s$lines, line == 1); p <- s$pts
    before <- p$surg_days < 0 | as.numeric(l1$start[match(p$patientid, l1$patientid)] - p$init) < 0
    q[["Q5: patients failing criterion 3 with a surgery or 1L dated before initial diagnosis"]] <-
      sum(p$patientid %in% fail3 & before, na.rm = TRUE)
    for (fl in c("proc", "psoc"))
      q[[paste0("Q6: ADC treated ", toupper(fl), ", 2L+: ADC and ", toupper(fl), " on the same line (counted) / on any lines")]] <-
        paste(n_distinct(lot$patientid[lot$adc == "Y" & lot[[fl]] %in% "Y"]), "/",
              length(intersect(lot$patientid[lot$adc == "Y"], lot$patientid[lot[[fl]] %in% "Y"])))
    other_proc <- lot$proc %in% "Y" & !lot$plat %in% "Platinum Resistant"
    q[["Q13: listed PROC lines (patients) that are not 'Platinum Resistant' (refractory or prior LOT)"]] <-
      paste0(sum(other_proc), " (", n_distinct(lot$patientid[other_proc]), ")")
    q[[paste0("Q16: listed lines (patients) whose PROC would change with plat_carry_forward = ", !plat_carry_forward)]] <-
      safe({ alt <- lot_table("OC", s, ids, carry = !plat_carry_forward); ch <- alt$proc != lot$proc
             paste0(sum(ch), " (", n_distinct(lot$patientid[ch]), ")") })
  }
  q[["Q4: eligible patients with no visit-gap value (kept; excluded if no value counted as a gap)"]] <-
    sum(is.na(s$pts$gap[s$pts$patientid %in% ids]))
  tibble(Cancer = cancer, Question = names(q), Count = vapply(q, as.character, ""))
}

# ---- Derive ----------------------------------------------------------------------------
# Remove earlier output files with the same names first, so a stopped run leaves none
final <- file.path(out_dir, paste0(out_stem, c(summary = ".xlsx", EC = "_LOT_EC.xlsx", OC = "_LOT_OC.xlsx")))
names(final) <- c("summary", "EC", "OC")
unlink(final[file.exists(final)])

res <- list()
for (cancer in c("EC", "OC")) {
  s      <- prepare(read_rwdp(src[[cancer]]), cancer)
  crit   <- criteria(cancer, s)
  att    <- attrition(crit)
  lot    <- lot_table(cancer, s, att$ids)
  counts <- count_table(cancer, lot, length(att$ids))
  reconcile(cancer, att$ids, lot, counts)
  res[[cancer]] <- list(s = s, ids = att$ids, attrition = att$table, lot = lot, counts = counts,
                        regimens = regimens(cancer, s, crit), adc = adc_review(cancer, lot),
                        checks = data_checks(cancer, s, att$ids, lot),
                        questions = question_counts(cancer, s, crit, att$ids, lot))
  cat(cancer, ": ", nrow(s$pts), " patients, ", length(att$ids), " eligible, ", nrow(lot), " lines listed\n", sep = "")
}

# ---- Write ----------------------------------------------------------------------------
options(openxlsx.dateFormat = "yyyy-mm-dd")
header_style <- createStyle(textDecoration = "bold", fgFill = "#D9D9D9", wrapText = TRUE, valign = "top")
n_el <- vapply(res, function(r) length(r$ids), 1L)
attrition_notes <- c("% remaining: the row's N as a % of the first row's N.",
                     "N excluded at this step: the previous row's N minus this row's N.",
                     "Criteria apply in this order; each row counts patients meeting it and every criterion above it.")
count_notes <- c(
  paste0("Percentages are calculated using as the denominator the total number of patients meeting all ",
         "eligibility criteria (inclusion and exclusion), separately for EC (N = ", n_el[["EC"]], ") and OC (N = ", n_el[["OC"]], ")."),
  "2L, 3L, 4L, 5L: patients whose line of that number is in the stratum (PROC/PSOC: the line's RWDP flag is Y; ADC treated: the line contains an Appendix A ADC; ADC treated PROC/PSOC: the same line is both).",
  "Total (2L+) and 3L+: patients who are ever in the stratum at lines 2-5 or 3-5.",
  "Strata are not mutually exclusive.")

# Sheets of each file, with notes and column widths
sheets <- list(
  summary = list("1a Attrition EC"  = res$EC$attrition, "1b Attrition OC" = res$OC$attrition,
                 "3 Patient counts" = bind_rows(res$EC$counts, res$OC$counts),
                 "1L regimens"      = bind_rows(res$EC$regimens, res$OC$regimens),
                 "ADC line names"   = bind_rows(res$EC$adc, res$OC$adc),
                 "Data checks"      = bind_rows(res$EC$checks, res$OC$checks),
                 "Open questions"   = bind_rows(res$EC$questions, res$OC$questions)),
  EC = list("LOT EC" = lot_deliverable(res$EC$lot)),
  OC = list("LOT OC" = lot_deliverable(res$OC$lot)))
notes  <- list("1a Attrition EC" = attrition_notes, "1b Attrition OC" = attrition_notes, "3 Patient counts" = count_notes)
widths <- list("1a Attrition EC" = c(95, 10, 12, 14), "1b Attrition OC" = c(95, 10, 12, 14),
               "3 Patient counts" = c(8, 18, rep(9, 12)), "Data checks" = c(8, 90, 30), "Open questions" = c(8, 110, 12),
               "LOT EC" = c(18, 14, 14, 10, 50, 10, 10, 10), "LOT OC" = c(18, 14, 14, 10, 50, 10, 10, 10))

# Every cell of a saved sheet equals what was written (rows after the data are notes)
same_cells <- function(path, sheet, x) {
  y <- read.xlsx(path, sheet = sheet, sep.names = " ", na.strings = character(), detectDates = TRUE, skipEmptyRows = FALSE)
  if (!identical(names(y), names(x)) || nrow(y) < nrow(x)) return(FALSE)
  all(vapply(names(x), function(k) {
    a <- x[[k]]; b <- y[[k]][seq_len(nrow(x))]
    if (inherits(a, "Date")) b <- suppressWarnings(as.Date(b))
    else if (is.numeric(a)) b <- suppressWarnings(as.numeric(b))
    else { a <- trimws(as.character(a)); b <- trimws(as.character(b)); a[a %in% ""] <- NA; b[b %in% ""] <- NA }
    identical(is.na(a), is.na(b)) && (if (is.numeric(a)) isTRUE(all.equal(as.numeric(a[!is.na(a)]), b[!is.na(b)]))
                                      else all(a[!is.na(a)] == b[!is.na(b)]))
  }, TRUE))
}

# Read the saved files back: every cell as written, and they agree with each other
verify_outputs <- function(files, sheets, res) {
  for (f in names(sheets)) for (sh in names(sheets[[f]]))
    if (!same_cells(files[[f]], sh, sheets[[f]][[sh]])) fail("The saved file does not hold what was written: ", f, ", sheet ", sh, ".")
  rd <- function(...) read.xlsx(..., sep.names = " ", na.strings = character())
  counts <- rd(files[["summary"]], sheet = "3 Patient counts")
  for (cancer in intersect(c("EC", "OC"), names(files))) {
    ids <- as.character(res[[cancer]]$ids)
    att <- num(rd(files[["summary"]], sheet = paste(c(EC = "1a", OC = "1b")[[cancer]], "Attrition", cancer))$N)
    lot <- rd(files[[cancer]]); pid <- as.character(lot$`Patient ID`); ln <- as.integer(lot$`LOT number`)
    again <- count_table(cancer, tibble(patientid = pid, line = ln, adc = lot$`ADC LOT (Y/N)`,
                                        psoc = lot$`PSOC flag (OC only)`, proc = lot$`PROC flag (OC only)`), length(ids))
    saved <- counts[counts$Cancer %in% cancer, names(again)]
    ok <- c("one row per patient and line" = !anyDuplicated(paste(pid, ln)),
            "rows as derived" = nrow(lot) == nrow(res[[cancer]]$lot),
            "patients = eligible patients" = setequal(pid, ids),
            "last attrition row = eligible patients" = isTRUE(tail(att[!is.na(att)], 1) == length(ids)),
            "one 2L row per eligible patient" = sum(ln == 2, na.rm = TRUE) == length(ids),
            "table 3 = counts from the LOT file" = nrow(saved) == nrow(again) &&
              isTRUE(all.equal(as.data.frame(saved), as.data.frame(again), check.attributes = FALSE)))
    if (!all(ok)) fail(cancer, ": the saved files do not agree: ", paste(names(ok)[!ok], collapse = "; "), ".")
  }
  invisible(TRUE)
}

# Save to temporary files, check them, then copy to the final names
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tmp <- vapply(names(sheets), function(f) {
  wb <- createWorkbook()
  for (sh in names(sheets[[f]])) {
    x <- sheets[[f]][[sh]]
    addWorksheet(wb, sh); writeData(wb, sh, x, headerStyle = header_style)
    if (length(notes[[sh]])) writeData(wb, sh, notes[[sh]], startRow = nrow(x) + 3)
    setColWidths(wb, sh, cols = seq_along(x), widths = if (is.null(widths[[sh]])) "auto" else widths[[sh]])
    freezePane(wb, sh, firstRow = TRUE)
  }
  path <- tempfile(fileext = ".xlsx"); saveWorkbook(wb, path, overwrite = TRUE); path
}, "")
checked <- tryCatch(verify_outputs(as.list(tmp), sheets, res), error = function(e) e)
if (inherits(checked, "error")) {
  unlink(tmp)
  fail(conditionMessage(checked), " No output written.")
}
# All three files or none
ok <- suppressWarnings(file.copy(tmp, final[names(sheets)], overwrite = TRUE)) & file.size(final[names(sheets)]) %in% file.size(tmp)
unlink(tmp)
if (!all(ok)) { unlink(final); fail("The output files could not be written to ", out_dir, ". No output written.") }

cat("\nSaved files read back and checked.\n")
for (cancer in c("EC", "OC")) { cat("\n==", cancer, "attrition\n"); print(as.data.frame(res[[cancer]]$attrition)) }
cat("\n== Patient counts\n"); print(as.data.frame(bind_rows(res$EC$counts, res$OC$counts)))
cat("\nWritten:\n ", paste(final, collapse = "\n  "), "\n")
