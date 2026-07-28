#!/usr/bin/env Rscript
# =============================================================================
# build_kpi08_dashboard.R
#
# Embeds the ASMA (KPI08) numbers into kpi08.html so the dashboard opens by
# simply double-clicking the file (offline, no server, no Python).
#
# Two ways to use it:
#
#   1) From a terminal
#        Rscript KPI08/build_kpi08_dashboard.R
#
#   2) From R / a Quarto chunk — source it and call the function
#        source(here::here("KPI08", "build_kpi08_dashboard.R"))
#        build_kpi08_dashboard()
#
#      Sourcing only defines the function; nothing is written until you call it.
#
# It reads data/PBWG-BRA-asma-analytic-<from>-<to>-ref<refYear>-<variant>.csv
# (written by the `prepare-bra-asma-data` chunk of ASMA-BRA-ingestion.qmd),
# aggregates the daily rows to per-airport / per-year / monthly averages, and
# writes the result into the <script id="dash-data"> block of kpi08.html.
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install it once with install.packages('jsonlite').")
}

# =============================================================================
# build_kpi08_dashboard(...)
#
#   data_dir  : where the ASMA analytic CSV lives          (default data/)
#   html_file : the dashboard page to embed the data into  (default kpi08.html)
#   ref_year / variant : must match _chapter-setup.R
#   rings     : ASMA rings to expose, in display order
#
# Returns, invisibly, the path of the page written.
# =============================================================================
build_kpi08_dashboard <- function(data_dir  = here::here("data"),
                                  html_file = here::here("kpi08.html"),
                                  ref_year  = 2024,
                                  variant   = "icao_ganp_p20",
                                  rings     = c("C40", "C100")) {

  pat <- sprintf("^PBWG-BRA-asma-analytic-[0-9]{4}-[0-9]{4}-ref%d-%s\\.csv$",
                 ref_year, variant)
  files <- list.files(data_dir, pattern = pat, full.names = TRUE)
  if (length(files) == 0) {
    stop("No ASMA analytic CSV found in ", data_dir, "/ matching ", pat,
         "\n  -> run the `prepare-bra-asma-data` chunk in ASMA-BRA-ingestion.qmd first.")
  }
  # if several spans exist, use the most recently modified one
  if (length(files) > 1) {
    files <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
    message("Several analytic files found; using the newest: ", basename(files))
  }

  raw <- readr::read_csv(
    files, show_col_types = FALSE,
    col_types = readr::cols(DATE = readr::col_character(), .default = readr::col_guess())
  ) |>
    mutate(YEAR = substr(DATE, 1, 4), MONTH = substr(DATE, 6, 7))

  # the supplied-KPI08 column is optional; treat it as missing rather than failing
  if (!"TOT_BRA_KPI08" %in% names(raw)) raw$TOT_BRA_KPI08 <- NA_real_

  agg <- function(df, keys) {
    df |>
      group_by(across(all_of(keys))) |>
      summarise(mvts = sum(MVTS_VALID), na = sum(MVTS_NA),
                asma = sum(TOT_ASMA), ref = sum(TOT_REF), add = sum(TOT_ADD_TIME),
                sup = sum(TOT_BRA_KPI08, na.rm = TRUE),
                .groups = "drop") |>
      mutate(avg_asma    = round(asma / mvts, 3),
             avg_ref     = round(ref  / mvts, 3),
             avg_add     = round(add  / mvts, 3),
             avg_add_sup = round(sup  / mvts, 3),
             na_share    = round(100 * na / (mvts + na), 2))
  }
  leaf <- function(r) list(mvts = r$mvts, avg_asma = r$avg_asma, avg_ref = r$avg_ref,
                           avg_add = r$avg_add, avg_add_sup = r$avg_add_sup,
                           na_share = r$na_share)

  # turn a data frame into nested named lists keyed by `keys`, `fun` at the leaf
  nest <- function(df, keys, fun) {
    if (length(keys) == 0) return(fun(df))
    lapply(split(df, df[[keys[1]]]), function(s) nest(s, keys[-1], fun))
  }

  ap_df <- agg(raw, c("ICAO", "RANGE", "YEAR"))
  ov_df <- agg(raw, c("RANGE", "YEAR"))
  # Carry the movement count with each month, not just the average: a month can
  # hold a single boundary flight (a source extract ending at midnight leaves one
  # arrival in the next month), and one movement is not a monthly average.
  mo_df <- raw |>
    group_by(RANGE, YEAR, MONTH) |>
    summarise(avg_add = round(sum(TOT_ADD_TIME) / sum(MVTS_VALID), 3),
              mvts = sum(MVTS_VALID), .groups = "drop")

  airports <- nest(ap_df, c("ICAO", "RANGE", "YEAR"), leaf)
  overall  <- nest(ov_df, c("RANGE", "YEAR"),        leaf)
  monthly  <- nest(mo_df, c("RANGE", "YEAR", "MONTH"),
                   function(r) list(add = r$avg_add, mvts = r$mvts))

  # a year whose last day is before 31 Dec is flagged as partial in the page
  partial <- list()
  pd <- raw |> group_by(YEAR) |> summarise(maxd = max(DATE), .groups = "drop") |>
    filter(maxd < paste0(YEAR, "-12-31"))
  for (i in seq_len(nrow(pd))) partial[[pd$YEAR[i]]] <- pd$maxd[i]

  payload <- list(airports = airports, overall = overall, monthly = monthly,
                  meta = list(partial = partial),
                  # I() keeps these as JSON arrays: auto_unbox would turn a
                  # single ring or a single year into a bare string, and the page
                  # iterates over them
                  rings = I(intersect(rings, unique(raw$RANGE))),
                  years = I(sort(unique(raw$YEAR))))
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, digits = 6, na = "null"))

  # Write the numbers to their OWN file next to the page, rather than rewriting the
  # page itself. Regenerating a tracked HTML file turned every rebuild into a merge
  # conflict on one enormous line; keeping the generated data separate leaves
  # kpi08.html stable. A plain <script src> still loads from file://, so the page
  # keeps working offline by double-click.
  data_file <- file.path(dirname(html_file), "kpi08-data.js")
  readr::write_file(
    paste0("// Generated by KPI08/build_kpi08_dashboard.R — do not edit by hand.\n",
           "// Rebuild with: Rscript KPI08/build_kpi08_dashboard.R\n",
           "window.DASH_DATA = ", json, ";\n"),
    data_file
  )

  if (!grepl("kpi08-data.js", readr::read_file(html_file), fixed = TRUE))
    warning(html_file, " does not load kpi08-data.js; the page will not see the data.")

  message(sprintf("Wrote %s from %s (rings: %s | years: %s). Open %s to view.",
                  basename(data_file), basename(files),
                  paste(payload$rings, collapse = ", "),
                  paste(payload$years, collapse = ", "),
                  basename(html_file)))
  invisible(data_file)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) build_kpi08_dashboard()
