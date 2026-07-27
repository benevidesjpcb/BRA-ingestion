#!/usr/bin/env Rscript
# =============================================================================
# build_kpi08_dashboard.R
#
# Embeds the ASMA (KPI08) numbers into kpi08.html so the dashboard opens by
# simply double-clicking the file (offline, no server, no Python).
#
# Run it whenever the ASMA analytic output changes:
#   - from a terminal:  Rscript build_kpi08_dashboard.R
#   - from RStudio:     source("build_kpi08_dashboard.R")
# Then double-click kpi08.html.
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

data_dir  <- "data"
html_file <- "kpi08.html"
ref_year  <- 2024
variant   <- "icao_ganp_p20"
asma_ring <- "C40"      # label for the ASMA ring; matches asma_ring in _chapter-setup.R

pat <- sprintf("^PBWG-BRA-asma-analytic-[0-9]{4}-[0-9]{4}-ref%d-%s\\.csv$", ref_year, variant)
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
if (!"TOT_ADD_SUPPLIED" %in% names(raw)) raw$TOT_ADD_SUPPLIED <- NA_real_

agg <- function(df, keys) {
  df |>
    group_by(across(all_of(keys))) |>
    summarise(mvts = sum(MVTS_VALID), na = sum(MVTS_NA),
              asma = sum(TOT_ASMA), ref = sum(TOT_REF), add = sum(TOT_ADD_TIME),
              sup = sum(TOT_ADD_SUPPLIED, na.rm = TRUE),
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

# turn a data frame into nested named lists keyed by `keys`, applying `fun` at the leaf
nest <- function(df, keys, fun) {
  if (length(keys) == 0) return(fun(df))
  lapply(split(df, df[[keys[1]]]), function(s) nest(s, keys[-1], fun))
}

ap_df <- agg(raw, c("ICAO", "YEAR"))
ov_df <- agg(raw, c("YEAR"))
mo_df <- raw |>
  group_by(YEAR, MONTH) |>
  summarise(avg_add = round(sum(TOT_ADD_TIME) / sum(MVTS_VALID), 3), .groups = "drop")

airports <- nest(ap_df, c("ICAO", "YEAR"), leaf)
overall  <- nest(ov_df, c("YEAR"),         leaf)
monthly  <- nest(mo_df, c("YEAR", "MONTH"), function(r) r$avg_add)

# a year whose last day is before 31 Dec is flagged as partial in the page
partial <- list()
pd <- raw |> group_by(YEAR) |> summarise(maxd = max(DATE), .groups = "drop") |>
  filter(maxd < paste0(YEAR, "-12-31"))
for (i in seq_len(nrow(pd))) partial[[pd$YEAR[i]]] <- pd$maxd[i]

payload <- list(airports = airports, overall = overall, monthly = monthly,
                meta = list(partial = partial, ring = asma_ring),
                years = sort(unique(raw$YEAR)))
json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, digits = 6, na = "null"))

# replace the content between <script id="dash-data" ...> and the next </script>
html <- readr::read_file(html_file)
open_tag  <- '<script id="dash-data" type="application/json">'
close_tag <- '</script>'
i1 <- regexpr(open_tag, html, fixed = TRUE)
if (i1 < 0) stop('Marker <script id="dash-data"> not found in ', html_file)
start <- i1 + attr(i1, "match.length")
rest  <- substring(html, start)
i2 <- regexpr(close_tag, rest, fixed = TRUE)
readr::write_file(paste0(substring(html, 1, start - 1), json, substring(rest, i2)), html_file)

message(sprintf("Embedded %s into %s (years: %s). Double-click it to view.",
                basename(files), html_file, paste(payload$years, collapse = ", ")))
