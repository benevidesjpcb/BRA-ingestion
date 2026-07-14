#!/usr/bin/env Rscript
# =============================================================================
# build_dashboard.R
#
# Embeds the taxi-time numbers into index.html so the dashboard opens by simply
# double-clicking the file (offline, no server, no Python).
#
# Run it whenever you add or remove analytic CSVs in data/:
#   - from a terminal:  Rscript build_dashboard.R
#   - from RStudio:     source("build_dashboard.R")
# Then double-click index.html.
#
# It reads every data/PBWG-<BRA|EUR>-txxt-analytic-<YEAR>-ref2024-icao_ganp_p20.csv,
# aggregates the daily rows to per-airport / per-region / monthly averages, and
# writes the result into the <script id="dash-data"> block of index.html.
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install it once with install.packages('jsonlite').")
}

data_dir  <- "data"
ref_year  <- 2024
variant   <- "icao_ganp_p20"
html_file <- "index.html"

pat <- sprintf("^PBWG-(BRA|EUR)-txxt-analytic-([0-9]{4})-ref%d-%s\\.csv$", ref_year, variant)
files <- list.files(data_dir, pattern = pat, full.names = TRUE)
if (length(files) == 0) {
  stop("No analytic CSVs found in ", data_dir, "/ matching ", pat)
}

read_one <- function(f) {
  m <- stringr::str_match(basename(f), "^PBWG-(BRA|EUR)-txxt-analytic-([0-9]{4})-")
  readr::read_csv(
    f, show_col_types = FALSE,
    col_types = readr::cols(DATE = readr::col_character(), .default = readr::col_guess())
  ) |>
    mutate(REGION = m[2], YEAR = m[3]) |>
    filter(substr(DATE, 1, 4) == YEAR)        # keep only the file's own year
}
raw <- purrr::map(files, read_one) |> bind_rows() |> mutate(MONTH = substr(DATE, 6, 7))

agg <- function(df, keys) {
  df |>
    group_by(across(all_of(keys))) |>
    summarise(mvts = sum(MVTS_VALID), na = sum(MVTS_NA),
              txxt = sum(TOT_TXXT), ref = sum(TOT_REF), add = sum(TOT_ADD_TIME),
              .groups = "drop") |>
    mutate(avg_txxt = round(txxt / mvts, 3),
           avg_ref  = round(ref  / mvts, 3),
           avg_add  = round(add  / mvts, 3),
           na_share = round(100 * na / (mvts + na), 2))
}
leaf <- function(r) list(mvts = r$mvts, avg_txxt = r$avg_txxt, avg_ref = r$avg_ref,
                         avg_add = r$avg_add, na_share = r$na_share)

# turn a data frame into nested named lists keyed by `keys`, applying `fun` at the leaf
nest <- function(df, keys, fun) {
  if (length(keys) == 0) return(fun(df))
  lapply(split(df, df[[keys[1]]]), function(s) nest(s, keys[-1], fun))
}

ap_df <- agg(raw, c("REGION", "ICAO", "PHASE", "YEAR"))
ov_df <- agg(raw, c("REGION", "YEAR", "PHASE"))
mo_df <- raw |>
  group_by(REGION, YEAR, PHASE, MONTH) |>
  summarise(avg_add = round(sum(TOT_ADD_TIME) / sum(MVTS_VALID), 3), .groups = "drop")

airports <- nest(ap_df, c("REGION", "ICAO", "PHASE", "YEAR"), leaf)
overall  <- nest(ov_df, c("REGION", "YEAR", "PHASE"),          leaf)
monthly  <- nest(mo_df, c("REGION", "YEAR", "PHASE", "MONTH"), function(r) r$avg_add)

partial <- list()
pd <- raw |> group_by(REGION, YEAR) |> summarise(maxd = max(DATE), .groups = "drop") |>
  filter(maxd < paste0(YEAR, "-12-31"))
for (i in seq_len(nrow(pd))) partial[[pd$REGION[i]]][[pd$YEAR[i]]] <- pd$maxd[i]

payload <- list(airports = airports, overall = overall, monthly = monthly,
                meta = list(partial = partial), years = sort(unique(raw$YEAR)))
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

message(sprintf("Embedded %d files into %s (years: %s). Double-click it to view.",
                length(files), html_file, paste(payload$years, collapse = ", ")))
