#!/usr/bin/env Rscript
# =============================================================================
# download_tatic.R
#
# Incrementally downloads TATIC movement data from the CGNA/DECEA API into
# data-raw/tatic/, then flattens everything to CSV (via ingest_tatic.R).
#
# It fills the period [start .. today] in windows of at most 30 days (the API
# limit). On any PC it looks at what is already in data-raw/tatic/ and only
# downloads the windows it is still missing, so you never re-download data you
# already have. The window that includes today is always refreshed because it
# can still receive new movements.
#
#   Rscript download_tatic.R                 # from Jan 1 of this year to today
#   Rscript download_tatic.R 20250101        # from a given start date to today
#   Rscript download_tatic.R 20250101 20250630   # an explicit start/end range
#
# The token is read from the environment (never hardcoded):
#   export TATIC_TOKEN="your-token"        (or put it in .Renviron)
#
# API (GET): https://portal.cgna.decea.mil.br/apiv1/tatic?token=...&datai=YYYYMMDD&dataf=YYYYMMDD
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. Install it with install.packages('httr2').")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').")
})

# ---- configuration ----------------------------------------------------------
base_url <- Sys.getenv("TATIC_URL", unset = "https://portal.cgna.decea.mil.br/apiv1/tatic")
token    <- Sys.getenv("TATIC_TOKEN", unset = "")
if (!nzchar(token)) {
  stop("TATIC_TOKEN is not set. Run:  export TATIC_TOKEN=\"your-token\"  ",
       "(or put it in .Renviron) before this script.")
}

raw_dir  <- file.path("data-raw", "tatic")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

max_days <- 30                 # API limit per call
today    <- Sys.Date()

# ---- resolve the [start .. end] period --------------------------------------
as_ymd <- function(s) as.Date(s, format = "%Y%m%d")
fmt    <- function(d) format(as.Date(d), "%Y%m%d")

args  <- commandArgs(trailingOnly = TRUE)
start <- if (length(args) >= 1) as_ymd(args[1]) else as.Date(format(today, "%Y-01-01"))
end   <- if (length(args) >= 2) as_ymd(args[2]) else today
if (is.na(start) || is.na(end)) stop("Dates must be YYYYMMDD, e.g. 20250101.")
if (start > end) stop("start (", fmt(start), ") is after end (", fmt(end), ").")

message(sprintf("TATIC period: %s -> %s  (windows of <= %d days)",
                fmt(start), fmt(end), max_days))

# ---- download one window ----------------------------------------------------
fetch_window <- function(datai, dataf, out_json) {
  resp <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = datai, dataf = dataf) |>
    httr2::req_user_agent("BRA-ingestion/tatic") |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200)
    stop("TATIC API returned HTTP ", httr2::resp_status(resp),
         " for window ", datai, "-", dataf, ". Check the token/dates.")

  body <- httr2::resp_body_string(resp)
  if (!jsonlite::validate(body))
    stop("TATIC response for ", datai, "-", dataf,
         " is not valid JSON. First 200 chars:\n", substr(body, 1, 200))

  writeLines(body, out_json)
  n <- tryCatch(length(jsonlite::fromJSON(body, simplifyVector = FALSE)),
                error = function(e) NA_integer_)
  ifelse(is.na(n), "?", n)
}

# ---- walk the period in <= 30-day windows -----------------------------------
downloaded <- 0L
skipped    <- 0L
chunk_start <- start

while (chunk_start <= end) {
  chunk_end <- chunk_start + (max_days - 1)   # fixed 30-day boundary (stable filename)
  datai <- fmt(chunk_start)
  dataf <- fmt(chunk_end)
  out_json <- file.path(raw_dir, sprintf("tatic-%s-%s.json", datai, dataf))

  # a window is "complete" only when its last day is already in the past
  complete <- chunk_end < today
  have_it  <- file.exists(out_json) && file.info(out_json)$size > 0

  if (have_it && complete) {
    message("  skip (already have): ", basename(out_json))
    skipped <- skipped + 1L
  } else {
    # request only up to today (asking for future days is pointless)
    dataf_req <- fmt(min(chunk_end, today))
    reason <- if (have_it) "refresh (open window)" else "download"
    n <- fetch_window(datai, dataf_req, out_json)
    message(sprintf("  %s: %s  (%s record(s))", reason, basename(out_json), n))
    downloaded <- downloaded + 1L
  }

  chunk_start <- chunk_end + 1
}

message(sprintf("Done: %d window(s) downloaded/refreshed, %d already present.",
                downloaded, skipped))

# ---- flatten everything in data-raw/tatic/ to CSV ---------------------------
if (file.exists("ingest_tatic.R")) {
  message("Flattening to CSV via ingest_tatic.R ...")
  source("ingest_tatic.R")
} else {
  message("Raw JSON saved. Run  Rscript ingest_tatic.R  to build the CSV.")
}
