#!/usr/bin/env Rscript
# =============================================================================
# download_tatic.R
#
# Incrementally downloads TATIC movement data from the CGNA/DECEA API into
# data-raw/tatic/, then flattens everything to CSV (via ingest_tatic.R).
#
# IMPORTANT: the TATIC API serves ONE DAY per call. Passing a wide window (e.g.
# datai=20260101 dataf=20260130) returns only the first day, not the range.
# So this script walks the period ONE DAY AT A TIME: for each day d it requests
# datai=d, dataf=d+1 (the exclusive next day, exactly like the API example).
#
# It fills [start .. today]. On any PC it looks at what is already in
# data-raw/tatic/ and only downloads the days it is still missing; the current
# day is always refreshed because it can still receive new movements.
#
#   Rscript download_tatic.R                 # from Jan 1 of this year to today
#   Rscript download_tatic.R 20250101        # from a given start date to today
#   Rscript download_tatic.R 20250101 20250630   # an explicit start/end range
#
# The token is read from the environment (never hardcoded):
#   export TATIC_TOKEN="your-token"        (or put it in .Renviron)
#
# API (GET, per day):
#   https://portal.cgna.decea.mil.br/apiv1/tatic?token=...&datai=YYYYMMDD&dataf=YYYYMMDD
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

raw_dir <- file.path("data-raw", "tatic")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

today <- Sys.Date()

# ---- resolve the [start .. end] period --------------------------------------
as_ymd <- function(s) as.Date(s, format = "%Y%m%d")
fmt    <- function(d) format(as.Date(d), "%Y%m%d")

args  <- commandArgs(trailingOnly = TRUE)
start <- if (length(args) >= 1) as_ymd(args[1]) else as.Date(format(today, "%Y-01-01"))
end   <- if (length(args) >= 2) as_ymd(args[2]) else today
if (is.na(start) || is.na(end)) stop("Dates must be YYYYMMDD, e.g. 20250101.")
if (start > end) stop("start (", fmt(start), ") is after end (", fmt(end), ").")

n_days <- as.integer(end - start) + 1L
message(sprintf("TATIC period: %s -> %s  (%d day-by-day call(s))",
                fmt(start), fmt(end), n_days))

# ---- download one day -------------------------------------------------------
# Writes the day's JSON only on success and returns the record count. Any HTTP,
# network, timeout or JSON problem raises an error (handled per-day by the loop),
# and NO file is written, so a re-run retries exactly that day.
fetch_day <- function(datai, dataf, out_json) {
  resp <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = datai, dataf = dataf) |>
    httr2::req_user_agent("BRA-ingestion/tatic") |>
    httr2::req_timeout(120) |>                 # fail instead of hanging forever
    httr2::req_throttle(rate = 2) |>           # be polite: <= 2 req/s
    httr2::req_retry(max_tries = 5) |>         # backoff on 429/5xx/transient
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200)
    stop("HTTP ", httr2::resp_status(resp))

  body <- httr2::resp_body_string(resp)
  if (!jsonlite::validate(body))
    stop("response is not valid JSON (first 120 chars: ",
         substr(body, 1, 120), ")")

  writeLines(body, out_json)                   # only reached on success
  n <- tryCatch(length(jsonlite::fromJSON(body, simplifyVector = FALSE)),
                error = function(e) NA_integer_)
  ifelse(is.na(n), NA_integer_, n)
}

# ---- walk the period one day at a time --------------------------------------
downloaded <- 0L
skipped    <- 0L
failed     <- character(0)
day <- start

while (day <= end) {
  datai <- fmt(day)
  dataf <- fmt(day + 1)                       # exclusive next day (per API example)
  out_json <- file.path(raw_dir, sprintf("tatic-%s.json", datai))

  complete <- day < today                     # a past day never changes
  have_it  <- file.exists(out_json) && file.info(out_json)$size > 0

  if (have_it && complete) {
    skipped <- skipped + 1L
  } else {
    reason <- if (have_it) "refresh (today)" else "download"
    # one bad day must not abort the whole run: log it and move on
    n <- tryCatch(fetch_day(datai, dataf, out_json),
                  error = function(e) {
                    message(sprintf("  FAILED           %s  (%s)",
                                    basename(out_json), conditionMessage(e)))
                    NULL
                  })
    if (is.null(n)) {
      failed <- c(failed, datai)
    } else {
      message(sprintf("  %-16s %s  (%s record(s))",
                      reason, basename(out_json),
                      ifelse(is.na(n), "?", n)))
      downloaded <- downloaded + 1L
    }
  }

  day <- day + 1
}

message(sprintf("Done: %d day(s) downloaded/refreshed, %d already present, %d failed.",
                downloaded, skipped, length(failed)))
if (length(failed) > 0)
  message("Failed day(s): ", paste(failed, collapse = ", "),
          "\n  -> just re-run  Rscript download_tatic.R  to retry only these.")

# ---- flatten everything in data-raw/tatic/ to CSV ---------------------------
if (file.exists("ingest_tatic.R")) {
  message("Flattening to CSV via ingest_tatic.R ...")
  source("ingest_tatic.R")
} else {
  message("Raw JSON saved. Run  Rscript ingest_tatic.R  to build the CSV.")
}
