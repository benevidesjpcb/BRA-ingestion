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

# ---- decide which days we still need ----------------------------------------
# A past day already saved is skipped; today is always refreshed.
all_days <- seq(start, end, by = "day")
need <- Filter(function(d) {
  out_json <- file.path(raw_dir, sprintf("tatic-%s.json", fmt(d)))
  have_it  <- file.exists(out_json) && file.info(out_json)$size > 0
  !(have_it && d < today)                     # keep only days we must fetch
}, all_days)
skipped <- length(all_days) - length(need)

# ---- fetch the needed days in small parallel batches ------------------------
# The API serves ONE request at a time: a day fetched alone downloads fine, but
# concurrent requests sit in the server's queue and time out (observed: with 3
# at once, one day completed and the other two got 0 - few bytes before the
# timeout). So the default is SEQUENTIAL (concurrency = 1). Each day is still
# saved immediately, so progress persists across interrupts and a re-run resumes
# from the first missing day. You can try raising TATIC_CONCURRENCY, but the
# server is effectively serial, so it usually just causes timeouts.
concurrency <- as.integer(Sys.getenv("TATIC_CONCURRENCY", unset = "1"))
downloaded  <- 0L
failed      <- character(0)

fetch_batch <- function(days_batch) {
  reqs <- lapply(days_batch, function(d) {
    httr2::request(base_url) |>
      httr2::req_url_query(token = token, datai = fmt(d), dataf = fmt(d + 1)) |>
      httr2::req_user_agent("BRA-ingestion/tatic") |>
      httr2::req_timeout(300) |>                   # generous: a full 6-7 MB day is slow
      httr2::req_throttle(rate = concurrency) |>
      httr2::req_retry(max_tries = 4)
  })
  tmp   <- vapply(days_batch, function(d) tempfile(fileext = ".json"), character(1))
  final <- vapply(days_batch, function(d)
    file.path(raw_dir, sprintf("tatic-%s.json", fmt(d))), character(1))

  resps <- httr2::req_perform_parallel(
    reqs, paths = tmp, on_error = "continue",
    max_active = concurrency, progress = FALSE
  )

  for (i in seq_along(resps)) {
    r <- resps[[i]]
    http_ok <- inherits(r, "httr2_response") &&
      httr2::resp_status(r) == 200 &&
      file.exists(tmp[i]) && file.info(tmp[i])$size > 0
    parsed <- if (http_ok)
      tryCatch(jsonlite::fromJSON(tmp[i], simplifyVector = FALSE),
               error = function(e) NULL) else NULL

    if (!is.null(parsed)) {
      file.copy(tmp[i], final[i], overwrite = TRUE)   # persist immediately
      message(sprintf("  ok     %s  (%d record(s))",
                      basename(final[i]), length(parsed)))
      downloaded <<- downloaded + 1L
    } else {
      why <- if (inherits(r, "httr2_response") && httr2::resp_status(r) != 200)
               paste0("HTTP ", httr2::resp_status(r))
             else if (inherits(r, "condition")) conditionMessage(r)
             else "timeout/invalid JSON"
      message(sprintf("  FAILED %s  (%s)", basename(final[i]), why))
      failed <<- c(failed, fmt(days_batch[[i]]))
    }
    unlink(tmp[i])
  }
}

if (length(need) > 0) {
  batches <- split(need, ceiling(seq_along(need) / concurrency))
  message(sprintf("Fetching %d day(s) in %d batch(es) of up to %d ...",
                  length(need), length(batches), concurrency))
  for (b in seq_along(batches)) {
    fetch_batch(batches[[b]])
    # progress line: every batch when parallel, every 10 days when sequential
    if (concurrency > 1 || b %% 10 == 0)
      message(sprintf("  ...%d/%d done  (saved: %d, failed: %d)",
                      b * concurrency, length(need), downloaded, length(failed)))
  }
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
