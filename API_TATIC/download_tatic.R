#!/usr/bin/env Rscript
# =============================================================================
# download_tatic.R
#
# Incrementally downloads TATIC movement data from the CGNA/DECEA API into
# data-raw/tatic/, one JSON file per day, then flattens everything to CSV
# (via ingest_tatic.R).
#
#   source(here::here("API_TATIC", "download_tatic.R"))
#   download_tatic()                              # Jan 1 of this year -> today
#   download_tatic("20250101")                    # from a date -> today
#   download_tatic("20250101", "20250630")        # an explicit range
#
# or as a script:
#
#   Rscript API_TATIC/download_tatic.R 20250101 20250630
#
# ONE DAY PER CALL. Passing a wide window (datai=20260101 dataf=20260130)
# returns only the first day, not the range — so the period is walked one day at
# a time: for each day d it requests datai=d, dataf=d+1 (the exclusive next day,
# exactly like the API example).
#
# RESUMABLE. It looks at what is already in data-raw/tatic/ and only fetches the
# days still missing; the current day is always refreshed, because it can still
# receive new movements. Each day is written the moment it arrives, so an
# interrupted run keeps everything already fetched.
#
# The token is read from the environment, never hardcoded:
#   TATIC_TOKEN="..."      in .Renviron (git-ignored), or the shell environment
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

# =============================================================================
# download_tatic(from, to, out_dir, ...)
#
#   from    : first day, "YYYYMMDD" or a Date (default: Jan 1 of the current year)
#   to      : last day, inclusive (default: today)
#   out_dir : where the per-day JSON goes (default: data-raw/tatic)
#   ingest  : TRUE also rebuilds the combined CSV afterwards
#   force   : TRUE re-fetches days already on disk
#
# Returns, invisibly, a list(downloaded, skipped, failed).
# =============================================================================
download_tatic <- function(from    = NULL,
                           to      = NULL,
                           out_dir = here::here("data-raw", "tatic"),
                           ingest  = TRUE,
                           force   = FALSE,
                           base_url = Sys.getenv(
                             "TATIC_URL",
                             unset = "https://portal.cgna.decea.mil.br/apiv1/tatic")) {

  token <- Sys.getenv("TATIC_TOKEN", unset = "")
  if (!nzchar(token))
    stop("TATIC_TOKEN is not set. Put it in .Renviron (git-ignored):\n",
         "  TATIC_TOKEN=your-token\n",
         "and restart R. Never write the token into a script.")

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message("Created ", out_dir)
  }

  today <- Sys.Date()
  as_ymd <- function(s) {
    if (inherits(s, "Date")) return(s)
    as.Date(as.character(s), format = "%Y%m%d")
  }
  fmt <- function(d) format(as.Date(d), "%Y%m%d")

  start <- if (is.null(from)) as.Date(format(today, "%Y-01-01")) else as_ymd(from)
  end   <- if (is.null(to))   today                              else as_ymd(to)
  if (is.na(start) || is.na(end)) stop("Dates must be YYYYMMDD, e.g. 20250101.")
  if (start > end) stop("from (", fmt(start), ") is after to (", fmt(end), ").")

  message(sprintf("TATIC period: %s -> %s  (%d day-by-day call(s))",
                  fmt(start), fmt(end), as.integer(end - start) + 1L))

  # ---- which days do we still need? ---------------------------------------
  # A past day already saved is skipped; today is always refreshed.
  day_file <- function(d) file.path(out_dir, sprintf("tatic-%s.json", fmt(d)))
  all_days <- seq(start, end, by = "day")
  need <- Filter(function(d) {
    if (force) return(TRUE)
    have <- file.exists(day_file(d)) && file.info(day_file(d))$size > 0
    !(have && d < today)
  }, all_days)
  skipped <- length(all_days) - length(need)

  # ---- fetch ---------------------------------------------------------------
  # The API is effectively SERIAL: a day fetched alone downloads fine, but
  # concurrent requests sit in the server's queue and time out (observed: with 3
  # at once, one day completed and the other two returned a few bytes before the
  # timeout). Hence concurrency 1 by default. Raising TATIC_CONCURRENCY usually
  # just produces timeouts.
  concurrency <- as.integer(Sys.getenv("TATIC_CONCURRENCY", unset = "1"))
  downloaded  <- 0L
  failed      <- character(0)

  fetch_batch <- function(days_batch) {
    reqs <- lapply(days_batch, function(d) {
      httr2::request(base_url) |>
        httr2::req_url_query(token = token, datai = fmt(d), dataf = fmt(d + 1)) |>
        httr2::req_user_agent("BRA-ingestion/tatic") |>
        httr2::req_timeout(300) |>          # generous: a full 6-7 MB day is slow
        httr2::req_throttle(rate = concurrency) |>
        httr2::req_retry(max_tries = 4)
    })
    tmp   <- vapply(days_batch, function(d) tempfile(fileext = ".json"), character(1))
    final <- vapply(days_batch, day_file, character(1))

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
      if (concurrency > 1 || b %% 10 == 0)
        message(sprintf("  ...%d/%d done  (saved: %d, failed: %d)",
                        min(b * concurrency, length(need)), length(need),
                        downloaded, length(failed)))
    }
  }

  message(sprintf("Done: %d day(s) downloaded/refreshed, %d already present, %d failed.",
                  downloaded, skipped, length(failed)))
  if (length(failed) > 0)
    message("Failed day(s): ", paste(failed, collapse = ", "),
            "\n  -> re-run download_tatic() to retry only these.")

  if (ingest) {
    ing <- here::here("API_TATIC", "ingest_tatic.R")
    if (file.exists(ing)) {
      message("Flattening to CSV via ingest_tatic.R ...")
      source(ing)
      ingest_tatic(raw_dir = out_dir)
    } else {
      message("Raw JSON saved. Run ingest_tatic() to build the CSV.")
    }
  }

  invisible(list(downloaded = downloaded, skipped = skipped, failed = failed))
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  download_tatic(from = if (length(args) >= 1) args[1] else NULL,
                 to   = if (length(args) >= 2) args[2] else NULL)
}
