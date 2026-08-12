#!/usr/bin/env Rscript
# =============================================================================
# download_tatic.R
#
# Downloads TATIC movement data from the CGNA/DECEA API into the same shape as
# every other dataset in this project:
#
#   data-raw/tatic/parts/tatic_2025-01.csv   one file per MONTH (fetched day by day)
#   data-raw/tatic/tatic_2025.csv            the months merged into one YEAR
#
#   source(here::here("API_TATIC", "download_tatic.R"))
#   download_tatic(2025)                 # a whole year
#   download_tatic(2025, from = "20250101", to = "20250630")
#
# or as a script:
#
#   Rscript API_TATIC/download_tatic.R 2025
#
# ---------------------------------------------------------------------------
# ONE DAY PER CALL. Unlike ODIN, this API ignores a wide window: asking for
# datai=20250101 dataf=20250130 returns ONLY the first day. So the period is
# walked one day at a time — datai=d, dataf=d+1 — and the days are accumulated
# into the month file. The month is the unit of storage; the day is the unit of
# fetching.
#
# RESUMABLE AT THE DAY. Each day is appended to its month file the moment it
# arrives, and a re-run reads the days already in that file and asks only for
# what is missing. An interrupt costs at most one day, never a month. The day is
# recorded in an added column, TATIC_DAY, rather than inferred from the record —
# the source's own date fields describe the EVENT, and a movement can be
# reported on a day other than the one it is filed under.
#
# SERIAL BY DEFAULT. The API queues concurrent requests until they time out
# (observed: with 3 at once, one day completed and the other two returned a few
# bytes). TATIC_CONCURRENCY exists but raising it usually just costs time.
#
# The token is read from the environment, never hardcoded:
#   TATIC_TOKEN="..."      in .Renviron (git-ignored), or the shell environment
#
# API (GET, per day):
#   https://portal.cgna.decea.mil.br/apiv1/tatic?token=...&datai=YYYYMMDD&dataf=YYYYMMDD
# =============================================================================

suppressPackageStartupMessages({
  for (p in c("httr2", "jsonlite", "data.table"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' is required. install.packages('", p, "')")
})

TATIC_SEP <- ";"   # same delimiter as every other raw file in data-raw/

# ---- JSON arrays -> one string ----------------------------------------------
# A record can carry a nested array (a list of sectors, say). A list column
# cannot be written to CSV, so it is collapsed to a pipe-separated string — the
# same convention the ODIN downloader uses for its own array columns.
tatic_flatten_lists <- function(df) {
  for (nm in names(df)) {
    if (is.list(df[[nm]]))
      df[[nm]] <- vapply(df[[nm]], function(v) {
        if (is.null(v) || length(v) == 0) NA_character_
        else paste(unlist(v), collapse = "|")
      }, character(1))
  }
  df
}

# ---- one day from the API ----------------------------------------------------
# Returns a data.frame (possibly 0 rows) or NULL when the request failed. The
# difference matters: 0 rows is an answer ("no movements that day"), NULL is a
# failure that must be retried.
tatic_fetch_day <- function(day, token, base_url, timeout = 300) {
  fmt <- function(d) format(as.Date(d), "%Y%m%d")
  req <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = fmt(day), dataf = fmt(day + 1)) |>
    httr2::req_user_agent("BRA-ingestion/tatic") |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 4)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (!inherits(resp, "httr2_response")) return(NULL)
  if (httr2::resp_status(resp) != 200) return(NULL)

  body <- httr2::resp_body_string(resp)
  if (!jsonlite::validate(body)) return(NULL)
  df <- tryCatch(jsonlite::fromJSON(body, simplifyDataFrame = TRUE, flatten = TRUE),
                 error = function(e) NULL)
  if (is.null(df)) return(NULL)
  if (!is.data.frame(df)) {
    if (length(df) == 0) return(data.frame())      # empty day
    df <- tryCatch(as.data.frame(df), error = function(e) NULL)
    if (is.null(df)) return(NULL)
  }
  if (nrow(df) == 0) return(data.frame())
  df <- tatic_flatten_lists(df)
  df[] <- lapply(df, as.character)
  df$TATIC_DAY <- format(as.Date(day))             # the day WE asked for
  df
}

# ---- part files --------------------------------------------------------------
tatic_read_csv <- function(path) {
  as.data.frame(data.table::fread(file = path, sep = TATIC_SEP,
                                  colClasses = "character", na.strings = "",
                                  showProgress = FALSE, fill = Inf,
                                  header = TRUE))
}
tatic_write_csv <- function(df, path) {
  data.table::fwrite(df, path, sep = TATIC_SEP, na = "", quote = TRUE)
}
tatic_rbind_fill <- function(lst) {
  lst  <- Filter(function(d) !is.null(d) && nrow(d) > 0, lst)
  if (length(lst) == 0) return(NULL)
  cols <- unique(unlist(lapply(lst, names)))
  lst  <- lapply(lst, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] <- NA_character_
    d[cols]
  })
  do.call(rbind, lst)
}

# which days does a month part already hold?
tatic_days_in_part <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(character(0))
  d <- tryCatch(
    data.table::fread(file = path, sep = TATIC_SEP, select = "TATIC_DAY",
                      colClasses = "character", showProgress = FALSE,
                      fill = Inf, header = TRUE)[[1]],
    error = function(e) NULL)
  if (is.null(d)) return(character(0))
  unique(d[!is.na(d)])
}

# =============================================================================
# download_tatic(years, from, to, out_dir, force)
#
#   years   : years to build, e.g. 2025 or 2024:2026 (default: current year)
#   from/to : optional "YYYYMMDD" bounds INSIDE those years, for a partial run
#   out_dir : default data-raw/tatic
#   force   : TRUE re-fetches days already stored
#
# Returns, invisibly, the year files written.
# =============================================================================
download_tatic <- function(years   = as.integer(format(Sys.Date(), "%Y")),
                           from    = NULL,
                           to      = NULL,
                           out_dir = here::here("data-raw", "tatic"),
                           force   = FALSE,
                           base_url = Sys.getenv(
                             "TATIC_URL",
                             unset = "https://portal.cgna.decea.mil.br/apiv1/tatic")) {

  token <- Sys.getenv("TATIC_TOKEN", unset = "")
  if (!nzchar(token))
    stop("TATIC_TOKEN is not set. Put it in .Renviron (git-ignored):\n",
         "  TATIC_TOKEN=your-token\n",
         "and restart R. Never write the token into a script.")

  parts_dir <- file.path(out_dir, "parts")
  for (d in c(out_dir, parts_dir))
    if (!dir.exists(d)) { dir.create(d, recursive = TRUE); message("Created ", d) }

  years <- suppressWarnings(as.integer(years))
  if (length(years) == 0 || any(is.na(years)))
    stop("Years must be 4-digit numbers, e.g. 2025 or 2024:2026.")

  as_ymd <- function(s) if (is.null(s)) NULL else as.Date(as.character(s), "%Y%m%d")
  lo <- as_ymd(from); hi <- as_ymd(to)
  today       <- Sys.Date()
  concurrency <- as.integer(Sys.getenv("TATIC_CONCURRENCY", unset = "1"))
  written     <- character(0)

  for (yr in years) {
    year_start <- as.Date(sprintf("%d-01-01", yr))
    year_end   <- min(as.Date(sprintf("%d-12-31", yr)), today)
    if (!is.null(lo)) year_start <- max(year_start, lo)
    if (!is.null(hi)) year_end   <- min(year_end, hi)
    if (year_start > year_end) {
      message(sprintf("Year %d: nothing in range.", yr)); next
    }
    message(sprintf("Year %d: %s -> %s", yr, year_start, year_end))

    months <- unique(format(seq(year_start, year_end, by = "day"), "%Y-%m"))
    for (ym in months) {
      part_csv <- file.path(parts_dir, sprintf("tatic_%s.csv", ym))
      m_first  <- as.Date(paste0(ym, "-01"))
      m_last   <- min(seq(m_first, by = "month", length.out = 2)[2] - 1, year_end)
      m_first  <- max(m_first, year_start)
      want     <- format(seq(m_first, m_last, by = "day"))

      have <- if (force) character(0) else tatic_days_in_part(part_csv)
      # today is always refetched: it can still receive movements
      have <- setdiff(have, format(today))
      need <- setdiff(want, have)

      if (length(need) == 0) {
        message(sprintf("  %s  skip (all %d day(s) already stored)", ym, length(want)))
        next
      }
      message(sprintf("  %s  fetching %d of %d day(s) ...", ym, length(need), length(want)))

      # rows already in the part, minus any day we are refetching
      keep <- NULL
      if (file.exists(part_csv)) {
        old  <- tatic_read_csv(part_csv)
        keep <- old[!(old$TATIC_DAY %in% need), , drop = FALSE]
      }

      fetched <- list()
      failed  <- character(0)
      for (i in seq_along(need)) {
        day <- as.Date(need[i])
        df  <- tatic_fetch_day(day, token, base_url)
        if (is.null(df)) {
          message(sprintf("    %s  FAILED (retried; will be picked up next run)", need[i]))
          failed <- c(failed, need[i])
          next
        }
        if (nrow(df) == 0) {
          # A day with no movements is an answer, not a failure. It is recorded
          # as a single row carrying only the day, so the resume logic knows the
          # day was asked for and does not request it again on every run.
          df <- data.frame(TATIC_DAY = need[i], stringsAsFactors = FALSE)
          message(sprintf("    %s  no records", need[i]))
        } else {
          message(sprintf("    %s  %d record(s)", need[i], nrow(df)))
        }
        fetched[[length(fetched) + 1L]] <- df

        # persist as we go: an interrupt costs one day, not the month
        if (i %% 5 == 0 || i == length(need)) {
          part <- tatic_rbind_fill(c(list(keep), fetched))
          if (!is.null(part)) {
            part <- part[order(part$TATIC_DAY), , drop = FALSE]
            tatic_write_csv(part, part_csv)
          }
        }
      }
      if (length(failed) > 0)
        message(sprintf("  %s  %d day(s) failed: %s", ym, length(failed),
                        paste(failed, collapse = ", ")))
    }

    # ---- merge the months into the year file --------------------------------
    part_files <- list.files(parts_dir,
                             pattern = sprintf("^tatic_%d-[0-9]{2}\\.csv$", yr),
                             full.names = TRUE)
    if (length(part_files) == 0) {
      message(sprintf("Year %d: no months on disk; nothing merged.", yr)); next
    }
    combined <- tatic_rbind_fill(lapply(part_files, tatic_read_csv))
    if (is.null(combined)) {
      message(sprintf("Year %d: months are empty; nothing written.", yr)); next
    }
    # the placeholder rows for empty days are storage bookkeeping, not data
    real <- combined[rowSums(!is.na(combined[setdiff(names(combined), "TATIC_DAY")])) > 0,
                     , drop = FALSE]
    real <- real[order(real$TATIC_DAY), , drop = FALSE]

    out_csv <- file.path(out_dir, sprintf("tatic_%d.csv", yr))
    tatic_write_csv(real, out_csv)
    written <- c(written, out_csv)
    message(sprintf("Year %d: merged %d month(s) -> %d record(s), %d column(s) -> %s",
                    yr, length(part_files), nrow(real), ncol(real), out_csv))

    # ---- days with no records ------------------------------------------------
    # Reported, never silently tolerated: this is exactly how the dstaxi hole on
    # 2025-10-31 stayed invisible. A day that was never asked for and a day the
    # source has nothing for are different things, and both are listed.
    asked <- unique(combined$TATIC_DAY)
    want  <- format(seq(as.Date(sprintf("%d-01-01", yr)),
                        min(as.Date(sprintf("%d-12-31", yr)), today - 1), by = "day"))
    never <- setdiff(want, asked)
    empty <- setdiff(intersect(want, asked), unique(real$TATIC_DAY))
    show  <- function(x) paste(c(utils::head(x, 10),
                                 if (length(x) > 10) sprintf("... (+%d)", length(x) - 10)),
                               collapse = ", ")
    if (length(never) > 0)
      message(sprintf("  NOTE: %d day(s) never downloaded: %s", length(never), show(never)))
    if (length(empty) > 0)
      message(sprintf("  NOTE: %d day(s) the source has no records for: %s",
                      length(empty), show(empty)))
  }

  invisible(written)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) download_tatic()
  else download_tatic(years = as.integer(args[1]),
                      from  = if (length(args) >= 2) args[2] else NULL,
                      to    = if (length(args) >= 3) args[3] else NULL)
}
