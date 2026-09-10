#!/usr/bin/env Rscript
# =============================================================================
# download_taxi_cgna.R
#
# The taxi-time table AS THE CGNA SERVES IT -- the same `dstaxi` the ODIN API
# serves, from the other side -- into one file per year, under a name that can
# never be confused with the ODIN one:
#
#   data-raw/dstaxi/dsTaxi<year>cgna.csv                    the year
#   data-raw/dstaxi/parts/dsTaxi<year>cgna_<YYYY-MM>.csv    one file per MONTH
#
#   source(here::here("TAXI", "download_taxi_cgna.R"))
#   download_taxi_cgna(2025:2026)
#   download_taxi_cgna(2025, from = "20250310", to = "20250310")   # one day
#
# or as a script:
#
#   Rscript TAXI/download_taxi_cgna.R 2025
#
# ---------------------------------------------------------------------------
# THIS IS NOT TATIC. TATIC (API_TATIC/) is the CGNA's milestone feed, a
# different table with a different meaning. This endpoint is `dstaxi` itself:
#   https://portal.cgna.decea.mil.br/apiv1/dstaxi?token=...&datai=&dataf=
# Same token as TATIC. NOT the same date format: this endpoint answers
# "Formato de data invalido. Utilize YYYY-MM-DD ou YYYY-MM-DD HH:MM:SS" to the
# YYYYMMDD that TATIC requires. What comes back is the taxi table, so there is
# nothing to harmonise -- the columns are already the ones the rest of the
# project reads.
#
# WHY TWO SOURCES FOR ONE TABLE. The ODIN API (TAXI/download_taxi.R) serves the
# same `dstaxi`. Whether the two agree movement by movement is a question worth
# asking rather than assuming, which is why the CGNA years are written beside
# the ODIN ones under their own name instead of over them. The inventory in
# _chapter-setup.R matches `^dsTaxi20\d{2}\.csv$`, so these files sit in the
# same folder without ever being picked up as if they were the ODIN download.
#
# ONE DAY PER CALL, AND THE DAY IS CUT LOCALLY. The window is walked day by
# day, as with TATIC: this API family has answered a wide window with only its
# first day. Whether `dataf` is inclusive is not documented and not worth
# guessing -- asked as [d, d+1] an exclusive bound gives the day and an
# inclusive one gives two, which would file the same movement under two days.
# So the wide bound is asked for and the answer is TRIMMED to the day, on
# `dh_bimtra`: the movement stamp, the same column the ODIN download anchors
# its month windows on. The API's own semantics then cannot produce a duplicate
# or a hole either way.
# The day asked for is recorded in an added column, CGNA_DAY, rather than
# inferred from the record -- the source's stamps describe the MOVEMENT, and a
# movement can be reported on a day other than the one it is filed under. That
# column is what makes a re-run resumable at the day: an interrupt costs one
# day, never a month.
#
# PAGINATED, AND THE ENVELOPE SAYS SO. The answer is not a bare array but an
# object: the rows, plus `page`, `per_page`, `total` and `total_pages`. Keeping
# only the first page silently caps every day at per_page (1000) -- a whole year
# came back the same size as a busy month before this was handled. Every page is
# fetched, and the rows are counted against the `total` the API itself
# reported: a day that does not add up is named rather than quietly stored
# short.
#
# The token is read from the environment, never hardcoded -- the same
# TATIC_TOKEN in .Renviron (git-ignored) that download_tatic() uses.
# =============================================================================

# The CGNA day-walk helpers (fetch, the CSV read/write conventions, the proxy)
# live with the other CGNA downloader; sourcing only defines functions.
source(here::here("API_TATIC", "download_tatic.R"))

CGNA_TAXI_URL <- Sys.getenv("CGNA_TAXI_URL",
                            unset = "https://portal.cgna.decea.mil.br/apiv1/dstaxi")

# Rows per page. 1000 is what the API returns when not asked; raising it (if the
# endpoint allows) means fewer requests for the same day, nothing more.
CGNA_TAXI_PAGE_SIZE <- as.integer(Sys.getenv("CGNA_TAXI_PAGE_SIZE", unset = "1000"))

# API spelling -> the spelling already on disk. The ODIN download does the same
# four renames (see TAXI/download_taxi.R note 3), so a CGNA year is read by
# exactly the same code as an ODIN year and as a year that came from the zip.
CGNA_TAXI_RENAME <- c(dhbimtra     = "dh_bimtra",
                      dhvra        = "dh_vra",
                      matchvra     = "match_vra",
                      vratipolinha = "vra_tipo_linha")

# Applied case-insensitively: the exports and the APIs have disagreed on
# capitalisation before, and a rename that silently does not fire leaves the
# preparation reading a column that is not there.
cgna_taxi_rename <- function(df) {
  nm <- names(df)
  hit <- match(tolower(nm), names(CGNA_TAXI_RENAME))
  nm[!is.na(hit)] <- unname(CGNA_TAXI_RENAME[hit[!is.na(hit)]])
  names(df) <- nm
  df
}

# ---- one day from the API ----------------------------------------------------
# data.frame (possibly 0 rows) on success, NULL on failure. The difference
# matters: 0 rows is an answer ("no movements that day"), NULL must be retried.
# ---- keep only the day we asked for -----------------------------------------
# Returns the rows whose movement stamp falls on `day`. A row with no usable
# stamp is KEPT: dropping it would silently lose a movement over a parsing
# question, and the CGNA_DAY column still records which request it arrived in.
cgna_taxi_trim_day <- function(df, day) {
  if (is.null(df) || nrow(df) == 0 || !"dh_bimtra" %in% names(df)) return(df)
  d <- substr(trimws(df$dh_bimtra), 1, 10)
  keep <- is.na(d) | !nzchar(d) | d == format(as.Date(day))
  if (!all(keep))
    message(sprintf("      (%d row(s) outside %s dropped)",
                    sum(!keep), format(as.Date(day))))
  df[keep, , drop = FALSE]
}

# ---- the paginated envelope -------------------------------------------------
# Returns the pieces of one page: the rows and what the API says about the whole
# answer. The rows are found by shape (the one data.frame in the object) rather
# than by a hardcoded name, because that name is the only part of the contract
# not visible in the envelope itself.
.cgna_page_parts <- function(parsed) {
  num <- function(x) if (is.null(x)) NA_integer_ else suppressWarnings(as.integer(x[[1]]))
  rows <- NULL
  if (is.data.frame(parsed)) rows <- parsed
  else if (is.list(parsed)) {
    df <- Filter(is.data.frame, parsed)
    if (length(df) >= 1) rows <- df[[1]]
    else {
      lst <- Filter(function(x) is.list(x) && !is.data.frame(x), parsed)
      if (length(lst) == 1)
        rows <- tryCatch(as.data.frame(lst[[1]]), error = function(e) NULL)
    }
  }
  list(rows        = rows,
       page        = num(parsed[["page"]]),
       per_page    = num(parsed[["per_page"]]),
       total       = num(parsed[["total"]]),
       total_pages = num(parsed[["total_pages"]]))
}

# ---- one page ----------------------------------------------------------------
# NULL on failure, the parts of the page on success.
cgna_taxi_fetch_page <- function(day, token, page, base_url = CGNA_TAXI_URL,
                                 per_page = CGNA_TAXI_PAGE_SIZE, timeout = 300) {
  fmt <- function(d) format(as.Date(d), "%Y-%m-%d")   # NOT the YYYYMMDD TATIC wants
  req <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = fmt(day), dataf = fmt(day + 1),
                         page = page, per_page = per_page) |>
    httr2::req_user_agent("BRA-ingestion/dstaxi-cgna") |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 4) |>
    bra_proxy()

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (!inherits(resp, "httr2_response")) {
    message("      transport error: ", conditionMessage(resp))
    return(NULL)
  }
  # The API explains its refusals in the body (a bad date format, an expired
  # token). Swallowing that leaves "FAILED" and a trip to the API docs to find
  # out what it already said, so it is shown.
  if (httr2::resp_status(resp) != 200) {
    why <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    message(sprintf("      HTTP %d%s", httr2::resp_status(resp),
                    if (nzchar(why)) paste0(": ", substr(why, 1, 300)) else ""))
    return(NULL)
  }

  body <- httr2::resp_body_string(resp)
  if (!jsonlite::validate(body)) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyDataFrame = TRUE, flatten = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  .cgna_page_parts(parsed)
}

# ---- one whole day, every page ----------------------------------------------
# data.frame (possibly 0 rows) on success, NULL on failure. The difference
# matters: 0 rows is an answer ("no movements that day"), NULL must be retried.
cgna_taxi_fetch_day <- function(day, token, base_url = CGNA_TAXI_URL,
                                per_page = CGNA_TAXI_PAGE_SIZE, timeout = 300) {
  pages <- list()
  page  <- 1L
  total <- NA_integer_
  n_pages <- NA_integer_
  repeat {
    pp <- cgna_taxi_fetch_page(day, token, page, base_url, per_page, timeout)
    if (is.null(pp)) return(NULL)          # a failed page fails the day: a day
                                            # stored half-complete would look
                                            # finished to the resume logic
    if (page == 1L) { total <- pp$total; n_pages <- pp$total_pages }
    rows <- pp$rows
    if (is.null(rows) || nrow(rows) == 0) break
    rows <- tatic_flatten_lists(rows)      # a nested array cannot be written to CSV
    rows[] <- lapply(rows, as.character)
    pages[[length(pages) + 1L]] <- rows

    # Stop on what the envelope says when it says it, and on a short page when
    # it does not: an API that stops reporting total_pages must not turn into an
    # endless loop, and one that reports it must not be probed for a page past
    # the end on every single day.
    if (!is.na(n_pages) && page >= n_pages) break
    if (is.na(n_pages) && nrow(rows) < per_page) break
    page <- page + 1L
    if (page > 1000L) {                     # a guard, not an expectation
      message("      stopped at 1000 pages -- the envelope never ended")
      break
    }
  }

  df <- tatic_rbind_fill(pages)
  if (is.null(df)) return(data.frame())
  # The API told us how many rows the day has. Checking costs nothing and is the
  # difference between a short day that is noticed and one that is not.
  if (!is.na(total) && nrow(df) != total)
    message(sprintf("      WARNING: %d row(s) fetched, API reported total=%d",
                    nrow(df), total))
  df <- cgna_taxi_rename(df)
  df <- cgna_taxi_trim_day(df, day)
  if (nrow(df) == 0) return(data.frame())
  df$CGNA_DAY <- format(as.Date(day))   # the day WE asked for
  df
}

# which days does a month part already hold?
cgna_taxi_days_in_part <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(character(0))
  d <- tryCatch(
    data.table::fread(file = path, sep = TATIC_SEP, select = "CGNA_DAY",
                      colClasses = "character", showProgress = FALSE,
                      fill = Inf, header = TRUE)[[1]],
    error = function(e) NULL)
  if (is.null(d)) return(character(0))
  unique(d[!is.na(d)])
}

# =============================================================================
# download_taxi_cgna(years, from, to, out_dir, force)
#
#   years   : years to build, e.g. 2025 or 2025:2026
#   from/to : optional "YYYYMMDD" bounds INSIDE those years, for a partial run
#   out_dir : default data-raw/dstaxi -- beside the ODIN files
#   force   : TRUE re-fetches days already stored
#
# Returns, invisibly, the year files written.
# =============================================================================
download_taxi_cgna <- function(years    = taxi_cgna_default_years(),
                               from     = NULL,
                               to       = NULL,
                               out_dir  = here::here("data-raw", "dstaxi"),
                               force    = FALSE,
                               base_url = CGNA_TAXI_URL) {

  token <- Sys.getenv("TATIC_TOKEN", unset = "")
  if (!nzchar(token))
    stop("TATIC_TOKEN is not set. Put it in .Renviron (git-ignored):\n",
         "  TATIC_TOKEN=your-token\n",
         "and restart R. Never write the token into a script.")

  years <- suppressWarnings(as.integer(years))
  if (length(years) == 0 || any(is.na(years)))
    stop("Years must be 4-digit numbers, e.g. 2025 or 2025:2026.")

  parts_dir <- file.path(out_dir, "parts")
  for (d in c(out_dir, parts_dir))
    if (!dir.exists(d)) { dir.create(d, recursive = TRUE); message("Created ", d) }

  as_ymd <- function(s) if (is.null(s)) NULL else as.Date(as.character(s), "%Y%m%d")
  lo <- as_ymd(from); hi <- as_ymd(to)
  today   <- Sys.Date()
  written <- character(0)

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
      part_csv <- file.path(parts_dir, sprintf("dsTaxi%dcgna_%s.csv", yr, ym))
      m_first  <- as.Date(paste0(ym, "-01"))
      m_last   <- min(seq(m_first, by = "month", length.out = 2)[2] - 1, year_end)
      m_first  <- max(m_first, year_start)
      want     <- format(seq(m_first, m_last, by = "day"))

      have <- if (force) character(0) else cgna_taxi_days_in_part(part_csv)
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
        keep <- old[!(old$CGNA_DAY %in% need), , drop = FALSE]
      }

      fetched <- list()
      failed  <- character(0)
      for (i in seq_along(need)) {
        day <- as.Date(need[i])
        df  <- cgna_taxi_fetch_day(day, token, base_url)
        if (is.null(df)) {
          message(sprintf("    %s  FAILED (retried; will be picked up next run)", need[i]))
          failed <- c(failed, need[i])
        } else {
          if (nrow(df) == 0) {
            # A day with no movements is an answer, not a failure. Recorded as one
            # row carrying only the day, so the resume logic knows it was asked
            # for and does not request it again on every run.
            df <- data.frame(CGNA_DAY = need[i], stringsAsFactors = FALSE)
            message(sprintf("    %s  no records", need[i]))
          } else {
            message(sprintf("    %s  %d record(s)", need[i], nrow(df)))
          }
          fetched[[length(fetched) + 1L]] <- df
        }

        # persist as we go: an interrupt costs one day, not the month. The
        # checkpoint is OUTSIDE the success branch on purpose: a day that fails
        # must cost that day only. Skipping to the next iteration on failure --
        # as this loop used to -- means a failure on the LAST needed day never
        # reaches the `i == length(need)` checkpoint, and every day fetched
        # since the previous one is discarded. Asked for three days and given a
        # 502 on the third, this wrote none of them.
        if (i %% 5 == 0 || i == length(need)) {
          part <- tatic_rbind_fill(c(list(keep), fetched))
          if (!is.null(part)) {
            part <- part[order(part$CGNA_DAY), , drop = FALSE]
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
                             pattern = sprintf("^dsTaxi%dcgna_%d-[0-9]{2}\\.csv$", yr, yr),
                             full.names = TRUE)
    if (length(part_files) == 0) {
      message(sprintf("Year %d: no month on disk; nothing merged.", yr)); next
    }
    combined <- tatic_rbind_fill(lapply(sort(part_files), tatic_read_csv))
    if (is.null(combined)) {
      message(sprintf("Year %d: months are empty; nothing written.", yr)); next
    }
    # the placeholder rows for empty days are storage bookkeeping, not data
    real <- combined[rowSums(!is.na(combined[setdiff(names(combined), "CGNA_DAY")])) > 0,
                     , drop = FALSE]
    real <- real[order(real$CGNA_DAY), , drop = FALSE]

    out_csv <- file.path(out_dir, sprintf("dsTaxi%dcgna.csv", yr))
    tatic_write_csv(real, out_csv)
    written <- c(written, out_csv)
    message(sprintf("Year %d: merged %d month(s) -> %d record(s), %d column(s) -> %s",
                    yr, length(part_files), nrow(real), ncol(real), out_csv))

    # ---- the gaps, over the RANGE THAT WAS ASKED FOR -------------------------
    # Scoped to the window of this run: reporting 359 missing days after a
    # one-day request is noise, and noise is how a real hole goes unnoticed.
    # A day never asked for and a day the source has nothing for are different
    # things, and both are listed.
    asked <- unique(combined$CGNA_DAY)
    want  <- format(seq(year_start, min(year_end, today - 1), by = "day"))
    never <- setdiff(want, asked)
    empty <- setdiff(intersect(want, asked), unique(real$CGNA_DAY))
    show  <- function(x) paste(c(utils::head(x, 10),
                                 if (length(x) > 10) sprintf("... (+%d)", length(x) - 10)),
                               collapse = ", ")
    if (length(never) > 0)
      message(sprintf("  NOTE: %d day(s) of the requested range not downloaded: %s",
                      length(never), show(never)))
    if (length(empty) > 0)
      message(sprintf("  NOTE: %d day(s) the source has no records for: %s",
                      length(empty), show(empty)))

    # months held outside this run's window, so a partial year is visible
    held <- sort(unique(substr(asked, 1, 7)))
    message(sprintf("  Months on disk for %d: %s", yr, paste(held, collapse = ", ")))
  }

  invisible(written)
}

# The study period comes from _chapter-setup.R (dsTaxi_years) when that has been
# sourced; only a session that never loaded it falls back to the current year.
taxi_cgna_default_years <- function() {
  if (exists("dsTaxi_years", inherits = TRUE)) get("dsTaxi_years", inherits = TRUE)
  else as.integer(format(Sys.Date(), "%Y"))
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) download_taxi_cgna()
  else download_taxi_cgna(years = as.integer(args[1]),
                          from  = if (length(args) >= 2) args[2] else NULL,
                          to    = if (length(args) >= 3) args[3] else NULL)
}
