#!/usr/bin/env Rscript
# =============================================================================
# download_totalbr_cgna.R
#
# The national movement table AS THE CGNA SERVES IT -- the same `total_brasil`
# the ODIN API serves, from the other side -- into one file per year, under a
# name that can never be confused with the ODIN one:
#
#   data-raw/totalbr/totalbr_<year>cgna.csv                     the year
#   data-raw/totalbr/parts/totalbr_<year>cgna_<YYYY-MM>.csv     one file per MONTH
#
#   source(here::here("TOTALBR", "download_totalbr_cgna.R"))
#   download_totalbr_cgna(2026, month = 1)                 # ONE MONTH -- start here
#   download_totalbr_cgna(2026, from = "20260101", to = "20260131")   # the same
#   download_totalbr_cgna(2026)                            # the whole year
#
# or as a script:
#
#   Rscript TOTALBR/download_totalbr_cgna.R 2026 20260101 20260131
#
# ---------------------------------------------------------------------------
# THIS IS NOT TATIC. TATIC (API_TATIC/) is the CGNA's milestone feed, a
# different table with a different meaning. This endpoint is `total_brasil`
# itself, on the same host and with the same token:
#   https://portal.cgna.decea.mil.br/apiv1/<table>?token=...&datai=&dataf=
#
# WHICH <table>. `voossisceab` -- "Consulta Voos SISCEAB", the CGNA's own name
# for the national flight table. It takes token, datai, dataf, page and
# per_page (default 1000), and the dates are YYYY-MM-DD. The path is documented
# at https://portal.cgna.decea.mil.br/apiv1/apidocs/#/Indicadores. It is the
# default here; CGNA_TOTALBR_URL overrides it, and if that default ever stops
# answering the candidates below are probed in turn so a renamed path is
# reported rather than read as an empty year.
#
# NOT THE SAME DATE FORMAT AS TATIC. This API family answers
# "Formato de data invalido. Utilize YYYY-MM-DD ou YYYY-MM-DD HH:MM:SS" to the
# YYYYMMDD that /apiv1/tatic requires, so the dates are sent as YYYY-MM-DD.
#
# ONE DAY PER CALL, AND THE DAY IS CUT LOCALLY. The window is walked day by
# day, as with TATIC and the CGNA taxi download: this API family has answered a
# wide window with only its first day. Whether `dataf` is inclusive is not
# documented and not worth guessing -- asked as [d, d+1] an exclusive bound
# gives the day and an inclusive one gives two, which would file the same
# movement under two days. So the wide bound is asked for and the answer is
# TRIMMED to the day, on the table's own date column (dt_dia, falling back to
# dh_inicio). The API's own semantics then cannot produce a duplicate or a hole
# either way.
#
# The day asked for is recorded in an added column, CGNA_DAY, rather than
# inferred from the record -- the source's stamps describe the FLIGHT, and a
# flight can be reported on a day other than the one it is filed under. That
# column is what makes a re-run resumable at the day: an interrupt costs one
# day, never a month.
#
# PER_PAGE IS CAPPED AT 1000, SO A DAY IS ALWAYS SEVERAL PAGES. The national
# table runs to thousands of flights a day, and per_page cannot be raised past
# 1000 -- so paging is not an optimisation here, it is the only way to get a
# whole day. Its default is 1, which means an omitted per_page fetches a day one
# row at a time; it is always sent.
#
# PAGINATED, AND THE ENVELOPE SAYS SO. The answer is not a bare array but an
# object: the rows, plus `page`, `per_page`, `total` and `total_pages`. Keeping
# only the first page silently caps every day at per_page -- on the taxi
# endpoint a whole year came back the same size as a busy month before this was
# handled. Every page is fetched, and the rows are counted against the `total`
# the API itself reported: a day that does not add up is named rather than
# quietly stored short.
#
# NOTHING IS FILTERED BY AIRPORT, for the reason totalbr_sources.R gives: this
# is the national table and it is kept whole. Restricting it is a downstream
# decision.
#
# The token is read from the environment, never hardcoded -- the same
# TATIC_TOKEN in .Renviron (git-ignored) that download_tatic() uses.
# =============================================================================

# The CGNA day-walk helpers (the fetch conventions, the CSV read/write, the
# proxy) live with the other CGNA downloaders; sourcing only defines functions.
source(here::here("API_TATIC", "download_tatic.R"))

# Paths tried, in order, when CGNA_TOTALBR_URL is not set. `dstaxi` is served as
# the ODIN name with the underscores dropped, so that spelling leads.
CGNA_TOTALBR_CANDIDATES <- c("voossisceab", "totalbrasil", "total_brasil",
                             "totalbr", "total-brasil")

CGNA_TOTALBR_BASE <- Sys.getenv("CGNA_BASE_URL",
                                unset = "https://portal.cgna.decea.mil.br/apiv1")

# Rows per page. The endpoint documents per_page as optional with a default of
# 1 and a MAXIMUM OF 1000, so 1000 is both the setting and the ceiling: asking
# for more is the kind of parameter an API answers by silently falling back to
# its default, which would fetch a busy day one row at a time. Anything above
# 1000 is clamped, out loud.
CGNA_TOTALBR_PAGE_MAX <- 1000L
CGNA_TOTALBR_PAGE_SIZE <- local({
  n <- suppressWarnings(as.integer(Sys.getenv("CGNA_TOTALBR_PAGE_SIZE",
                                              unset = "1000")))
  if (is.na(n) || n < 1L) n <- CGNA_TOTALBR_PAGE_MAX
  if (n > CGNA_TOTALBR_PAGE_MAX) {
    message("CGNA_TOTALBR_PAGE_SIZE=", n, " exceeds the endpoint maximum; using ",
            CGNA_TOTALBR_PAGE_MAX, ".")
    n <- CGNA_TOTALBR_PAGE_MAX
  }
  n
})

# The column the trim is done on, in order of preference. dt_dia is what the
# ODIN download anchors its month windows on; dh_inicio is the observed start,
# kept as a fallback for the day the envelope drops dt_dia.
CGNA_TOTALBR_DATE_COLS <- c("dt_dia", "dhinicio", "dh_inicio")

# =============================================================================
# the paginated envelope
# =============================================================================
# The rows are found by SHAPE (the one data.frame in the object) rather than by
# a hardcoded name: that name is the only part of the contract not visible in
# the envelope itself, and it differs between endpoints of this API.
.cgna_totalbr_page_parts <- function(parsed) {
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
# NULL on failure, the parts of the page on success. `quiet` exists for the
# endpoint resolution below, where a failure is an expected answer ("not this
# path") rather than something to report.
cgna_totalbr_fetch_page <- function(day, token, page, base_url,
                                    per_page = CGNA_TOTALBR_PAGE_SIZE,
                                    timeout = 300, quiet = FALSE) {
  fmt <- function(d) format(as.Date(d), "%Y-%m-%d")   # NOT the YYYYMMDD TATIC wants
  req <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = fmt(day), dataf = fmt(day + 1),
                         page = page, per_page = per_page) |>
    httr2::req_user_agent("BRA-ingestion/totalbr-cgna") |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = if (quiet) 1 else 4) |>
    bra_proxy()

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (!inherits(resp, "httr2_response")) {
    if (!quiet) message("      transport error: ", conditionMessage(resp))
    return(NULL)
  }
  # The API explains its refusals in the body (a bad date format, an expired
  # token). Swallowing that leaves "FAILED" and a trip to the API docs to find
  # out what it already said, so it is shown.
  if (httr2::resp_status(resp) != 200) {
    if (!quiet) {
      why <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      message(sprintf("      HTTP %d%s", httr2::resp_status(resp),
                      if (nzchar(why)) paste0(": ", substr(why, 1, 300)) else ""))
    }
    return(NULL)
  }

  body <- httr2::resp_body_string(resp)
  if (!jsonlite::validate(body)) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyDataFrame = TRUE, flatten = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  .cgna_totalbr_page_parts(parsed)
}

# =============================================================================
# cgna_totalbr_url(day) -- which path serves this table
#
# Returns the endpoint URL, or stops with what was tried. CGNA_TOTALBR_URL wins
# outright; otherwise the documented path (voossisceab) is tried first and the
# rest only if it does not answer, so the normal run costs one small request.
# =============================================================================
cgna_totalbr_url <- function(token, day = Sys.Date() - 30,
                             candidates = CGNA_TOTALBR_CANDIDATES,
                             base = CGNA_TOTALBR_BASE) {
  fixed <- Sys.getenv("CGNA_TOTALBR_URL", unset = "")
  if (nzchar(fixed)) return(fixed)

  message("Resolving the CGNA endpoint for the national table (probing ",
          format(as.Date(day)), ") ...")
  for (cand in candidates) {
    url <- paste0(sub("/+$", "", base), "/", cand)
    pp  <- cgna_totalbr_fetch_page(day, token, page = 1L, base_url = url,
                                   per_page = 1L, timeout = 120, quiet = TRUE)
    if (!is.null(pp) && !is.null(pp$rows) && nrow(pp$rows) > 0) {
      message("  -> ", url)
      return(url)
    }
    message("  ", url, ": no rows")
  }
  stop("None of these CGNA paths returned rows:\n  ",
       paste(paste0(sub("/+$", "", base), "/", candidates), collapse = "\n  "),
       "\nSet CGNA_TOTALBR_URL in .Renviron to the correct one, e.g.\n",
       "  CGNA_TOTALBR_URL=", sub("/+$", "", base), "/totalbrasil\n",
       "A probe day with no traffic, an expired TATIC_TOKEN or a proxy that is ",
       "not configured produce the same symptom -- check those before ",
       "concluding the path is wrong.")
}

# ---- keep only the day we asked for -----------------------------------------
# Returns the rows whose date column falls on `day`. A row with no usable stamp
# is KEPT: dropping it would silently lose a flight over a parsing question, and
# the CGNA_DAY column still records which request it arrived in.
cgna_totalbr_trim_day <- function(df, day, date_cols = CGNA_TOTALBR_DATE_COLS) {
  if (is.null(df) || nrow(df) == 0) return(df)
  col <- intersect(date_cols, names(df))
  if (length(col) == 0) return(df)
  d <- substr(trimws(df[[col[1]]]), 1, 10)
  keep <- is.na(d) | !nzchar(d) | d == format(as.Date(day))
  if (!all(keep))
    message(sprintf("      (%d row(s) outside %s dropped, on %s)",
                    sum(!keep), format(as.Date(day)), col[1]))
  df[keep, , drop = FALSE]
}

# ---- one whole day, every page ----------------------------------------------
# data.frame (possibly 0 rows) on success, NULL on failure. The difference
# matters: 0 rows is an answer ("no flights that day"), NULL must be retried.
cgna_totalbr_fetch_day <- function(day, token, base_url,
                                   per_page = CGNA_TOTALBR_PAGE_SIZE,
                                   timeout = 300) {
  pages   <- list()
  page    <- 1L
  total   <- NA_integer_
  n_pages <- NA_integer_
  repeat {
    pp <- cgna_totalbr_fetch_page(day, token, page, base_url, per_page, timeout)
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
    if (page > 5000L) {                    # a guard, not an expectation: the
                                           # national table is ~10x dstaxi
      message("      stopped at 5000 pages -- the envelope never ended")
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
  df <- cgna_totalbr_trim_day(df, day)
  if (nrow(df) == 0) return(data.frame())
  df$CGNA_DAY <- format(as.Date(day))   # the day WE asked for
  df
}

# which days does a month part already hold?
cgna_totalbr_days_in_part <- function(path) {
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
# download_totalbr_cgna(years, month, from, to, out_dir, force)
#
#   years   : years to build, e.g. 2026 or 2025:2026 (default: the study years)
#   month   : 1..12 -- the shorthand for "just this month of that year". It is
#             the same thing as from/to on the month's first and last day, and
#             it is how this should be run the first time: one month says
#             whether the source is what we think it is, at a hundredth of the
#             cost of a year.
#   from/to : optional "YYYYMMDD" bounds INSIDE those years, for a partial run
#   out_dir : default data-raw/totalbr -- beside the ODIN files
#   force   : TRUE re-fetches days already stored
#
# Returns, invisibly, the year files written.
# =============================================================================
download_totalbr_cgna <- function(years    = totalbr_cgna_default_years(),
                                  month    = NULL,
                                  from     = NULL,
                                  to       = NULL,
                                  out_dir  = here::here("data-raw", "totalbr"),
                                  force    = FALSE,
                                  base_url = NULL) {

  token <- Sys.getenv("TATIC_TOKEN", unset = "")
  if (!nzchar(token))
    stop("TATIC_TOKEN is not set. Put it in .Renviron (git-ignored):\n",
         "  TATIC_TOKEN=your-token\n",
         "and restart R. Never write the token into a script.")

  years <- suppressWarnings(as.integer(years))
  if (length(years) == 0 || any(is.na(years)))
    stop("Years must be 4-digit numbers, e.g. 2026 or 2025:2026.")

  # `month` is from/to on that month, and saying both is a contradiction rather
  # than something to resolve silently.
  if (!is.null(month)) {
    if (!is.null(from) || !is.null(to))
      stop("Give `month` or `from`/`to`, not both.")
    if (length(years) != 1L)
      stop("`month` needs exactly one year, e.g. download_totalbr_cgna(2026, month = 1).")
    m1 <- as.Date(sprintf("%d-%02d-01", years, as.integer(month)))
    from <- format(m1, "%Y%m%d")
    to   <- format(seq(m1, by = "month", length.out = 2)[2] - 1, "%Y%m%d")
  }

  parts_dir <- file.path(out_dir, "parts")
  for (d in c(out_dir, parts_dir))
    if (!dir.exists(d)) { dir.create(d, recursive = TRUE); message("Created ", d) }

  as_ymd <- function(s) if (is.null(s)) NULL else as.Date(as.character(s), "%Y%m%d")
  lo <- as_ymd(from); hi <- as_ymd(to)
  today   <- Sys.Date()
  written <- character(0)

  # Resolved once, on a day inside the window actually being asked for: probing
  # a day the source has no traffic for would reject every candidate path.
  if (is.null(base_url)) {
    probe <- if (!is.null(lo)) lo else min(as.Date(sprintf("%d-01-15", min(years))), today - 1)
    base_url <- cgna_totalbr_url(token, day = min(probe, today - 1))
  }

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
      part_csv <- file.path(parts_dir, sprintf("totalbr_%dcgna_%s.csv", yr, ym))
      m_first  <- as.Date(paste0(ym, "-01"))
      m_last   <- min(seq(m_first, by = "month", length.out = 2)[2] - 1, year_end)
      m_first  <- max(m_first, year_start)
      want     <- format(seq(m_first, m_last, by = "day"))

      have <- if (force) character(0) else cgna_totalbr_days_in_part(part_csv)
      # today is always refetched: it can still receive flights
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
        df  <- cgna_totalbr_fetch_day(day, token, base_url)
        if (is.null(df)) {
          message(sprintf("    %s  FAILED (retried; will be picked up next run)", need[i]))
          failed <- c(failed, need[i])
          next
        }
        if (nrow(df) == 0) {
          # A day with no flights is an answer, not a failure. Recorded as one
          # row carrying only the day, so the resume logic knows it was asked
          # for and does not request it again on every run.
          df <- data.frame(CGNA_DAY = need[i], stringsAsFactors = FALSE)
          message(sprintf("    %s  no records", need[i]))
        } else {
          message(sprintf("    %s  %d record(s)", need[i], nrow(df)))
        }
        fetched[[length(fetched) + 1L]] <- df

        # persist as we go: an interrupt costs one day, not the month
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
                             pattern = sprintf("^totalbr_%dcgna_%d-[0-9]{2}\\.csv$", yr, yr),
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

    out_csv <- file.path(out_dir, sprintf("totalbr_%dcgna.csv", yr))
    tatic_write_csv(real, out_csv)
    written <- c(written, out_csv)
    message(sprintf("Year %d: merged %d month(s) -> %d record(s), %d column(s) -> %s",
                    yr, length(part_files), nrow(real), ncol(real), out_csv))

    # ---- the gaps, over the RANGE THAT WAS ASKED FOR -------------------------
    # Scoped to the window of this run: reporting 335 missing days after a
    # one-month request is noise, and noise is how a real hole goes unnoticed.
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

# The study period comes from _chapter-setup.R (totalbr_data_years) when that
# has been sourced; only a session that never loaded it falls back to the
# current year.
totalbr_cgna_default_years <- function() {
  if (exists("totalbr_data_years", inherits = TRUE))
    get("totalbr_data_years", inherits = TRUE)
  else as.integer(format(Sys.Date(), "%Y"))
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) download_totalbr_cgna()
  else download_totalbr_cgna(years = as.integer(args[1]),
                             from  = if (length(args) >= 2) args[2] else NULL,
                             to    = if (length(args) >= 3) args[3] else NULL)
}
