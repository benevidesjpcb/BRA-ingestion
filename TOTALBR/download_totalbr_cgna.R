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
# for the national flight table, documented at
# https://portal.cgna.decea.mil.br/apiv1/apidocs/#/Indicadores. It takes token,
# datai, dataf, page and per_page, and the dates are YYYY-MM-DD. It is simply
# the URL; CGNA_TOTALBR_URL overrides it if the path is ever renamed.
#
# There is deliberately NO endpoint probing. Trying a list of candidate paths
# and taking the first that returns rows sounds defensive and is the opposite:
# every reason a request can fail -- an expired token, a proxy that is not
# configured, a date the endpoint rejects, an envelope shaped differently than
# expected -- collapses into the same "no rows", for five URLs in a row, and the
# one thing the API actually said is thrown away. A single URL that fails
# loudly, printing the status and the body, says what is wrong on the first
# attempt. cgna_totalbr_check() below is that request, on its own.
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

CGNA_TOTALBR_URL <- Sys.getenv(
  "CGNA_TOTALBR_URL",
  unset = "https://portal.cgna.decea.mil.br/apiv1/voossisceab")

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
# The rows are found by SHAPE rather than by a hardcoded name: the name of the
# array is the only part of the contract not visible in the envelope itself, and
# it differs between endpoints of this API. The search is RECURSIVE -- a payload
# that nests the rows one level down (data$items, say) is the difference between
# a working download and a year that reads as empty, and it is not worth a
# second round trip to find out which shape this endpoint uses.
.cgna_find_rows <- function(x, depth = 0L) {
  if (depth > 4L) return(NULL)
  if (is.data.frame(x)) return(if (nrow(x) > 0 || ncol(x) > 0) x else NULL)
  if (!is.list(x) || length(x) == 0) return(NULL)
  # a bare list of records (jsonlite did not simplify it) is the rows too
  if (is.null(names(x)) && all(vapply(x, is.list, logical(1))))
    return(tryCatch(do.call(rbind, lapply(x, as.data.frame)), error = function(e) NULL))
  for (el in x) {
    got <- .cgna_find_rows(el, depth + 1L)
    if (!is.null(got)) return(got)
  }
  NULL
}

.cgna_totalbr_page_parts <- function(parsed) {
  num <- function(x) if (is.null(x)) NA_integer_ else suppressWarnings(as.integer(x[[1]]))
  list(rows        = .cgna_find_rows(parsed),
       page        = num(parsed[["page"]]),
       per_page    = num(parsed[["per_page"]]),
       total       = num(parsed[["total"]]),
       total_pages = num(parsed[["total_pages"]]))
}

# =============================================================================
# ONE PAGE OF ONE WINDOW
#
# The window, not the day, is the unit of a request. The endpoint accepts
# "YYYY-MM-DD HH:MM:SS" as well as a bare date -- which is what makes it
# possible to ask for an hour, and that turns out to matter: the portal answers
# a whole day of the national table with
#
#   HTTP 502 ... The proxy server received an invalid response from an upstream
#   server. Reason: Error reading from remote server
#
# That is the CGNA's own Apache in front of the application, not our proxy and
# not the token: the backend takes longer to assemble the answer than the front
# end will wait. It happens with per_page=5 as readily as with 1000, because the
# cost is in building the result set, before paging touches it. A narrower
# window is the only thing that reduces it, so a 502 is not a failure to report
# and give up on -- it is the signal to ask for less at a time. See
# cgna_totalbr_fetch_day().
#
# Returns a list, always, because "it failed" and "why it failed" have to travel
# together: the day-splitter retries a 502 by halving the window and must not
# retry a 401 at all.
#
#   ok = TRUE   rows, page, per_page, total, total_pages
#   ok = FALSE  status (HTTP code, or NA for a transport error) and
#               retryable (TRUE for 5xx, a timeout and a transport error --
#               the failures a smaller window can fix)
# =============================================================================
CGNA_TOTALBR_DT_FMT <- "%Y-%m-%d %H:%M:%S"

cgna_totalbr_fetch_page <- function(from, to, token, page,
                                    base_url = CGNA_TOTALBR_URL,
                                    per_page = CGNA_TOTALBR_PAGE_SIZE,
                                    timeout = 300, quiet = FALSE) {
  # A whole day is sent as two bare dates, the form the endpoint documents and
  # the one that has worked elsewhere in this API family; anything narrower
  # carries the time. Sending "00:00:00" for a day would change the request that
  # is made in the ordinary case for no reason.
  fmt <- function(x) if (inherits(x, "Date")) format(x, "%Y-%m-%d")
                     else format(as.POSIXct(x, tz = "UTC"), CGNA_TOTALBR_DT_FMT, tz = "UTC")

  req <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = fmt(from), dataf = fmt(to),
                         page = page, per_page = per_page) |>
    httr2::req_user_agent("BRA-ingestion/totalbr-cgna") |>
    httr2::req_timeout(timeout) |>
    # NOT req_retry: a 502 from an overloaded upstream is not fixed by asking
    # the same question again four times, it is fixed by asking a smaller one.
    # Retrying here only multiplies the wait before the splitter gets its turn.
    httr2::req_error(is_error = function(resp) FALSE) |>
    bra_proxy()

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (!inherits(resp, "httr2_response")) {
    if (!quiet) message("      transport error: ", conditionMessage(resp))
    return(list(ok = FALSE, status = NA_integer_, retryable = TRUE))
  }
  st <- httr2::resp_status(resp)
  if (st != 200) {
    if (!quiet) {
      why <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      # an HTML error page says nothing useful line by line; its title does
      if (grepl("<html", why, ignore.case = TRUE)) {
        ttl <- regmatches(why, regexpr("(?i)<title>[^<]*</title>", why, perl = TRUE))
        why <- if (length(ttl)) gsub("(?i)</?title>", "", ttl, perl = TRUE) else "HTML error page"
      }
      message(sprintf("      HTTP %d%s", st, if (nzchar(why)) paste0(": ", substr(why, 1, 200)) else ""))
    }
    return(list(ok = FALSE, status = st, retryable = st >= 500 || st == 408 || st == 429))
  }

  body <- httr2::resp_body_string(resp)
  if (!jsonlite::validate(body)) {
    if (!quiet) message("      the answer is not JSON: ", substr(body, 1, 200))
    return(list(ok = FALSE, status = st, retryable = FALSE))
  }
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyDataFrame = TRUE, flatten = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed)) {
    if (!quiet) message("      JSON that could not be parsed into a table.")
    return(list(ok = FALSE, status = st, retryable = FALSE))
  }
  c(list(ok = TRUE), .cgna_totalbr_page_parts(parsed))
}

# =============================================================================
# EVERY PAGE OF ONE WINDOW
#
# list(ok = TRUE, df) or list(ok = FALSE, retryable = ...). A failed page fails
# the window: a window stored half-complete would look finished to the resume
# logic, and the missing half is invisible from then on.
# =============================================================================
cgna_totalbr_fetch_range <- function(from, to, token, base_url = CGNA_TOTALBR_URL,
                                     per_page = CGNA_TOTALBR_PAGE_SIZE,
                                     timeout = 300, quiet = FALSE) {
  pages   <- list()
  page    <- 1L
  total   <- NA_integer_
  n_pages <- NA_integer_
  repeat {
    pp <- cgna_totalbr_fetch_page(from, to, token, page, base_url, per_page,
                                  timeout, quiet)
    if (!isTRUE(pp$ok))
      return(list(ok = FALSE, retryable = isTRUE(pp$retryable), status = pp$status))
    if (page == 1L) { total <- pp$total; n_pages <- pp$total_pages }
    rows <- pp$rows
    if (is.null(rows) || nrow(rows) == 0) break
    rows <- tatic_flatten_lists(rows)      # a nested array cannot be written to CSV
    rows[] <- lapply(rows, as.character)
    pages[[length(pages) + 1L]] <- rows

    # Stop on what the envelope says when it says it, and on a short page when
    # it does not: an API that stops reporting total_pages must not turn into an
    # endless loop, and one that reports it must not be probed for a page past
    # the end on every single window.
    if (!is.na(n_pages) && page >= n_pages) break
    if (is.na(n_pages) && nrow(rows) < per_page) break
    page <- page + 1L
    if (page > 5000L) {                    # a guard, not an expectation
      message("      stopped at 5000 pages -- the envelope never ended")
      break
    }
  }

  df <- tatic_rbind_fill(pages)
  if (is.null(df)) df <- data.frame()
  # The API told us how many rows the window has. Checking costs nothing and is
  # the difference between a short answer that is noticed and one that is not.
  if (!is.na(total) && nrow(df) != total && !quiet)
    message(sprintf("      WARNING: %d row(s) fetched, API reported total=%d",
                    nrow(df), total))
  list(ok = TRUE, df = df, total = total)
}

# =============================================================================
# cgna_totalbr_check(day, hours) -- ONE request, everything it answered
#
#   source(here::here("TOTALBR", "download_totalbr_cgna.R"))
#   cgna_totalbr_check("2026-01-15")             # the whole day
#   cgna_totalbr_check("2026-01-15", hours = 1)  # 00:00 -> 01:00 only
#
# `hours` is the diagnostic for a 502: if the day is refused and an hour is
# served, the portal's front end is timing out on its own backend and the
# download's window-narrowing will get through. If an hour is refused too, the
# endpoint is down and no amount of narrowing helps.
#
# Run this first, and whenever a download comes back empty. It asks for a single
# page of one day and prints the URL (with the token redacted), the HTTP status,
# the first of the raw body, and what the envelope was understood to contain --
# the row count, the column names, and the page/total fields. An empty year and
# a rejected request look identical from the outside; this is what separates
# them, and it is why nothing here guesses at a URL.
# =============================================================================
cgna_totalbr_check <- function(day = Sys.Date() - 30, hours = NULL,
                               base_url = CGNA_TOTALBR_URL, per_page = 5L) {
  token <- Sys.getenv("TATIC_TOKEN", unset = "")
  if (!nzchar(token))
    stop("TATIC_TOKEN is not set. Put it in .Renviron (git-ignored) and restart R.")
  day <- as.Date(day)
  if (is.null(hours)) {
    from <- day; to <- day + 1
    fmt  <- function(d) format(d, "%Y-%m-%d")
  } else {
    from <- as.POSIXct(paste0(format(day), " 00:00:00"), tz = "UTC")
    to   <- from + hours * 3600
    fmt  <- function(d) format(d, CGNA_TOTALBR_DT_FMT, tz = "UTC")
  }

  message("URL      : ", base_url)
  message("Query    : token=<", nchar(token), " chars> datai=", fmt(from),
          " dataf=", fmt(to), " page=1 per_page=", per_page)

  req <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = fmt(from), dataf = fmt(to),
                         page = 1L, per_page = per_page) |>
    httr2::req_user_agent("BRA-ingestion/totalbr-cgna") |>
    httr2::req_timeout(120) |>
    httr2::req_error(is_error = function(resp) FALSE) |>   # report it, do not throw
    bra_proxy()

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (!inherits(resp, "httr2_response")) {
    message("Transport: FAILED -- ", conditionMessage(resp))
    message("  A proxy that is not configured looks exactly like this. See proxy.R.")
    return(invisible(NULL))
  }
  st <- httr2::resp_status(resp)
  message("Status   : HTTP ", st)
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  message("Body[1:400]:\n", substr(body, 1, 400))

  # A 5xx here is the CGNA's own front end, not us. Say so, and say what to try,
  # rather than leaving an HTML error page to be interpreted.
  if (st >= 500) {
    message("\nThat is the CGNA's front end giving up on its own backend, not the\n",
            "token and not our proxy. The usual cause is the window: the national\n",
            "table for a whole day takes longer to assemble than the front end\n",
            "waits, and per_page does not help because the cost is in building the\n",
            "result set before paging touches it.")
    if (is.null(hours))
      message("  Try one hour:  cgna_totalbr_check(\"", format(day), "\", hours = 1)\n",
              "  If that is served, download_totalbr_cgna() will get through on its\n",
              "  own: it narrows a refused day to 6-hour, then 1-hour, then 15-minute\n",
              "  windows before giving up.")
    else
      message("  An hour was refused too, so narrowing will not help. The endpoint is\n",
              "  down or the day is beyond what it can serve; try another day, and\n",
              "  report it to the CGNA if a recent day fails the same way.")
    return(invisible(NULL))
  }

  if (!jsonlite::validate(body)) {
    message("\nThe answer is not JSON -- usually a login page or a proxy error page.")
    return(invisible(body))
  }
  parsed <- jsonlite::fromJSON(body, simplifyDataFrame = TRUE, flatten = TRUE)
  message("\nTop-level: ", paste(names(parsed), collapse = ", "))
  pp <- .cgna_totalbr_page_parts(parsed)
  message("Envelope : page=", pp$page, " per_page=", pp$per_page,
          " total=", pp$total, " total_pages=", pp$total_pages)
  if (is.null(pp$rows) || nrow(pp$rows) == 0) {
    message("Rows     : none found.")
    message("  If `total` above is a number greater than 0, the rows are there and\n",
            "  the envelope is shaped differently than expected -- send the body\n",
            "  printed above. If `total` is 0 or absent, this day genuinely has no\n",
            "  records, or the token does not cover it.")
  } else {
    message("Rows     : ", nrow(pp$rows), " x ", ncol(pp$rows))
    message("Columns  : ", paste(names(pp$rows), collapse = ", "))
    message("\nFirst row:")
    print(utils::head(as.data.frame(lapply(pp$rows, as.character)), 1))
  }
  invisible(pp)
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

# =============================================================================
# ONE WHOLE DAY -- narrowing the window until the portal can answer
#
# data.frame (possibly 0 rows) on success, NULL on failure. The difference
# matters: 0 rows is an answer ("no flights that day"), NULL must be retried.
#
# The day is asked for as one request first, because when that works it is one
# request instead of twenty-four. When it comes back 502 -- the CGNA front end
# giving up on its own backend -- the SAME day is asked for in smaller windows:
# 6 hours, then 1 hour, then 15 minutes. Each step is a different question, not
# the same one repeated, which is why this succeeds where a retry loop cannot.
#
# The steps stop at 15 minutes on purpose. A day the portal cannot serve in
# quarter-hour slices is not a window problem, and grinding on to the minute
# would turn one bad day into 1440 requests and an afternoon.
#
# The windows tile the day back to back, [t, t+step), so no flight is fetched
# twice and none falls between two of them. The answer is still trimmed to the
# day on dt_dia afterwards, exactly as for a single-request day.
# =============================================================================
CGNA_TOTALBR_STEPS <- c(24, 6, 1, 0.25)    # hours per request, in order

cgna_totalbr_fetch_day <- function(day, token, base_url = CGNA_TOTALBR_URL,
                                   per_page = CGNA_TOTALBR_PAGE_SIZE,
                                   timeout = 300,
                                   steps = CGNA_TOTALBR_STEPS) {
  day <- as.Date(day)
  t0  <- as.POSIXct(paste0(format(day), " 00:00:00"), tz = "UTC")

  for (k in seq_along(steps)) {
    step <- steps[k]
    if (step >= 24) {
      # the ordinary case: the whole day, as two bare dates
      res <- cgna_totalbr_fetch_range(day, day + 1, token, base_url, per_page, timeout)
      if (isTRUE(res$ok)) return(.cgna_totalbr_finish(res$df, day))
      if (!isTRUE(res$retryable)) return(NULL)   # a 401 is not fixed by asking less
      message(sprintf("      the whole day was refused (HTTP %s); retrying in %g-hour windows",
                      as.character(res$status), steps[k + 1L]))
      next
    }

    edges <- t0 + seq(0, 24 * 3600, by = step * 3600)
    parts <- list()
    ok    <- TRUE
    for (i in seq_len(length(edges) - 1L)) {
      res <- cgna_totalbr_fetch_range(edges[i], edges[i + 1L], token, base_url,
                                      per_page, timeout)
      if (!isTRUE(res$ok)) {
        if (!isTRUE(res$retryable)) return(NULL)
        ok <- FALSE
        if (k < length(steps))
          message(sprintf("      %s refused at %g-hour windows; narrowing to %g",
                          format(edges[i], "%H:%M"), step, steps[k + 1L]))
        break
      }
      if (nrow(res$df) > 0) parts[[length(parts) + 1L]] <- res$df
    }
    if (ok) {
      df <- tatic_rbind_fill(parts)
      if (is.null(df)) df <- data.frame()
      message(sprintf("      (assembled from %d window(s) of %g hour(s))",
                      length(edges) - 1L, step))
      return(.cgna_totalbr_finish(df, day))
    }
  }

  message("      still refused at the narrowest window; the portal cannot serve this day now")
  NULL
}

# trim to the day, then stamp the day we asked for
.cgna_totalbr_finish <- function(df, day) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
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
                                  base_url = CGNA_TOTALBR_URL) {

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

  message("Endpoint: ", base_url)

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
