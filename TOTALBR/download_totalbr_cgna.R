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
# DATES ARE YYYY-MM-DD, AND ONLY THAT. /apiv1/tatic requires YYYYMMDD and
# refuses anything else; this endpoint refuses YYYYMMDD in turn, with
# {"error":"Formato inv\u00e1lido. Utilize YYYY-MM-DD."} -- and it refuses a
# time as well. "YYYY-MM-DD HH:MM:SS" is what the TATIC endpoint accepts, NOT
# this one, so the day is the finest window that exists here. That is not a
# detail: it decides what can be done about the 502 below.
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
# THE 502, AND THE TWO THINGS THAT CAN BE DONE ABOUT IT. Asked for a day, the
# portal has answered
#
#   HTTP 502 ... The proxy server received an invalid response from an upstream
#   server. Reason: Error reading from remote server
#
# That is the CGNA's own Apache in front of the application -- not our proxy,
# not the token -- giving up on a backend that took longer than it would wait.
# Since the dates carry no time, THE DAY CANNOT BE SPLIT: there is no narrower
# question to ask. Two levers remain, and both are used, in this order:
#
#   1. ASK FOR ONE DATE, NOT TWO. datai=d dataf=d+1 is a two-day span if dataf
#      is inclusive, which doubles the work behind a front end that is already
#      timing out. datai=d dataf=d is unambiguous and cannot cost more. The
#      answer is still trimmed to the day locally, so an inclusive and an
#      exclusive dataf produce the same file either way.
#   2. RETRY, SMALLER, WITH A PAUSE. A 502 from an overloaded backend is the one
#      failure that repeating can fix, and it is now the only tool left. Each
#      attempt waits longer and asks for fewer rows per page (1000, then 250,
#      then 50): if the backend applies the page limit in the query it does less
#      work per request, and if it does not, nothing is lost but the attempt.
#
# When both fail the day is left alone and named. It is not stored short and not
# silently skipped -- a re-run picks it up, because CGNA_DAY never recorded it.
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

# The shared CGNA plumbing: the proxy, the CSV conventions, the JSON flattening.
# Sourcing only defines functions.
#
# NOT API_TATIC/download_tatic.R, which is where these helpers used to live and
# which this file used to source for them. TATIC is a different endpoint with a
# different contract and nothing to do with the national table; pulling in one
# dataset's downloader to borrow four utilities made it look as though TOTALBR
# depended on TATIC, which it does not. The one thing they genuinely share is
# TATIC_TOKEN -- the token authenticates a person against the portal, not
# against an endpoint.
source(here::here("CGNA", "cgna_common.R"))

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

# How long to wait for one request. The portal's own gateway gives up at 60
# seconds and answers 502, so a client timeout above that never fires on that
# failure -- it only decides how long a genuinely hanging request blocks the
# run. 120 is past the gateway with room to spare.
CGNA_TOTALBR_TIMEOUT <- as.integer(Sys.getenv("CGNA_TOTALBR_TIMEOUT", unset = "120"))

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
  # The envelope is {"count": N, "data": [...]}. It does NOT carry page,
  # per_page, total or total_pages -- confirmed against the live answer. `count`
  # is the rows IN THIS PAGE, not the size of the whole result, so it cannot say
  # how many pages there are and paging stops on a short page instead.
  list(rows  = .cgna_find_rows(parsed),
       count = num(parsed[["count"]]))
}

# =============================================================================
# ONE PAGE
#
# Returns a list, always, because "it failed" and "why it failed" have to travel
# together: the caller retries a 502 and must not retry a 401 at all.
#
#   ok = TRUE   rows, page, per_page, total, total_pages
#   ok = FALSE  status (HTTP code, or NA for a transport error) and
#               retryable (TRUE for 5xx, a timeout and a transport error --
#               the failures a pause and a smaller page can fix)
#
# Both dates are formatted YYYY-MM-DD. There is no time component to give: the
# endpoint refuses one, which is why there is no narrower window than a day.
# =============================================================================
cgna_totalbr_fetch_page <- function(from, to, token, page,
                                    base_url = CGNA_TOTALBR_URL,
                                    per_page = CGNA_TOTALBR_PAGE_SIZE,
                                    timeout = CGNA_TOTALBR_TIMEOUT,
                                    quiet = FALSE) {
  fmt <- function(d) format(as.Date(d), "%Y-%m-%d")

  req <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = fmt(from), dataf = fmt(to),
                         page = page, per_page = per_page) |>
    httr2::req_user_agent("BRA-ingestion/totalbr-cgna") |>
    httr2::req_timeout(timeout) |>
    # NOT req_retry: the retry that matters here changes the question (a smaller
    # page, after a longer pause) and lives in cgna_totalbr_fetch_day(), where
    # it can also tell a 502 from a 401. Retrying identically inside the page
    # fetch would only multiply the wait before that gets its turn.
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
  # The endpoint reports a refusal as a 200 with {"error": "..."} as readily as
  # with a status code. Read as an envelope that is simply short of rows, that
  # would store an empty day and never ask again.
  if (!is.null(parsed[["error"]])) {
    if (!quiet) message("      the API returned an error: ", parsed[["error"]][[1]])
    return(list(ok = FALSE, status = st, retryable = FALSE))
  }
  c(list(ok = TRUE), .cgna_totalbr_page_parts(parsed))
}

# =============================================================================
# EVERY PAGE OF ONE REQUEST
#
# list(ok = TRUE, df) or list(ok = FALSE, retryable = ...). A failed page fails
# the window: a window stored half-complete would look finished to the resume
# logic, and the missing half is invisible from then on.
# =============================================================================
cgna_totalbr_fetch_range <- function(from, to, token, base_url = CGNA_TOTALBR_URL,
                                     per_page = CGNA_TOTALBR_PAGE_SIZE,
                                     timeout = CGNA_TOTALBR_TIMEOUT,
                                     quiet = FALSE) {
  pages <- list()
  page  <- 1L
  repeat {
    pp <- cgna_totalbr_fetch_page(from, to, token, page, base_url, per_page,
                                  timeout, quiet)
    if (!isTRUE(pp$ok))
      return(list(ok = FALSE, retryable = isTRUE(pp$retryable), status = pp$status))
    rows <- pp$rows
    if (is.null(rows) || nrow(rows) == 0) break
    rows <- cgna_flatten_lists(rows)      # a nested array cannot be written to CSV
    rows[] <- lapply(rows, as.character)
    pages[[length(pages) + 1L]] <- rows

    # A SHORT PAGE IS THE ONLY END-OF-RESULT SIGNAL THIS ENVELOPE GIVES. It
    # reports `count` -- the rows in this page -- and nothing about the result as
    # a whole, so there is no total to check the download against and no page
    # count to stop on. A full page means "ask for the next one"; anything
    # shorter is the last.
    if (nrow(rows) < per_page) break
    page <- page + 1L
    if (page > 5000L) {                    # a guard, not an expectation
      message("      stopped at 5000 pages -- the pages never got shorter")
      break
    }
  }

  df <- cgna_rbind_fill(pages)
  if (is.null(df)) df <- data.frame()
  list(ok = TRUE, df = df, pages = length(pages))
}

# =============================================================================
# cgna_totalbr_check(day, span, per_page) -- ONE request, everything it answered
#
#   source(here::here("TOTALBR", "download_totalbr_cgna.R"))
#   cgna_totalbr_check("2026-01-15")                  # datai = dataf = that day
#   cgna_totalbr_check("2026-01-15", span = 1)        # dataf = the next day
#   cgna_totalbr_check("2026-01-15", per_page = 50)   # a smaller page
#
# Run this first, and whenever a download comes back empty or refused. It asks
# for a single page -- at the SAME per_page the download uses, so it tests the
# request that is actually made rather than a smaller one -- and prints the URL,
# the query with the token redacted, the
# HTTP status, the first of the raw body, and what the envelope was understood
# to contain -- the row count, the column names, the page/total fields. An empty
# day and a rejected request look identical from the outside; this is what
# separates them.
#
# `span` is days: 0 sends datai = dataf (the default, and the cheapest question
# for the portal), 1 sends dataf = the next day. It exists to measure two
# things that cannot be assumed -- whether dataf is inclusive, and whether the
# two-day span is what makes the backend time out.
# =============================================================================
cgna_totalbr_check <- function(day = Sys.Date() - 30, span = 0L,
                               base_url = CGNA_TOTALBR_URL,
                               per_page = CGNA_TOTALBR_PAGE_SIZE) {
  token <- Sys.getenv("TATIC_TOKEN", unset = "")
  if (!nzchar(token))
    stop("TATIC_TOKEN is not set. Put it in .Renviron (git-ignored) and restart R.")
  day <- as.Date(day)
  from <- day; to <- day + span
  fmt <- function(d) format(d, "%Y-%m-%d")

  message("URL      : ", base_url)
  message("Query    : token=<", nchar(token), " chars> datai=", fmt(from),
          " dataf=", fmt(to), " page=1 per_page=", per_page)

  req <- httr2::request(base_url) |>
    httr2::req_url_query(token = token, datai = fmt(from), dataf = fmt(to),
                         page = 1L, per_page = per_page) |>
    httr2::req_user_agent("BRA-ingestion/totalbr-cgna") |>
    httr2::req_timeout(CGNA_TOTALBR_TIMEOUT) |>
    httr2::req_error(is_error = function(resp) FALSE) |>   # report it, do not throw
    bra_proxy()

  t0   <- Sys.time()
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (!inherits(resp, "httr2_response")) {
    message("Transport: FAILED after ", secs, "s -- ", conditionMessage(resp))
    message("  A proxy that is not configured looks exactly like this. See proxy.R.")
    return(invisible(NULL))
  }
  st <- httr2::resp_status(resp)
  message("Status   : HTTP ", st, "  (", secs, "s)")
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  message("Body[1:400]:\n", substr(body, 1, 400))

  # A 5xx here is the CGNA's own front end, not us. Say so, and say what it
  # means, rather than leaving an HTML error page to be interpreted.
  if (st >= 500) {
    message("\nThat is the CGNA's front end giving up on its own backend, not the\n",
            "token and not our proxy. The seconds above say which: a 502 after ~30-60s\n",
            "is an upstream timeout; an immediate one is the application refusing or\n",
            "restarting.")
    message("  The day is the finest window this endpoint has -- it takes no time of\n",
            "  day -- and per_page is not a lever either: the same day answered 502\n",
            "  at 61s with per_page 5 and with 50 alike. Nothing in this client makes\n",
            "  the question smaller.\n",
            "  What DOES vary is the period. A recent day is served where an older one\n",
            "  times out, so try:\n",
            "    cgna_totalbr_check(Sys.Date() - 7)\n",
            "  If that answers and this does not, the finding is about how far back\n",
            "  the endpoint can reach inside the CGNA's own 60s gateway -- theirs to\n",
            "  fix, not ours to work around.")
    return(invisible(NULL))
  }

  if (!jsonlite::validate(body)) {
    message("\nThe answer is not JSON -- usually a login page or a proxy error page.")
    return(invisible(body))
  }
  parsed <- jsonlite::fromJSON(body, simplifyDataFrame = TRUE, flatten = TRUE)
  message("\nTop-level: ", paste(names(parsed), collapse = ", "))
  if (!is.null(parsed[["error"]])) {
    message("The API returned an error: ", parsed[["error"]][[1]])
    return(invisible(parsed))
  }
  pp <- .cgna_totalbr_page_parts(parsed)
  message("Envelope : count=", pp$count, " (rows in THIS page; the envelope says",
          " nothing about the result as a whole)")
  if (is.null(pp$rows) || nrow(pp$rows) == 0) {
    message("Rows     : none found.")
    message("  If `count` above is greater than 0, the rows are there and the\n",
            "  envelope is shaped differently than expected -- send the body printed\n",
            "  above. If it is 0 or absent, this day genuinely has no records, or\n",
            "  the token does not cover it.")
  } else {
    message("Rows     : ", nrow(pp$rows), " x ", ncol(pp$rows))
    message("Columns  : ", paste(names(pp$rows), collapse = ", "))
    message("\nFirst row:")
    print(utils::head(as.data.frame(lapply(pp$rows, as.character)), 1))
  }
  invisible(pp)
}

# =============================================================================
# ONE WHOLE DAY -- one date, and a retry that only waits
#
# data.frame (possibly 0 rows) on success, NULL on failure. The difference
# matters: 0 rows is an answer ("no flights that day"), NULL must be retried on
# a later run.
#
# THE RETRY DOES NOT ASK FOR LESS, BECAUSE THERE IS NO LESS TO ASK FOR. Measured
# against the portal: the same day came back 502 at 61 seconds at per_page=5 and
# at per_page=50 alike -- a fixed 60-second gateway timeout in front of a backend
# that assembles the result before paging touches it. The page size is not a
# lever on that failure, so every attempt asks at the ordinary page size; a
# smaller one would only be a slower way to make the same request.
#
# What is left is time, for a backend that is momentarily loaded rather than
# permanently too slow:
#
#   attempt 1   immediately
#   attempt 2   after 20s
#   attempt 3   after 60s
#
# A non-retryable failure (401, a bad parameter) stops at once: waiting does not
# fix a token. A day that fails every attempt is left alone and named -- never
# stored short, never silently skipped. CGNA_DAY did not record it, so the next
# run asks for it again.
#
# If every day of a month fails all three attempts while a recent day is served,
# this is not a retry problem: the endpoint cannot assemble that period inside
# its own gateway's patience, which is the CGNA's to fix.
# =============================================================================
CGNA_TOTALBR_WAITS <- c(0, 20, 60)    # seconds before each attempt

cgna_totalbr_fetch_day <- function(day, token, base_url = CGNA_TOTALBR_URL,
                                   per_page = CGNA_TOTALBR_PAGE_SIZE,
                                   timeout = CGNA_TOTALBR_TIMEOUT,
                                   waits = CGNA_TOTALBR_WAITS) {
  day <- as.Date(day)
  for (w in waits) {
    if (w > 0) {
      message(sprintf("      waiting %ds before asking again ...", w))
      Sys.sleep(w)
    }
    # ONE date, not a span: datai=d dataf=d+1 is two days if dataf is inclusive,
    # and doubling the work behind a front end that is already timing out is the
    # opposite of what is wanted. The trim below makes both readings equivalent.
    res <- cgna_totalbr_fetch_range(day, day, token, base_url, per_page, timeout)
    if (isTRUE(res$ok)) return(.cgna_totalbr_finish(res$df, day))
    if (!isTRUE(res$retryable)) return(NULL)
  }
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

# which days does a month part already hold?
cgna_totalbr_days_in_part <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(character(0))
  d <- tryCatch(
    data.table::fread(file = path, sep = CGNA_SEP, select = "CGNA_DAY",
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
        old  <- cgna_read_csv(part_csv)
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
          part <- cgna_rbind_fill(c(list(keep), fetched))
          if (!is.null(part)) {
            part <- part[order(part$CGNA_DAY), , drop = FALSE]
            cgna_write_csv(part, part_csv)
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
    combined <- cgna_rbind_fill(lapply(sort(part_files), cgna_read_csv))
    if (is.null(combined)) {
      message(sprintf("Year %d: months are empty; nothing written.", yr)); next
    }
    # the placeholder rows for empty days are storage bookkeeping, not data
    real <- combined[rowSums(!is.na(combined[setdiff(names(combined), "CGNA_DAY")])) > 0,
                     , drop = FALSE]
    real <- real[order(real$CGNA_DAY), , drop = FALSE]

    out_csv <- file.path(out_dir, sprintf("totalbr_%dcgna.csv", yr))
    cgna_write_csv(real, out_csv)
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
