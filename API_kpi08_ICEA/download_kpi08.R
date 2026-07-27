#!/usr/bin/env Rscript
# =============================================================================
# download_kpi08.R
#
# Downloads KPI08 (ASMA) data from the ICEA/DECEA ODIN API into ONE FILE PER
# YEAR, incrementally. Re-running only fetches what is missing and merges it
# into the existing year file, so you can run it daily/monthly to keep the
# current year up to date.
#
# Two ways to use it:
#
#   1) From a terminal
#        Rscript API_kpi08_ICEA/download_kpi08.R                 # current year
#        Rscript API_kpi08_ICEA/download_kpi08.R 2026            # one year
#        Rscript API_kpi08_ICEA/download_kpi08.R 2023 2024 2025  # several years
#
#   2) From R / a Quarto chunk — source it and call the function
#        source(here::here("API_kpi08_ICEA", "download_kpi08.R"))
#        download_kpi08(2023:2026, out_dir = here::here("data-raw", "kpi08"))
#
#      Sourcing only defines the function; nothing is downloaded until you call it.
#
# Output: <out_dir>/kpi08_<year>.csv  (semicolon-delimited, like the annual
#         archive; the folder is created if missing)
#
# ODIN is a PostgREST API. Query shape: <base>?<column>=<op>.<value>
#   (eq neq gt gte lt lte like ilike in is). Pagination via limit/offset.
#
# Configuration (environment variables):
#   ODIN_KPI08_URL  API endpoint     (default the kpi08 table below)
#   ODIN_DATE_COL   date column used to filter by year AND to resume
#                   incrementally    (default "aldt" — CONFIRM this is the
#                   arrival date/time column of the kpi08 table)
#   ODIN_ID_COL     unique id column used to de-duplicate on merge (default "id")
#   ODIN_PAGE_SIZE  rows per request (default 1000)
#   ODIN_TOKEN      optional Bearer token (the example endpoint needs none)
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. install.packages('httr2').")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required. install.packages('jsonlite').")
})

# bind data.frames with differing columns (union), filling missing with NA
kpi08_rbind_fill <- function(lst) {
  cols <- unique(unlist(lapply(lst, names)))
  lst <- lapply(lst, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] <- NA
    d[cols]
  })
  do.call(rbind, lst)
}

# =============================================================================
# download_kpi08(years, out_dir)
#
#   years   : years to fetch, e.g. 2026 or 2023:2026 (default: current year)
#   out_dir : where the kpi08_<year>.csv files live (created if missing)
#
# Returns, invisibly, the paths of the year files written.
# =============================================================================
download_kpi08 <- function(years   = as.integer(format(Sys.Date(), "%Y")),
                           out_dir = file.path("data-raw", "kpi08")) {

  base_url  <- Sys.getenv("ODIN_KPI08_URL",
                          unset = "https://odin-ms.icea.decea.mil.br/api/kpi08")
  token     <- Sys.getenv("ODIN_TOKEN", unset = "")
  date_col  <- Sys.getenv("ODIN_DATE_COL", unset = "aldt")
  id_col    <- Sys.getenv("ODIN_ID_COL", unset = "id")
  page_size <- as.integer(Sys.getenv("ODIN_PAGE_SIZE", unset = "1000"))

  years <- suppressWarnings(as.integer(years))
  if (length(years) == 0 || any(is.na(years)))
    stop("Years must be 4-digit numbers, e.g. 2026 or 2023:2026.")

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message("Created ", out_dir)
  }

  # ---- one page of the API --------------------------------------------------
  fetch_page <- function(extra_filters, offset) {
    req <- httr2::request(base_url) |>
      httr2::req_url_query(!!!extra_filters,
                           order  = paste0(date_col, ".asc"),
                           limit  = page_size,
                           offset = offset) |>
      httr2::req_user_agent("BRA-ingestion/kpi08") |>
      httr2::req_timeout(180) |>
      httr2::req_retry(max_tries = 4)
    if (nzchar(token))
      req <- httr2::req_headers(req, Authorization = paste("Bearer", token))

    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) != 200)
      stop("ODIN returned HTTP ", httr2::resp_status(resp), " at offset ", offset)
    body <- httr2::resp_body_string(resp)
    if (!jsonlite::validate(body))
      stop("ODIN response is not valid JSON (first 200 chars):\n", substr(body, 1, 200))
    jsonlite::fromJSON(body, flatten = TRUE)   # data.frame (may have 0 rows)
  }

  # ---- a whole [from, to) window, paginated ---------------------------------
  # Both bounds are pushed to the server with PostgREST's `and=(a.op.v,b.op.v)`
  # syntax, so only the requested year travels over the wire.
  download_window <- function(from_date, to_date) {
    filters <- list(
      and = sprintf("(%s.gte.%s,%s.lt.%s)", date_col, from_date, date_col, to_date)
    )
    rows   <- list()
    offset <- 0L
    repeat {
      page <- fetch_page(filters, offset)
      n <- if (is.data.frame(page)) nrow(page) else 0L
      if (n > 0) rows[[length(rows) + 1L]] <- page
      message(sprintf("    offset %-7d +%d rows", offset, n))
      if (n < page_size) break
      offset <- offset + page_size
    }
    if (length(rows) == 0) return(NULL)
    kpi08_rbind_fill(rows)
  }

  # ---- process each year ----------------------------------------------------
  written <- character(0)

  for (yr in years) {
    out_csv    <- file.path(out_dir, sprintf("kpi08_%d.csv", yr))
    year_start <- sprintf("%d-01-01", yr)
    year_end   <- sprintf("%d-01-01", yr + 1L)   # exclusive upper bound

    existing  <- NULL
    from_date <- year_start
    if (file.exists(out_csv)) {
      existing <- utils::read.csv(out_csv, sep = ";", colClasses = "character",
                                  check.names = FALSE, na.strings = "")
      if (date_col %in% names(existing) && nrow(existing) > 0) {
        have_max <- suppressWarnings(max(existing[[date_col]], na.rm = TRUE))
        if (!is.na(have_max) && nzchar(have_max)) from_date <- have_max
      }
      message(sprintf("Year %d: %d existing row(s); resuming from %s",
                      yr, nrow(existing), from_date))
    } else {
      message(sprintf("Year %d: no file yet; downloading the full year", yr))
    }

    message(sprintf("  Downloading %s from %s to %s ...", date_col, from_date, year_end))
    fresh <- download_window(from_date, year_end)

    if (is.null(fresh) && is.null(existing)) {
      message(sprintf("  Year %d: no rows returned.", yr))
      next
    }

    # merge existing + fresh, de-duplicate by id (fresh wins)
    combined <- if (is.null(existing)) fresh
                else if (is.null(fresh)) existing
                else kpi08_rbind_fill(list(existing, fresh))

    if (id_col %in% names(combined))
      combined <- combined[!duplicated(combined[[id_col]], fromLast = TRUE), , drop = FALSE]
    if (date_col %in% names(combined))
      combined <- combined[order(combined[[date_col]]), , drop = FALSE]

    utils::write.table(combined, out_csv, sep = ";", row.names = FALSE,
                       quote = TRUE, na = "", fileEncoding = "UTF-8")
    written <- c(written, out_csv)
    message(sprintf("  Year %d: wrote %d row(s), %d column(s) -> %s",
                    yr, nrow(combined), ncol(combined), out_csv))
    message("  Columns: ", paste(names(combined), collapse = ", "))
  }

  invisible(written)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args  <- commandArgs(trailingOnly = TRUE)
  years <- if (length(args) == 0) as.integer(format(Sys.Date(), "%Y")) else args
  download_kpi08(years)
  message("Done. If the year filter looks wrong, confirm ODIN_DATE_COL.")
}
