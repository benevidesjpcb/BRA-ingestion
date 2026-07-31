#!/usr/bin/env Rscript
# =============================================================================
# download_odin.R
#
# Generic downloader for the ICEA/DECEA ODIN API (PostgREST), shared by every
# table this project ingests. It is the engine behind KPI08/download_kpi08.R and
# TOTALBR/download_totalbr.R — keep the logic here, not copied per table, so a
# fix reaches every dataset at once.
#
#   source(here::here("ODIN", "download_odin.R"))
#   download_odin("kpi08", 2024:2026, date_col = "aldt", dedup_col = "c")
#
# What it does, and why each part exists (all of it learned the hard way):
#
#   * ONE MONTH PER REQUEST WINDOW, saved as soon as it arrives, so an
#     interrupted run keeps everything already fetched.
#   * A month is judged downloaded by the DAYS IT CONTAINS, never by a file
#     existing: a month fetched while it was still the current one holds only
#     part of its days and must be fetched again once it closes.
#   * Pagination ADVANCES BY THE ROWS ACTUALLY RETURNED, never by the requested
#     limit, because a server-side cap below that limit would otherwise truncate
#     the download in silence.
#   * De-duplication can key on the id PLUS a second column, for tables that
#     legitimately repeat an id across rows (KPI08 does, once per ASMA ring).
#
# Configuration (environment variables):
#   ODIN_API_ROOT   API root       (default https://odin-ms.icea.decea.mil.br/api)
#   ODIN_PAGE_SIZE  rows per request (default 10000)
#   ODIN_TOKEN      optional Bearer token
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. install.packages('httr2').")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required. install.packages('jsonlite').")
})

# Some ODIN columns are JSON arrays — TOTALBR has li_orgaos, li_tipovoo,
# li_regravoo, li_prnav, li_rvsm — which arrive as list-columns and cannot be
# written to a delimited file. Collapse each to a single string so the CSV
# round-trips; the separator is a pipe because the file is semicolon-delimited.
flatten_list_cols <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  for (nm in names(df)[vapply(df, is.list, logical(1))]) {
    df[[nm]] <- vapply(df[[nm]], function(x) {
      x <- unlist(x)
      if (length(x) == 0) NA_character_ else paste(x, collapse = "|")
    }, character(1))
  }
  df
}

# =============================================================================
# odin_probe(table, n)
#
# One small request against a table, to confirm an endpoint and its column names
# BEFORE committing to a download that takes hours. Prints the columns and shows
# the first rows, so `date_col` and `id_col` can be chosen from what is actually
# there rather than guessed.
#
#   odin_probe("total_brasil")     # -> columns, incl. which date columns exist
# =============================================================================
odin_probe <- function(table, n = 5L) {
  api_root <- Sys.getenv("ODIN_API_ROOT",
                         unset = "https://odin-ms.icea.decea.mil.br/api")
  req <- httr2::request(paste0(api_root, "/", table)) |>
    httr2::req_url_query(limit = n) |>
    httr2::req_user_agent(paste0("BRA-ingestion/", table)) |>
    httr2::req_timeout(120)
  token <- Sys.getenv("ODIN_TOKEN", unset = "")
  if (nzchar(token))
    req <- httr2::req_headers(req, Authorization = paste("Bearer", token))

  df <- flatten_list_cols(
    jsonlite::fromJSON(httr2::resp_body_string(httr2::req_perform(req)),
                       flatten = TRUE))
  if (!is.data.frame(df) || nrow(df) == 0) {
    message("The endpoint answered, but with no rows."); return(invisible(df))
  }
  message("Columns (", ncol(df), "): ", paste(names(df), collapse = ", "))
  # anything that looks like a date/time is a candidate for date_col
  cand <- grep("^(dt|dh|d[ah]_|.*(data|date|time|dt|hora)).*", names(df),
               value = TRUE, ignore.case = TRUE)
  if (length(cand) > 0)
    message("Date-like column(s): ", paste(cand, collapse = ", "))
  invisible(df)
}

odin_rbind_fill <- function(lst) {
  cols <- unique(unlist(lapply(lst, names)))
  lst <- lapply(lst, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] <- NA
    d[cols]
  })
  do.call(rbind, lst)
}

# =============================================================================
# download_odin(table, years, out_dir, date_col, id_col, dedup_col, prefix, ...)
#
#   table     : the ODIN table to read, e.g. "kpi08" or "totalbr"
#   years     : years to fetch, e.g. 2026 or 2024:2026 (default: current year)
#   out_dir   : where the <prefix>_<year>.csv files live (created if missing)
#   date_col  : the column the month windows filter on
#   id_col    : the unique record id, used to de-duplicate on merge
#   dedup_col : a second de-duplication key, for tables that repeat an id across
#               rows; NULL when the id alone identifies a row
#   prefix    : file-name prefix (defaults to the table name)
#   base_url  : override the endpoint; by default ODIN_API_ROOT + "/" + table
#   force     : TRUE re-downloads every month even if it is already on disk
#
# Returns, invisibly, the paths of the year files written.
# =============================================================================
download_odin <- function(table,
                          years     = as.integer(format(Sys.Date(), "%Y")),
                          out_dir   = file.path("data-raw", table),
                          date_col  = "aldt",
                          id_col    = "id",
                          dedup_col = NULL,
                          prefix    = table,
                          base_url  = NULL,
                          force     = FALSE) {

  api_root  <- Sys.getenv("ODIN_API_ROOT",
                          unset = "https://odin-ms.icea.decea.mil.br/api")
  if (is.null(base_url)) base_url <- paste0(api_root, "/", table)
  token     <- Sys.getenv("ODIN_TOKEN", unset = "")
  # A second de-duplication key alongside the id. KPI08 needs it because each
  # arrival appears once per ASMA ring under the same id; a table with one row
  # per record leaves this NULL.
  ring_col  <- dedup_col
  # rows per request. CHANGE THIS NUMBER to make the download coarser/faster.
  # If the server caps the page below what we ask, the log says so once and the
  # download still completes (it advances by the rows actually returned).
  page_size <- as.integer(Sys.getenv("ODIN_PAGE_SIZE", unset = "10000"))

  years <- suppressWarnings(as.integer(years))
  if (length(years) == 0 || any(is.na(years)))
    stop("Years must be 4-digit numbers, e.g. 2026 or 2023:2026.")

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message("Created ", out_dir)
  }

  # Offset pagination is only safe over a TOTAL order. Sorting by the date alone
  # leaves thousands of rows tied on the same timestamp, and their relative order
  # is then whatever the database happens to produce for each request — so a row
  # can be returned on two consecutive pages, or on neither. Adding the unique id
  # (and the second de-duplication key, where the id repeats) breaks every tie, so
  # the pages partition the window instead of overlapping it.
  order_by <- paste0(
    unique(c(date_col, id_col, ring_col)), ".asc", collapse = ",")

  # ---- one page of the API --------------------------------------------------
  fetch_page <- function(extra_filters, offset) {
    req <- httr2::request(base_url) |>
      httr2::req_url_query(!!!extra_filters,
                           order  = order_by,
                           limit  = page_size,
                           offset = offset) |>
      httr2::req_user_agent(paste0("BRA-ingestion/", table)) |>
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
    flatten_list_cols(jsonlite::fromJSON(body, flatten = TRUE))
  }

  # ---- a whole [from, to) window, paginated ---------------------------------
  # Both bounds are pushed to the server with PostgREST's `and=(a.op.v,b.op.v)`
  # syntax, so only the requested year travels over the wire.
  download_window <- function(from_date, to_date) {
    filters <- list(
      and = sprintf("(%s.gte.%s,%s.lt.%s)", date_col, from_date, date_col, to_date)
    )
    rows    <- list()
    offset  <- 0L
    total   <- 0L
    first_n <- NA_integer_
    repeat {
      page <- fetch_page(filters, offset)
      n <- if (is.data.frame(page)) nrow(page) else 0L
      if (n == 0L) break                  # an empty page is the only end signal
      if (offset == 0L) first_n <- n      # remembered to tell a cap from a short window
      rows[[length(rows) + 1L]] <- page
      total  <- total + n
      # advance by what the server actually returned: PostgREST may cap the page
      # size below the requested limit, and stepping by `limit` would skip rows
      # (or stop early) whenever that happens.
      offset <- offset + n
      message(sprintf("    +%-6d rows  (total %d)", n, total))
    }
    # A short first page only means the server capped it if more pages followed;
    # if that page was the whole window, the window simply held fewer rows.
    if (!is.na(first_n) && first_n < page_size && total > first_n)
      message(sprintf("    note: the server caps the page at %d rows (asked %d); ",
                      first_n, page_size),
              "raising ODIN_PAGE_SIZE further will not help.")
    if (length(rows) == 0) return(NULL)
    odin_rbind_fill(rows)
  }

  # ---- process each year, ONE MONTH AT A TIME -------------------------------
  # Each month is downloaded and saved on its own under <out_dir>/parts/, then
  # the months are merged into the year file. Progress therefore survives an
  # interrupt: a finished month is never downloaded twice. Only the "open" month
  # (the current one) is refreshed, because it can still receive new records.
  parts_dir <- file.path(out_dir, "parts")
  if (!dir.exists(parts_dir)) dir.create(parts_dir, recursive = TRUE)

  today      <- Sys.Date()
  this_year  <- as.integer(format(today, "%Y"))
  this_month <- as.integer(format(today, "%m"))

  write_csv2 <- function(df, path) {
    utils::write.table(df, path, sep = ";", row.names = FALSE,
                       quote = TRUE, na = "", fileEncoding = "UTF-8")
  }
  # fread(path) treats its first argument as `input`, which on a path holding
  # spaces (OneDrive folders do) is read as a literal string instead of a file.
  # Passing file = is the only form that is unambiguous.
  read_csv2 <- function(path) {
    if (requireNamespace("data.table", quietly = TRUE))
      as.data.frame(data.table::fread(file = path, sep = ";",
                                      colClasses = "character",
                                      na.strings = "", showProgress = FALSE))
    else
      utils::read.csv(path, sep = ";", colClasses = "character",
                      check.names = FALSE, na.strings = "")
  }

  # Which months does an existing year file already cover? Reads only the date
  # column, so a large year file is cheap to inspect. Returns integer months.
  # How many days of the year is a month, at minimum, expected to have? A month
  # downloaded while it was still the current one holds only part of its days, so
  # "the file exists" is not evidence the month is complete: once that month falls
  # into the past it would be skipped forever, frozen partial. Judge a month by
  # the days it actually contains.
  # Vectorised over `mo`: it is called with every month a file holds at once, and
  # seq() on dates only accepts a single `from`. The next month's first day is
  # built arithmetically instead, rolling the year over at December.
  days_in_month <- function(yr, mo) {
    mo    <- as.integer(mo)
    first <- as.Date(sprintf("%d-%02d-01", yr, mo))
    nxt   <- as.Date(sprintf("%d-%02d-01",
                             yr + as.integer(mo == 12L),
                             ifelse(mo == 12L, 1L, mo + 1L)))
    as.integer(nxt - first)
  }

  read_date_col <- function(path) {
    tryCatch({
      if (requireNamespace("data.table", quietly = TRUE))
        data.table::fread(file = path, sep = ";", select = date_col,
                          colClasses = "character", na.strings = "",
                          showProgress = FALSE)[[1]]
      else
        utils::read.csv(path, sep = ";", colClasses = "character",
                        na.strings = "")[[date_col]]
    }, error = function(e) NULL)
  }

  # distinct days present per month, from any file holding the date column
  month_day_counts <- function(path) {
    if (!file.exists(path)) return(integer(0))
    dates <- read_date_col(path)
    if (is.null(dates) || length(dates) == 0) return(integer(0))
    d <- unique(substr(dates[!is.na(dates)], 1, 10))
    mo <- suppressWarnings(as.integer(substr(d, 6, 7)))
    table(mo[!is.na(mo)])
  }

  # Months a file covers well enough to skip. One missing day is tolerated, so a
  # source that genuinely has no movements on a single day does not cause the
  # month to be re-fetched on every run.
  complete_months <- function(counts, yr) {
    if (length(counts) == 0) return(integer(0))
    mo <- as.integer(names(counts))
    mo[as.integer(counts) >= days_in_month(yr, mo) - 1L]
  }

  written <- character(0)

  for (yr in years) {
    if (yr > this_year) {
      message(sprintf("Year %d is in the future; skipped.", yr)); next
    }
    last_month <- if (yr == this_year) this_month else 12L
    out_csv    <- file.path(out_dir, sprintf("%s_%d.csv", prefix, yr))

    # months already held by a previously downloaded year file count as done, so
    # an existing <prefix>_<year>.csv is not downloaded all over again
    year_counts <- if (force) integer(0) else month_day_counts(out_csv)
    have_months <- complete_months(year_counts, yr)
    partial_in_file <- setdiff(as.integer(names(year_counts)), have_months)
    if (length(have_months) > 0)
      message(sprintf("Year %d: existing file already covers month(s) %s",
                      yr, paste(sprintf("%02d", have_months), collapse = ", ")))
    if (length(partial_in_file) > 0)
      message(sprintf("Year %d: month(s) %s are only partially covered; re-fetching",
                      yr, paste(sprintf("%02d", partial_in_file), collapse = ", ")))
    message(sprintf("Year %d: months 01-%02d", yr, last_month))

    for (mo in seq_len(last_month)) {
      part_csv <- file.path(parts_dir, sprintf("%s_%d-%02d.csv", prefix, yr, mo))
      # month window [first day of month, first day of next month)
      from_date <- sprintf("%d-%02d-01", yr, mo)
      to_date   <- if (mo == 12L) sprintf("%d-01-01", yr + 1L)
                   else sprintf("%d-%02d-01", yr, mo + 1L)

      is_open <- (yr == this_year && mo == this_month)   # still receiving data
      # A saved part is only evidence of a finished month if it actually holds the
      # month's days. A part written while the month was still open holds a few
      # days; once the month passes it would otherwise be skipped forever.
      part_ok <- if (!file.exists(part_csv)) FALSE else {
        cnt <- month_day_counts(part_csv)
        # An empty part is the marker for a past month the API had no rows for;
        # that is a finished answer, not a partial one.
        if (length(cnt) == 0) file.info(part_csv)$size < 1024
        else mo %in% complete_months(cnt, yr)
      }
      already <- part_ok || mo %in% have_months
      if (file.exists(part_csv) && !part_ok && !is_open)
        message(sprintf("  %d-%02d  re-fetching (saved part covers only part of the month)",
                        yr, mo))
      if (already && !is_open && !force) {
        message(sprintf("  %d-%02d  skip (already have)", yr, mo)); next
      }

      message(sprintf("  %d-%02d  downloading %s ...", yr, mo,
                      if (is_open) "(open month, refreshing)" else ""))
      part <- download_window(from_date, to_date)

      if (is.null(part)) {
        message(sprintf("  %d-%02d  no rows", yr, mo))
        # remember that an empty past month was checked, so we do not retry it
        if (!is_open) write_csv2(data.frame(), part_csv)
        next
      }
      write_csv2(part, part_csv)                          # persist immediately
      message(sprintf("  %d-%02d  saved %d row(s)", yr, mo, nrow(part)))
    }

    # ---- merge the year's months into one file ------------------------------
    part_files <- list.files(parts_dir,
                             pattern = sprintf("^%s_%d-[0-9]{2}\\.csv$", prefix, yr),
                             full.names = TRUE)
    if (length(part_files) == 0) {
      message(sprintf("Year %d: nothing new to merge; %s left as is.", yr,
                      basename(out_csv)))
      if (file.exists(out_csv)) written <- c(written, out_csv)
      next
    }
    parts <- lapply(part_files, read_csv2)
    # the existing year file is just another part, so months it already holds
    # are preserved instead of being overwritten by the newly fetched ones
    if (file.exists(out_csv)) parts <- c(list(read_csv2(out_csv)), parts)
    parts <- Filter(function(d) !is.null(d) && nrow(d) > 0, parts)
    if (length(parts) == 0) {
      message(sprintf("Year %d: no rows in any month; nothing written.", yr)); next
    }

    combined <- odin_rbind_fill(parts)
    # De-duplicate on the id AND the ASMA ring: each arrival appears once per ring
    # (c = 40 / 100), so keying on the id alone would drop one ring per flight.
    if (id_col %in% names(combined)) {
      key <- if (!is.null(ring_col) && ring_col %in% names(combined))
               paste(combined[[id_col]], combined[[ring_col]]) else combined[[id_col]]
      combined <- combined[!duplicated(key, fromLast = TRUE), , drop = FALSE]
    }
    if (date_col %in% names(combined))
      combined <- combined[order(combined[[date_col]]), , drop = FALSE]

    out_csv <- file.path(out_dir, sprintf("%s_%d.csv", prefix, yr))
    write_csv2(combined, out_csv)
    written <- c(written, out_csv)
    message(sprintf("Year %d: merged %d month(s) -> %d row(s), %d column(s) -> %s",
                    yr, length(parts), nrow(combined), ncol(combined), out_csv))
    message("  Columns: ", paste(names(combined), collapse = ", "))
  }

  invisible(written)
}

