#!/usr/bin/env Rscript
# =============================================================================
# download_kpi08.R
#
# Downloads KPI08 (ASMA) data from the ICEA/DECEA ODIN API into ONE FILE PER
# YEAR, fetching ONE MONTH AT A TIME.
#
# Each month is saved on its own under <out_dir>/parts/kpi08_<year>-<mm>.csv as
# soon as it arrives, and the months are then merged into <out_dir>/kpi08_<year>.csv.
# Because every finished month is on disk, an interrupted run loses nothing: a
# re-run skips the months it already has and only refreshes the current ("open")
# month, which can still receive new records. So you can run it daily or monthly
# to keep the current year up to date.
#
# Two ways to use it:
#
#   1) From a terminal
#        Rscript KPI08/download_kpi08.R                 # current year
#        Rscript KPI08/download_kpi08.R 2026            # one year
#        Rscript KPI08/download_kpi08.R 2023 2024 2025  # several years
#
#   2) From R / a Quarto chunk — source it and call the function
#        source(here::here("KPI08", "download_kpi08.R"))
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
#   ODIN_PAGE_SIZE  rows per request (default 10000; the default is set in the
#                   `page_size <- ...` line inside download_kpi08() — edit it
#                   there to change it permanently)
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
#   force   : TRUE re-downloads every month even if it is already on disk
#
# Returns, invisibly, the paths of the year files written.
# =============================================================================
download_kpi08 <- function(years   = as.integer(format(Sys.Date(), "%Y")),
                           out_dir = file.path("data-raw", "kpi08"),
                           force   = FALSE) {

  base_url  <- Sys.getenv("ODIN_KPI08_URL",
                          unset = "https://odin-ms.icea.decea.mil.br/api/kpi08")
  token     <- Sys.getenv("ODIN_TOKEN", unset = "")
  date_col  <- Sys.getenv("ODIN_DATE_COL", unset = "aldt")
  id_col    <- Sys.getenv("ODIN_ID_COL", unset = "id")
  ring_col  <- Sys.getenv("ODIN_RING_COL", unset = "c")   # ASMA ring (40 / 100 NM)
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
    total  <- 0L
    repeat {
      page <- fetch_page(filters, offset)
      n <- if (is.data.frame(page)) nrow(page) else 0L
      if (n == 0L) break                  # an empty page is the only end signal
      if (offset == 0L && n < page_size)
        message(sprintf("    note: server capped the page at %d rows (asked %d); ",
                        n, page_size),
                "raising ODIN_PAGE_SIZE further will not help.")
      rows[[length(rows) + 1L]] <- page
      total  <- total + n
      # advance by what the server actually returned: PostgREST may cap the page
      # size below the requested limit, and stepping by `limit` would skip rows
      # (or stop early) whenever that happens.
      offset <- offset + n
      message(sprintf("    +%-6d rows  (total %d)", n, total))
    }
    if (length(rows) == 0) return(NULL)
    kpi08_rbind_fill(rows)
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
  read_csv2 <- function(path) {
    if (requireNamespace("data.table", quietly = TRUE))
      as.data.frame(data.table::fread(path, sep = ";", colClasses = "character",
                                      na.strings = "", showProgress = FALSE))
    else
      utils::read.csv(path, sep = ";", colClasses = "character",
                      check.names = FALSE, na.strings = "")
  }

  # Which months does an existing year file already cover? Reads only the date
  # column, so a large year file is cheap to inspect. Returns integer months.
  months_in_year_file <- function(path) {
    if (!file.exists(path)) return(integer(0))
    dates <- tryCatch({
      if (requireNamespace("data.table", quietly = TRUE))
        data.table::fread(path, sep = ";", select = date_col,
                          colClasses = "character", na.strings = "",
                          showProgress = FALSE)[[1]]
      else
        utils::read.csv(path, sep = ";", colClasses = "character",
                        na.strings = "")[[date_col]]
    }, error = function(e) NULL)
    if (is.null(dates) || length(dates) == 0) return(integer(0))
    mo <- suppressWarnings(as.integer(substr(dates, 6, 7)))
    sort(unique(mo[!is.na(mo)]))
  }

  written <- character(0)

  for (yr in years) {
    if (yr > this_year) {
      message(sprintf("Year %d is in the future; skipped.", yr)); next
    }
    last_month <- if (yr == this_year) this_month else 12L
    out_csv    <- file.path(out_dir, sprintf("kpi08_%d.csv", yr))

    # months already held by a previously downloaded year file count as done, so
    # an existing kpi08_<year>.csv is not downloaded all over again
    have_months <- if (force) integer(0) else months_in_year_file(out_csv)
    if (length(have_months) > 0)
      message(sprintf("Year %d: existing file already covers month(s) %s",
                      yr, paste(sprintf("%02d", have_months), collapse = ", ")))
    message(sprintf("Year %d: months 01-%02d", yr, last_month))

    for (mo in seq_len(last_month)) {
      part_csv <- file.path(parts_dir, sprintf("kpi08_%d-%02d.csv", yr, mo))
      # month window [first day of month, first day of next month)
      from_date <- sprintf("%d-%02d-01", yr, mo)
      to_date   <- if (mo == 12L) sprintf("%d-01-01", yr + 1L)
                   else sprintf("%d-%02d-01", yr, mo + 1L)

      is_open <- (yr == this_year && mo == this_month)   # still receiving data
      already <- file.exists(part_csv) || mo %in% have_months
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
                             pattern = sprintf("^kpi08_%d-[0-9]{2}\\.csv$", yr),
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

    combined <- kpi08_rbind_fill(parts)
    # De-duplicate on the id AND the ASMA ring: each arrival appears once per ring
    # (c = 40 / 100), so keying on the id alone would drop one ring per flight.
    if (id_col %in% names(combined)) {
      key <- if (ring_col %in% names(combined))
               paste(combined[[id_col]], combined[[ring_col]]) else combined[[id_col]]
      combined <- combined[!duplicated(key, fromLast = TRUE), , drop = FALSE]
    }
    if (date_col %in% names(combined))
      combined <- combined[order(combined[[date_col]]), , drop = FALSE]

    out_csv <- file.path(out_dir, sprintf("kpi08_%d.csv", yr))
    write_csv2(combined, out_csv)
    written <- c(written, out_csv)
    message(sprintf("Year %d: merged %d month(s) -> %d row(s), %d column(s) -> %s",
                    yr, length(parts), nrow(combined), ncol(combined), out_csv))
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
