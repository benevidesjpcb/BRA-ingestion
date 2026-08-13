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
#   BRA_PROXY, BRA_PROXY_AUTH, BRA_PROXY_USERPWD   corporate proxy (see proxy.R)
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

# ---- corporate proxy --------------------------------------------------------
# Shared with the TATIC downloader: the network, not the API, decides whether a
# request needs proxy credentials. See proxy.R for the 407 story and the
# BRA_PROXY / ODIN_PROXY settings.
source(here::here("proxy.R"))
odin_proxy <- bra_proxy

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
    httr2::req_timeout(120) |>
    odin_proxy()
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

# =============================================================================
# odin_tables()
#
# The tables the API actually exposes. PostgREST serves an OpenAPI description at
# its root, so the list can be read instead of guessed — which matters when a new
# dataset is added and nobody remembers whether it is called "taxi", "kpi09" or
# something else entirely. Guessing costs a wasted download; asking costs one
# request.
# =============================================================================
odin_tables <- function() {
  api_root <- Sys.getenv("ODIN_API_ROOT",
                         unset = "https://odin-ms.icea.decea.mil.br/api")
  out <- tryCatch({
    req <- httr2::request(api_root) |>
      httr2::req_headers(Accept = "application/openapi+json") |>
      httr2::req_user_agent("BRA-ingestion/tables") |>
      httr2::req_timeout(60) |>
      odin_proxy()
    tok <- Sys.getenv("ODIN_TOKEN", unset = "")
    if (nzchar(tok)) req <- httr2::req_headers(req, Authorization = paste("Bearer", tok))
    spec <- jsonlite::fromJSON(httr2::resp_body_string(httr2::req_perform(req)))
    nm <- names(spec$paths)
    sort(sub("^/", "", nm[nzchar(sub("^/", "", nm))]))
  }, error = function(e) {
    message("Could not read the table list (", conditionMessage(e), ").")
    character(0)
  })
  if (length(out) == 0) message("No tables listed; check ODIN_API_ROOT and the token.")
  out
}

# =============================================================================
# odin_count(table, date_col, from, to) — how many rows the API HAS in a window
#
# Asks the server to count, without transferring the rows: PostgREST answers
# `Prefer: count=exact` in the Content-Range header. One cheap request.
#
# This is what separates "the download lost a day" from "the source has no such
# day" — two problems with opposite fixes. Re-downloading a day the API does not
# have just wastes time; treating a download gap as a source gap silently loses
# data.
#
#   odin_count("dstaxi", "dhbimtra", "2025-10-31", "2025-11-01")
#
# Bounds are half-open [from, to), the same as the downloader's month windows.
# =============================================================================
odin_count <- function(table, date_col, from, to, base_url = NULL) {
  api_root <- Sys.getenv("ODIN_API_ROOT",
                         unset = "https://odin-ms.icea.decea.mil.br/api")
  if (is.null(base_url)) base_url <- paste0(api_root, "/", table)
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      and = sprintf("(%s.gte.%s,%s.lt.%s)", date_col, from, date_col, to),
      limit = 1) |>
    # ask for the count and for as little data as possible
    httr2::req_headers(Prefer = "count=exact") |>
    httr2::req_user_agent(paste0("BRA-ingestion/count/", table)) |>
    httr2::req_timeout(120) |>
    odin_proxy()
  tok <- Sys.getenv("ODIN_TOKEN", unset = "")
  if (nzchar(tok)) req <- httr2::req_headers(req, Authorization = paste("Bearer", tok))

  resp <- httr2::req_perform(req)
  cr   <- httr2::resp_header(resp, "content-range")   # e.g. "0-0/9291"
  n    <- suppressWarnings(as.integer(sub(".*/", "", cr %||% "")))
  if (is.na(n))
    message("The server did not return a count (Content-Range: ", cr, ").")
  else
    message(sprintf("%s [%s, %s): %s row(s)", table, from, to,
                    format(n, big.mark = ",")))
  invisible(n)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

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
#   id_col    : the unique record id, used to de-duplicate on merge. NULL for a
#               table that has no unique id — nothing is then de-duplicated, and
#               `order_cols` must carry the total order instead.
#   dedup_col : a second de-duplication key, for tables that repeat an id across
#               rows; NULL when the id alone identifies a row
#   order_cols: the columns pagination orders on. Defaults to the date, the id
#               and the dedup key, which is a total order whenever an id exists.
#               A table without one needs enough columns here to break every tie
#               (see the comment at order_by below).
#   rename_cols: named character vector, API name -> name written to disk, for a
#               source whose established file header differs from the API's.
#               Applied the moment a month arrives, so the parts, the year file
#               and everything downstream all carry the on-disk names.
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
                          order_cols  = NULL,
                          rename_cols = NULL,
                          prefix    = table,
                          # what goes between the prefix and the year in a file
                          # name. Taxi files are already called dsTaxi2025.csv
                          # everywhere downstream, so its wrapper passes "" and the
                          # download lands on the name the rest of the pipeline
                          # already reads.
                          sep       = "_",
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
  #
  # A table with no unique id (dstaxi has none) cannot rely on that, so its
  # wrapper passes `order_cols` explicitly: enough columns that two rows sharing
  # all of them would be indistinguishable anyway.
  if (is.null(order_cols) || length(order_cols) == 0)
    order_cols <- c(date_col, id_col, ring_col)
  order_by <- paste0(unique(order_cols), ".asc", collapse = ",")

  # ---- API names vs the names written to disk -------------------------------
  # Everything after the download works on the on-disk names: the parts, the year
  # file, the month coverage read back from them, and the merge. Only the request
  # itself speaks the API's names.
  rename_part <- function(d) {
    if (is.null(rename_cols) || is.null(d) || !is.data.frame(d)) return(d)
    i  <- match(names(rename_cols), names(d))
    ok <- !is.na(i)
    names(d)[i[ok]] <- unname(rename_cols)[ok]
    d
  }
  disk_name <- function(x) {
    if (is.null(x) || is.null(rename_cols) || !(x %in% names(rename_cols))) x
    else unname(rename_cols[[x]])
  }
  date_col_disk <- disk_name(date_col)
  id_col_disk   <- disk_name(id_col)
  ring_col_disk <- disk_name(ring_col)

  # ---- one page of the API --------------------------------------------------
  fetch_page <- function(extra_filters, offset) {
    req <- httr2::request(base_url) |>
      httr2::req_url_query(!!!extra_filters,
                           order  = order_by,
                           limit  = page_size,
                           offset = offset) |>
      httr2::req_user_agent(paste0("BRA-ingestion/", table)) |>
      httr2::req_timeout(180) |>
      httr2::req_retry(max_tries = 4) |>
      odin_proxy()
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
  # Some existing year files carry rows with MORE fields than the header
  # (dsTaxi2024.csv does, from line 377601). fread then STOPS THERE AND ONLY
  # WARNS, so the rest of the year silently disappears from whatever is read
  # next — which, below, is the month coverage this decides against.
  #
  # fill = TRUE is NOT enough: it pads rows that are short, and still stops on a
  # row that is long. Only fill = Inf makes fread scan the file first and size
  # the table to the widest row. It is a number, so older data.table (which only
  # accepts TRUE/FALSE) needs the fallback.
  #
  # header = TRUE goes with it, and is not optional. fread decides on its own
  # whether row 1 is a header, and a header of 14 names above rows of 15 fields
  # looks to it like a short data row: it demotes the names to data and calls
  # every column V1..V15. The date column then "does not exist" in a file that
  # plainly has it. Our files are always written with a header, so there is no
  # ambiguity worth preserving.
  fill_all <- if (requireNamespace("data.table", quietly = TRUE) &&
                  utils::packageVersion("data.table") >= "1.15.0") Inf else TRUE
  read_csv2 <- function(path) {
    if (requireNamespace("data.table", quietly = TRUE))
      as.data.frame(data.table::fread(file = path, sep = ";",
                                      colClasses = "character",
                                      na.strings = "", showProgress = FALSE,
                                      fill = fill_all, header = TRUE))
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

  # Which column in THIS file holds the date? A file that came from an older
  # export can spell it differently from both the API and today's convention
  # (dh_bimtra / dhbimtra / DH_BIMTRA are the same column). Match on the header,
  # ignoring case and underscores, instead of failing. Returns NA when the file
  # really has no such column.
  file_date_col <- function(path) {
    hdr <- tryCatch(
      names(data.table::fread(file = path, sep = ";", nrows = 1L,
                              showProgress = FALSE, fill = fill_all,
                              header = TRUE)),
      error = function(e) character(0))
    if (length(hdr) == 0) return(NA_character_)
    if (date_col_disk %in% hdr) return(date_col_disk)
    flat <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(x)))
    hit  <- hdr[flat(hdr) %in% flat(c(date_col_disk, date_col))]
    if (length(hit) > 0) hit[1] else NA_character_
  }

  read_date_col <- function(path) {
    col <- file_date_col(path)
    if (is.na(col)) {
      # Loudly: silence here reads as "this year is empty", and the whole year is
      # then downloaded again on top of a file that was fine.
      warning("No date column in ", basename(path), ": looked for '",
              date_col_disk, "'. Its columns are read as: ",
              paste(utils::head(tryCatch(
                names(data.table::fread(file = path, sep = ";", nrows = 1L,
                                        showProgress = FALSE, fill = fill_all,
                                        header = TRUE)),
                error = function(e) "unreadable"), 20), collapse = ", "),
              ".\nThe file cannot be checked for coverage, so its months will be ",
              "downloaded again.", call. = FALSE, immediate. = TRUE)
      return(NULL)
    }
    tryCatch({
      if (requireNamespace("data.table", quietly = TRUE))
        # fill for the same reason as read_csv2: a ragged row must not truncate
        # the file, or the months after it look missing and get re-downloaded on
        # every run.
        data.table::fread(file = path, sep = ";", select = col,
                          colClasses = "character", na.strings = "",
                          showProgress = FALSE, fill = fill_all,
                          header = TRUE)[[1]]
      else
        utils::read.csv(path, sep = ";", colClasses = "character",
                        na.strings = "")[[col]]
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
    out_csv    <- file.path(out_dir, paste0(prefix, sep, yr, ".csv"))

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
      part_csv <- file.path(parts_dir,
                            sprintf("%s%s%d-%02d.csv", prefix, sep, yr, mo))
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
      part <- rename_part(download_window(from_date, to_date))

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
                             pattern = sprintf("^%s%s%d-[0-9]{2}\\.csv$",
                                               prefix, sep, yr),
                             full.names = TRUE)
    if (length(part_files) == 0) {
      message(sprintf("Year %d: nothing new to merge; %s left as is.", yr,
                      basename(out_csv)))
      if (file.exists(out_csv)) written <- c(written, out_csv)
      next
    }
    parts <- lapply(part_files, read_csv2)
    # The existing year file is just another part, so months it already holds are
    # preserved instead of being overwritten by the newly fetched ones — EXCEPT
    # for the months that were re-fetched. For those the freshly downloaded part
    # is the truth, and the old copy of the same month has to go: keeping both
    # would duplicate every one of its rows in a table that has no unique key to
    # de-duplicate on afterwards.
    fetched_months <- as.integer(sub(".*-([0-9]{2})\\.csv$", "\\1",
                                     basename(part_files)))
    if (file.exists(out_csv)) {
      old <- read_csv2(out_csv)
      # the existing file may spell the date column its own way; match it the
      # same forgiving way the coverage check does
      old_dc <- if (is.null(old)) NA_character_ else {
        flat <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(x)))
        hit  <- names(old)[flat(names(old)) %in% flat(c(date_col_disk, date_col))]
        if (length(hit) > 0) hit[1] else NA_character_
      }
      if (!is.null(old) && nrow(old) > 0 && !is.na(old_dc)) {
        old_mo <- suppressWarnings(as.integer(substr(old[[old_dc]], 6, 7)))
        drop   <- !is.na(old_mo) & old_mo %in% fetched_months
        if (any(drop)) {
          message(sprintf("  replacing %d row(s) of month(s) %s already in %s",
                          sum(drop),
                          paste(sprintf("%02d", sort(unique(old_mo[drop]))),
                                collapse = ", "),
                          basename(out_csv)))
          old <- old[!drop, , drop = FALSE]
        }
      }
      parts <- c(list(old), parts)
    }
    parts <- Filter(function(d) !is.null(d) && nrow(d) > 0, parts)
    if (length(parts) == 0) {
      message(sprintf("Year %d: no rows in any month; nothing written.", yr)); next
    }

    combined <- odin_rbind_fill(parts)
    # De-duplicate on the id AND the ASMA ring: each arrival appears once per ring
    # (c = 40 / 100), so keying on the id alone would drop one ring per flight.
    # A table with no id (id_col = NULL) is left alone: dropping rows on a key
    # that does not identify a record would delete real movements.
    if (!is.null(id_col_disk) && id_col_disk %in% names(combined)) {
      key <- if (!is.null(ring_col_disk) && ring_col_disk %in% names(combined))
               paste(combined[[id_col_disk]], combined[[ring_col_disk]])
             else combined[[id_col_disk]]
      combined <- combined[!duplicated(key, fromLast = TRUE), , drop = FALSE]
    }
    if (date_col_disk %in% names(combined))
      combined <- combined[order(combined[[date_col_disk]]), , drop = FALSE]

    write_csv2(combined, out_csv)
    written <- c(written, out_csv)
    message(sprintf("Year %d: merged %d month(s) -> %d row(s), %d column(s) -> %s",
                    yr, length(parts), nrow(combined), ncol(combined), out_csv))
    message("  Columns: ", paste(names(combined), collapse = ", "))

    # ---- days with no rows at all -------------------------------------------
    # A month is allowed to skip re-fetching with one day missing, so that a day
    # the source genuinely has no rows for does not trigger a download on every
    # run. That tolerance is silent, and it hid a real hole: dstaxi has NO rows
    # for 2025-10-31, ~9,300 movements that another export of the same data has.
    # Report the gap rather than re-fetch it: if the API does not have the day,
    # downloading it again changes nothing. odin_count() settles which it is.
    if (date_col_disk %in% names(combined)) {
      have  <- unique(substr(combined[[date_col_disk]], 1, 10))
      last  <- min(as.Date(sprintf("%d-12-31", yr)), today - 1)
      if (as.Date(sprintf("%d-01-01", yr)) <= last) {
        want <- format(seq(as.Date(sprintf("%d-01-01", yr)), last, by = "day"))
        miss <- setdiff(want, have)
        if (length(miss) > 0) {
          shown <- paste(utils::head(miss, 15), collapse = ", ")
          if (length(miss) > 15) shown <- paste0(shown, ", ... (+",
                                                 length(miss) - 15, " more)")
          message(sprintf("  NOTE: %d day(s) with no rows: %s", length(miss), shown))
          message("        Check whether the source has them, e.g.\n",
                  sprintf("          odin_count(\"%s\", \"%s\", \"%s\", \"%s\")",
                          table, date_col, miss[1],
                          format(as.Date(miss[1]) + 1)))
        }
      }
    }
  }

  invisible(written)
}

