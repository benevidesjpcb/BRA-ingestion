#!/usr/bin/env Rscript
# =============================================================================
# totalbr_sources.R
#
# TOTALBR arrives from two places and both are legitimate sources:
#
#   * a PARQUET archive holding the history (all airports, up to 2025), dropped
#     into data-raw/totalbr/ by hand — one file, ~1 GB;
#   * the ODIN API, which fills in whatever the archive does not cover.
#
# These helpers answer one question across both: WHICH DAYS DO WE ACTUALLY
# HAVE? Everything else follows from that — what to download, whether a year is
# complete, whether a month was truncated.
#
# Nothing here filters by airport. TOTALBR is the national table and is kept
# whole; restricting it to the study airports is a downstream decision, not an
# ingestion one, and doing it on the way in would throw away data that cannot be
# recovered without downloading the year again.
#
#   source(here::here("TOTALBR", "totalbr_sources.R"))
#   totalbr_day_counts()      # one row per source and day
#   totalbr_missing_years(2019:2026)
# =============================================================================

# Where the parquet archive lives. BRA_TOTALBR_PARQUET points at it directly
# when it is kept outside the repository (it is large, and data-raw/ is
# git-ignored); otherwise any .parquet in the raw folder is picked up.
totalbr_sources <- function(raw_dir = here::here("data-raw", "totalbr"),
                            include_parts = FALSE) {
  env_parq <- Sys.getenv("BRA_TOTALBR_PARQUET", unset = "")
  parquet <- if (nzchar(env_parq)) {
    if (file.exists(env_parq) || dir.exists(env_parq)) env_parq else character(0)
  } else if (dir.exists(raw_dir)) {
    list.files(raw_dir, pattern = "\\.parquet$", full.names = TRUE)
  } else character(0)

  # The archive is often dropped straight into data-raw/ rather than into the
  # dataset folder beside the downloads. Look one level up as well, but only for
  # files named after this dataset, so an unrelated parquet is not picked up.
  if (length(parquet) == 0 && !nzchar(env_parq)) {
    up <- dirname(raw_dir)
    if (dir.exists(up))
      parquet <- list.files(up, pattern = "^totalbr.*\\.parquet$",
                            full.names = TRUE)
  }

  csv <- if (dir.exists(raw_dir))
    list.files(raw_dir, pattern = "^totalbr_[0-9]{4}\\.csv$", full.names = TRUE)
  else character(0)

  # The per-month parts, as downloaded, BEFORE the merge de-duplicates them. The
  # year file cannot answer "did the API repeat a key?" — the merge has already
  # removed the evidence — so the duplicate check reads these instead.
  parts <- if (include_parts && dir.exists(file.path(raw_dir, "parts")))
    list.files(file.path(raw_dir, "parts"),
               pattern = "^totalbr_[0-9]{4}-[0-9]{2}\\.csv$", full.names = TRUE)
  else character(0)

  list(parquet = parquet, csv = csv, parts = parts)
}

# THE ZONE A COUNT IS TAKEN IN, stated once and used everywhere.
#
# The parquet archive stores its time columns as timestamp[us, tz=Europe/Paris].
# That zone says where the FILE WAS WRITTEN, not anything about Brazilian
# traffic — and it silently decided results: arrow computes lubridate::year() in
# the field's own zone, so a scan that let it do the work counted 2025 in Paris
# time (2,109,623 rows) while a scan that collected the column and formatted it
# in UTC counted 2,109,856. Two functions, one file, 233 rows apart, with nothing
# on screen to say why.
#
# Every derivation of a year or a day now names its zone. Change it here, not by
# whichever engine happened to evaluate the expression.
TOTALBR_TZ <- "UTC"

# ---- deriving YEAR / DAY from the date column, in arrow or in R -------------
# The date column may be a timestamp or text, and the two need different
# expressions. Built with rlang symbols rather than the .data pronoun because
# arrow's dplyr bindings evaluate these against its own query engine, where
# .data is not reliably supported.
totalbr_is_text_date <- function(ds, date_col) {
  grepl("string|utf8",
        ds$schema$GetFieldByName(date_col)$type$ToString(), ignore.case = TRUE)
}

totalbr_add_year_day <- function(q, date_col, is_text, day = FALSE,
                                 tz = TOTALBR_TZ) {
  s <- rlang::sym(date_col)
  if (is_text) {
    # text is already an ISO string; there is no zone to honour or ignore
    q <- dplyr::mutate(q, YEAR = substr(!!s, 1, 4))
    if (day) q <- dplyr::mutate(q, DAY = substr(!!s, 1, 10))
    return(q)
  }
  # A stored timestamp carries a zone, and arrow will use it unless told
  # otherwise. with_tz() moves the instant into the zone we chose to count in;
  # if this arrow build has no binding for it, fall back to the field's own zone
  # and SAY SO, rather than return a number whose meaning is unstated.
  shifted <- tryCatch({
    qq <- dplyr::mutate(q, .TZ_AT = lubridate::with_tz(!!s, tzone = tz))
    dplyr::compute(utils::head(qq, 1))   # force it: with_tz may fail lazily
    qq
  }, error = function(e) {
    message("  (this arrow build cannot shift the timezone: counting in the ",
            "field's own zone, not ", tz, ")")
    NULL
  })
  if (is.null(shifted)) {
    q <- dplyr::mutate(q, YEAR = as.character(lubridate::year(!!s)))
    if (day) q <- dplyr::mutate(q, DAY = as.character(as.Date(!!s)))
    return(q)
  }
  q <- dplyr::mutate(shifted, YEAR = as.character(lubridate::year(.TZ_AT)))
  if (day) q <- dplyr::mutate(q, DAY = as.character(as.Date(.TZ_AT)))
  q
}

# ---- days held by the parquet archive ---------------------------------------
# Counted with arrow's query engine rather than by reading the file: a 1 GB
# archive does not need to enter memory to be counted, and pushing the group-by
# down returns a few thousand rows instead of tens of millions.
totalbr_parquet_day_counts <- function(path, date_col = "dt_dia") {
  ds <- arrow::open_dataset(path)
  if (!date_col %in% names(ds))
    stop("The parquet has no column '", date_col, "'. Columns: ",
         paste(names(ds), collapse = ", "))

  # the date column may be stored as a timestamp or as text; both reduce to the
  # first ten characters of an ISO date, which is all the day count needs
  is_text <- totalbr_is_text_date(ds, date_col)

  counted <- tryCatch({
    q <- dplyr::select(ds, dplyr::all_of(date_col))
    q <- totalbr_add_year_day(q, date_col, is_text, day = TRUE)
    q |>
      dplyr::group_by(DAY) |>
      dplyr::summarise(MOVEMENTS = dplyr::n(), .groups = "drop") |>
      dplyr::collect() |>
      dplyr::rename(DATE = DAY)
  }, error = function(e) {
    # the pushdown is an optimisation, not a requirement: if this arrow build
    # cannot do it, read the one column and count in R
    message("  (arrow pushdown unavailable: ", conditionMessage(e),
            " — reading the date column instead)")
    v <- dplyr::collect(dplyr::select(ds, DT = dplyr::all_of(date_col)))$DT
    tibble::tibble(DATE = substr(as.character(v), 1, 10)) |>
      dplyr::count(DATE, name = "MOVEMENTS")
  })

  counted |>
    dplyr::filter(!is.na(DATE), DATE != "") |>
    dplyr::mutate(SOURCE = basename(path), TYPE = "parquet")
}

# ---- days held by a downloaded year CSV -------------------------------------
totalbr_csv_day_counts <- function(path, date_col = "dt_dia") {
  # fread(file = ) — not fread(path): passed positionally, a path containing
  # spaces (OneDrive folders have them) is read as literal text, not a file
  v <- data.table::fread(file = path, sep = ";", select = date_col,
                         colClasses = "character", na.strings = "",
                         showProgress = FALSE)[[1]]
  tibble::tibble(DATE = substr(v, 1, 10)) |>
    dplyr::filter(!is.na(DATE), DATE != "") |>
    dplyr::count(DATE, name = "MOVEMENTS") |>
    dplyr::mutate(SOURCE = basename(path), TYPE = "csv")
}

# =============================================================================
# totalbr_day_counts(raw_dir, date_col)
#
# One row per SOURCE and DATE, with the movement count. Every completeness check
# in the pipeline is built on this, so the parquet archive and the downloaded
# CSVs are judged by exactly the same rule.
# =============================================================================
totalbr_day_counts <- function(raw_dir  = here::here("data-raw", "totalbr"),
                               date_col = "dt_dia") {
  src <- totalbr_sources(raw_dir)
  if (length(src$parquet) == 0 && length(src$csv) == 0)
    return(tibble::tibble(SOURCE = character(0), TYPE = character(0),
                          DATE = character(0), MOVEMENTS = integer(0)))

  parts <- c(
    lapply(src$parquet, function(p) totalbr_parquet_day_counts(p, date_col)),
    lapply(src$csv,     function(f) totalbr_csv_day_counts(f, date_col))
  )
  dplyr::bind_rows(parts) |>
    dplyr::mutate(YEAR  = substr(DATE, 1, 4),
                  MONTH = substr(DATE, 6, 7)) |>
    dplyr::select(SOURCE, TYPE, YEAR, MONTH, DATE, MOVEMENTS)
}

# =============================================================================
# totalbr_schema_info(raw_dir)
#
# The arrow type of every time column in the parquet, TIMEZONE INCLUDED.
#
# This exists because two functions in this repository counted the same year of
# the same file and disagreed by 233 rows: the comparison lets arrow compute the
# year with lubridate::year(), which honours whatever timezone the field carries,
# while totalbr_year_count() collects the column and formats it in UTC. If the
# field is stored as timestamp[us, tz=America/Sao_Paulo] those two are three hours
# apart and a few hundred rows fall on different sides of 1 January.
#
# A count is only as trustworthy as the zone it was taken in, and the zone is a
# property of the file that has to be read rather than assumed.
# =============================================================================
totalbr_schema_info <- function(raw_dir = here::here("data-raw", "totalbr")) {
  src <- totalbr_sources(raw_dir)
  if (length(src$parquet) == 0) {
    message("No parquet archive in ", raw_dir); return(tibble::tibble())
  }
  purrr::map(src$parquet, function(path) {
    sch <- arrow::open_dataset(path)$schema
    nm  <- names(sch)
    tibble::tibble(
      FILE   = basename(path),
      COLUMN = nm,
      TYPE   = vapply(nm, function(x) sch$GetFieldByName(x)$type$ToString(),
                      character(1))
    ) |>
      dplyr::filter(grepl("^d[ht]_|timestamp", COLUMN) |
                    grepl("timestamp", TYPE, ignore.case = TRUE))
  }) |> purrr::list_rbind()
}

# =============================================================================
# totalbr_year_count(years, official, tz_offset_h, raw_dir, date_col)
#
# How many rows each year holds, under EVERY plausible definition of "the year" —
# because the answer differs and only one of them reproduces the official figure.
#
# Brazil publishes 2,109,588 controlled flights for 2025; this repository counts
# 2,109,623 in the archive, 35 more. A gap that small is a boundary, not a data
# difference, and there are four candidates for where the boundary sits:
#
#   dt_dia in UTC          what the pipeline uses today
#   dt_dia in local time   UTC-3: the first three hours of 1 January UTC are
#                          still 31 December in Brazil
#   dh_inicio in UTC       the movement rather than the reference day
#   dh_inicio in local
#
# Whichever column and zone reproduce the published number is the definition the
# study should adopt — and it should be adopted deliberately, not inherited from
# whichever column a script happened to filter on.
# =============================================================================
# `official` takes one figure per year, named — c("2025" = 2109588, "2024" = ...)
# — or a bare number when a single year is requested. Years with no figure get NA
# rather than a difference against somebody else's total.
totalbr_year_count <- function(years       = NULL,
                               official    = NULL,
                               tz_offset_h = -3,
                               raw_dir     = here::here("data-raw", "totalbr"),
                               date_col    = "dt_dia") {
  src <- totalbr_sources(raw_dir)

  one <- function(path) {
    want <- c(date_col, "dh_inicio")
    d <- if (grepl("\\.parquet$", path)) {
      ds <- arrow::open_dataset(path)
      if (!all(want %in% names(ds))) return(NULL)
      dplyr::collect(dplyr::select(ds, dplyr::all_of(want)))
    } else {
      head1 <- data.table::fread(file = path, sep = ";", nrows = 0,
                                 showProgress = FALSE)
      if (!all(want %in% names(head1))) return(NULL)
      tibble::as_tibble(
        data.table::fread(file = path, sep = ";", select = want,
                          colClasses = "character", na.strings = "",
                          showProgress = FALSE))
    }
    if (is.null(d) || nrow(d) == 0) return(NULL)
    to_t <- function(v) {
      if (inherits(v, "POSIXct")) { attr(v, "tzone") <- "UTC"; return(v) }
      as.POSIXct(sub("T", " ", as.character(v), fixed = TRUE), tz = "UTC")
    }
    dtd <- to_t(d[[date_col]]); ini <- to_t(d$dh_inicio)
    off <- tz_offset_h * 3600
    dplyr::bind_rows(
      tibble::tibble(SOURCE = basename(path), DEFINITION = "dt_dia (UTC)",
                     YEAR = format(dtd, "%Y", tz = "UTC")),
      tibble::tibble(SOURCE = basename(path),
                     DEFINITION = sprintf("dt_dia (UTC%+d)", tz_offset_h),
                     YEAR = format(dtd + off, "%Y", tz = "UTC")),
      tibble::tibble(SOURCE = basename(path), DEFINITION = "dh_inicio (UTC)",
                     YEAR = format(ini, "%Y", tz = "UTC")),
      tibble::tibble(SOURCE = basename(path),
                     DEFINITION = sprintf("dh_inicio (UTC%+d)", tz_offset_h),
                     YEAR = format(ini + off, "%Y", tz = "UTC")))
  }

  all <- dplyr::bind_rows(Filter(Negate(is.null),
                                 lapply(c(src$parquet, src$csv), one)))
  if (nrow(all) == 0) return(tibble::tibble())

  out <- all |>
    dplyr::filter(!is.na(YEAR)) |>
    dplyr::count(SOURCE, DEFINITION, YEAR, name = "ROWS")
  if (!is.null(years))
    out <- dplyr::filter(out, YEAR %in% as.character(years))

  if (!is.null(official)) {
    # An official figure belongs to ONE year. Applied to every row it subtracts
    # the 2025 total from 2019 and prints a difference of minus half a million,
    # which is not a finding about anything. Either name the years, or ask for a
    # single year and let the one figure attach to it.
    if (is.null(names(official))) {
      if (length(official) == 1 && !is.null(years) &&
          length(unique(as.character(years))) == 1) {
        official <- stats::setNames(official, as.character(years)[1])
      } else {
        stop("`official` needs one figure per year, named: ",
             "c(\"2025\" = 2109588). A bare number is only unambiguous when a ",
             "single year is requested.")
      }
    }
    out$OFFICIAL <- unname(official[out$YEAR])   # NA where no figure is known
    out$DIFF     <- out$ROWS - out$OFFICIAL
  }
  # most recent year first: the archive spans 2018-2026 and a tibble prints ten
  # rows, so ascending order hides exactly the years anyone is asking about
  dplyr::arrange(out, dplyr::desc(YEAR), SOURCE, DEFINITION)
}

# =============================================================================
# totalbr_year_edges(year, hours, raw_dir, date_col)
#
# The rows sitting within `hours` of a year boundary — the ones any change of
# definition moves from one year to the other. When a count is off by a few dozen,
# this is the population it can be off by, and it is small enough to read.
# =============================================================================
totalbr_year_edges <- function(year     = 2025,
                               hours    = 3,
                               raw_dir  = here::here("data-raw", "totalbr"),
                               date_col = "dt_dia") {
  counts <- totalbr_day_counts(raw_dir, date_col)
  if (nrow(counts) == 0) return(tibble::tibble())
  # day granularity is enough to show whether the edges are populated at all
  edge_days <- c(sprintf("%d-01-01", year), sprintf("%d-12-31", year),
                 sprintf("%d-12-31", year - 1), sprintf("%d-01-01", year + 1))
  counts |>
    dplyr::filter(DATE %in% edge_days) |>
    dplyr::arrange(SOURCE, DATE) |>
    dplyr::mutate(NOTE = dplyr::case_when(
      DATE == sprintf("%d-01-01", year + 1) ~ "next year: first hours UTC are 31 Dec in Brazil",
      DATE == sprintf("%d-12-31", year - 1) ~ "previous year: last hours UTC are 1 Jan in Brazil",
      TRUE ~ "inside the year"))
}

# =============================================================================
# totalbr_count_by(cols, ...)
#
# Movement counts grouped by YEAR plus the given columns, across every source.
# Used by the profiling tables so they work whether the data is in the parquet
# archive, in downloaded CSVs, or split between both.
#
# Columns absent from a source are skipped for that source rather than failing:
# the archive was produced elsewhere and need not carry exactly the API's
# columns. A source that carries none of them contributes nothing.
# =============================================================================
totalbr_count_by <- function(cols,
                             raw_dir  = here::here("data-raw", "totalbr"),
                             date_col = "dt_dia") {
  src <- totalbr_sources(raw_dir)

  from_parquet <- function(path) {
    ds <- arrow::open_dataset(path)
    if (!all(c(date_col, cols) %in% names(ds))) return(NULL)
    tryCatch({
      q <- dplyr::select(ds, dplyr::all_of(unique(c(date_col, cols))))
      q <- totalbr_add_year_day(q, date_col, totalbr_is_text_date(ds, date_col))
      q |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c("YEAR", cols)))) |>
        dplyr::summarise(MOVEMENTS = dplyr::n(), .groups = "drop") |>
        dplyr::collect()
    }, error = function(e) {
      # a list-typed column (the li_* fields may still be arrays in the archive)
      # cannot be grouped on; say so instead of failing the render
      message("  (cannot group ", paste(cols, collapse = ", "), " in ",
              basename(path), ": ", conditionMessage(e), ")")
      NULL
    })
  }

  from_csv <- function(path) {
    head1 <- data.table::fread(file = path, sep = ";", nrows = 0,
                               showProgress = FALSE)
    if (!all(c(date_col, cols) %in% names(head1))) return(NULL)
    d <- data.table::fread(file = path, sep = ";", colClasses = "character",
                           na.strings = "", showProgress = FALSE,
                           select = c(date_col, cols))
    d <- tibble::as_tibble(d)
    d$YEAR <- substr(d[[date_col]], 1, 4)
    d |> dplyr::count(dplyr::across(dplyr::all_of(c("YEAR", cols))),
                      name = "MOVEMENTS")
  }

  parts <- c(lapply(src$parquet, from_parquet), lapply(src$csv, from_csv))
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0)
    return(tibble::tibble(YEAR = character(0), MOVEMENTS = integer(0)))

  dplyr::bind_rows(parts) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("YEAR", cols)))) |>
    dplyr::summarise(MOVEMENTS = sum(MOVEMENTS), .groups = "drop")
}

# =============================================================================
# totalbr_year_coverage(years, ...)
#
# Per year: the days held (from ANY source), how many the year has, and whether
# that is enough to call it covered. The current year is measured against the
# days elapsed so far, not 365 — otherwise it would never read as covered.
# =============================================================================
totalbr_year_coverage <- function(years    = NULL,
                                  raw_dir  = here::here("data-raw", "totalbr"),
                                  date_col = "dt_dia",
                                  counts   = NULL) {
  if (is.null(counts)) counts <- totalbr_day_counts(raw_dir, date_col)
  today <- Sys.Date()

  have <- if (nrow(counts) == 0) {
    tibble::tibble(YEAR = character(0), DAYS_HELD = integer(0),
                   MOVEMENTS = integer(0), SOURCES = character(0))
  } else {
    counts |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(
        # distinct days, so a year present in BOTH the parquet and a CSV is not
        # counted twice
        DAYS_HELD = dplyr::n_distinct(DATE),
        MOVEMENTS = sum(MOVEMENTS),
        SOURCES   = paste(sort(unique(TYPE)), collapse = " + "),
        .groups   = "drop"
      )
  }

  yrs <- if (is.null(years)) have$YEAR else as.character(sort(unique(years)))
  tibble::tibble(YEAR = yrs) |>
    dplyr::left_join(have, by = "YEAR") |>
    dplyr::mutate(
      DAYS_HELD = tidyr::replace_na(DAYS_HELD, 0L),
      MOVEMENTS = tidyr::replace_na(MOVEMENTS, 0L),
      SOURCES   = tidyr::replace_na(SOURCES, "none"),
      # days the year can possibly have by now: a past year is whole, the
      # current year runs to today, a future year has none
      DAYS_POSSIBLE = as.integer(pmax(0, pmin(
        as.Date(paste0(YEAR, "-12-31")), today
      ) - as.Date(paste0(YEAR, "-01-01")) + 1)),
      # one missing day is tolerated: a genuinely empty day should not force a
      # whole year to be downloaded again
      COVERED = DAYS_HELD >= DAYS_POSSIBLE - 1L & DAYS_POSSIBLE > 0
    )
}

# =============================================================================
# totalbr_missing_years(years, ...)
#
# The years still worth asking the API for. Feed this to download_totalbr() so
# the years already held as a parquet archive are not downloaded all over again.
# =============================================================================
totalbr_missing_years <- function(years,
                                  raw_dir  = here::here("data-raw", "totalbr"),
                                  date_col = "dt_dia",
                                  counts   = NULL) {
  cov <- totalbr_year_coverage(years, raw_dir, date_col, counts)
  as.integer(cov$YEAR[!cov$COVERED & cov$DAYS_POSSIBLE > 0])
}
