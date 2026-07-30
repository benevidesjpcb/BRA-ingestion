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
totalbr_sources <- function(raw_dir = here::here("data-raw", "totalbr")) {
  env_parq <- Sys.getenv("BRA_TOTALBR_PARQUET", unset = "")
  parquet <- if (nzchar(env_parq)) {
    if (file.exists(env_parq) || dir.exists(env_parq)) env_parq else character(0)
  } else if (dir.exists(raw_dir)) {
    list.files(raw_dir, pattern = "\\.parquet$", full.names = TRUE)
  } else character(0)

  csv <- if (dir.exists(raw_dir))
    list.files(raw_dir, pattern = "^totalbr_[0-9]{4}\\.csv$", full.names = TRUE)
  else character(0)

  list(parquet = parquet, csv = csv)
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
  is_text <- grepl("string|utf8",
                   ds$schema$GetFieldByName(date_col)$type$ToString(),
                   ignore.case = TRUE)

  counted <- tryCatch({
    q <- dplyr::select(ds, DT = dplyr::all_of(date_col))
    q <- if (is_text) dplyr::mutate(q, DATE = substr(DT, 1, 10))
         else         dplyr::mutate(q, DATE = as.character(as.Date(DT)))
    q |> dplyr::count(DATE, name = "MOVEMENTS") |> dplyr::collect()
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
      dplyr::select(ds, dplyr::all_of(c(date_col, cols))) |>
        dplyr::mutate(YEAR = substr(as.character(.data[[date_col]]), 1, 4)) |>
        dplyr::count(dplyr::across(dplyr::all_of(c("YEAR", cols))),
                     name = "MOVEMENTS") |>
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
