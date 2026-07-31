#!/usr/bin/env Rscript
# =============================================================================
# check_totalbr_duplicates.R
#
# ICEA/DECEA have reported duplication in the ODIN data. This measures it under
# the operational definition, which is deliberately strict — two rows are the
# same movement only when they could not be two different flights:
#
#   1. SAME PK. The pk is the row hash; the same pk twice is the same row twice.
#      No judgement needed.
#
#   2. SAME AIRCRAFT, SAME PLACE, ALMOST THE SAME TIME. The same registration
#      (co_matricula), the same aerodrome pair, and dh_inicio OR dh_fim within a
#      few minutes of each other. One airframe cannot be in one place twice at
#      once, so this is a repeat.
#
# Anything else with a different pk is NOT duplication and is not counted here.
# Two rows sharing a callsign, a route and a day are two flights until the
# registration and the clock say otherwise — an aircraft can fly a route several
# times a day, and a callsign can be reused by a different airframe.
#
#   source(here::here("TOTALBR", "check_totalbr_duplicates.R"))
#   check_totalbr_duplicates(2026)              # the summary
#   totalbr_duplicate_examples(2026)            # the rows themselves
#
# Nothing here repairs anything. Which copy to keep is DECEA's call.
# =============================================================================

source(here::here("TOTALBR", "totalbr_sources.R"))

# How close two times must be to count as the same movement. Minutes.
TOTALBR_NEAR_MIN <- 10

# The columns the check needs. A source missing any of them is skipped, with a
# message, rather than producing a silently partial answer.
TOTALBR_DUP_COLS <- c("pk", "co_matricula", "co_addep", "co_addes",
                      "dh_inicio", "dh_fim", "co_indicativo")

# ---- read the columns the check needs, from either source kind --------------
totalbr_read_dup_cols <- function(path, date_col = "dt_dia", years = NULL) {
  want <- unique(c(TOTALBR_DUP_COLS, date_col))

  d <- if (grepl("\\.parquet$", path)) {
    ds <- arrow::open_dataset(path)
    miss <- setdiff(want, names(ds))
    if (length(miss) > 0) {
      message("  skipping ", basename(path), ": no column(s) ",
              paste(miss, collapse = ", "))
      return(NULL)
    }
    q <- dplyr::select(ds, dplyr::all_of(want))
    q <- totalbr_add_year_day(q, date_col, totalbr_is_text_date(ds, date_col))
    if (!is.null(years))
      q <- dplyr::filter(q, YEAR %in% as.character(years))
    dplyr::collect(q)
  } else {
    head1 <- data.table::fread(file = path, sep = ";", nrows = 0,
                               showProgress = FALSE)
    miss <- setdiff(want, names(head1))
    if (length(miss) > 0) {
      message("  skipping ", basename(path), ": no column(s) ",
              paste(miss, collapse = ", "))
      return(NULL)
    }
    x <- data.table::fread(file = path, sep = ";", select = want,
                           colClasses = "character", na.strings = "",
                           showProgress = FALSE)
    x <- tibble::as_tibble(x)
    x$YEAR <- substr(x[[date_col]], 1, 4)
    if (!is.null(years)) x <- x[x$YEAR %in% as.character(years), ]
    x
  }

  if (is.null(d) || nrow(d) == 0) return(NULL)
  # times as POSIXct whichever way they were stored, and the epoch sentinel
  # (1970-01-01, which ODIN uses in place of an empty value) treated as missing
  # so it cannot masquerade as a movement at the start of time
  for (nm in c("dh_inicio", "dh_fim")) {
    v <- d[[nm]]
    if (!inherits(v, "POSIXct")) v <- as.POSIXct(as.character(v), tz = "UTC")
    v[!is.na(v) & v < as.POSIXct("1980-01-01", tz = "UTC")] <- NA
    d[[nm]] <- v
  }
  d$SOURCE <- basename(path)
  d
}

# A timestamp of exactly 00:00:00 is a date with no time, not a movement at
# midnight. Genuine midnight movements exist but are ~1 in 1440, so treating the
# whole class as "no time known" costs almost nothing and removes an artefact
# that would otherwise dominate every count built on these columns.
totalbr_is_midnight <- function(x) {
  !is.na(x) & format(x, "%H:%M:%S", tz = "UTC") == "00:00:00"
}

# ---- rows that repeat an aircraft in one place within the tolerance ---------
# Sorted by the time column within (registration, aerodrome pair): a row whose
# gap to the previous row of the same group is within the tolerance is a repeat
# of it. Run once per time column, because two rows can be close on dh_fim while
# their dh_inicio are further apart.
totalbr_flag_near <- function(d, tol_min) {
  dt <- data.table::as.data.table(d)
  dt[, ROW_ID := .I]
  dt[, GRP := paste(co_matricula, co_addep, co_addes, sep = "")]

  flagged <- integer(0)
  for (tcol in c("dh_inicio", "dh_fim")) {
    # Rows whose time is exactly midnight carry no time of day: like dt_dia, these
    # columns fall back to the bare date on part of the data. Left in, every such
    # row of one aircraft on one day would sit at the same instant and be flagged
    # as a duplicate of the others — the test would measure the missing clock
    # rather than the traffic. They are untestable by this rule and counted as
    # NO_TIME. Done per column: a row with no dh_inicio can still be judged on
    # dh_fim.
    sub <- dt[!is.na(get(tcol)) & !totalbr_is_midnight(get(tcol)) &
              !is.na(co_matricula) & nzchar(co_matricula)]
    if (nrow(sub) == 0) next
    data.table::setorderv(sub, c("GRP", tcol))
    sub[, GAP := as.numeric(difftime(get(tcol), data.table::shift(get(tcol)),
                                     units = "mins")), by = GRP]
    flagged <- union(flagged, sub[!is.na(GAP) & abs(GAP) <= tol_min, ROW_ID])
  }
  flagged
}

# =============================================================================
# check_totalbr_duplicates(years, tol_min, raw_dir, date_col)
#
# Per year: duplication under both definitions, plus the rows the second test
# COULD NOT be applied to (no registration), so the number is read for what it
# covers rather than assumed to cover everything.
# =============================================================================
check_totalbr_duplicates <- function(years    = NULL,
                                     tol_min  = TOTALBR_NEAR_MIN,
                                     raw_dir  = here::here("data-raw", "totalbr"),
                                     date_col = "dt_dia") {

  src   <- totalbr_sources(raw_dir, include_parts = TRUE)
  # The pk test reads the per-month PARTS as well: the merge de-duplicates on pk,
  # so by the time a year file exists it can no longer show whether the API
  # repeated a row. The near test reads the merged files, which is the data
  # anything downstream would actually use.
  paths <- unique(c(src$parquet, src$csv))

  res <- purrr::map(paths, function(path) {
    message("Reading ", basename(path), " ...")
    d <- totalbr_read_dup_cols(path, date_col, years)
    if (is.null(d)) return(NULL)

    near <- totalbr_flag_near(d, tol_min)
    d$IS_NEAR <- seq_len(nrow(d)) %in% near

    tibble::as_tibble(d) |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(
        ROWS          = dplyr::n(),
        # same pk: the surplus is every copy after the first
        SAME_PK       = sum(duplicated(pk)),
        # same aircraft, same aerodrome pair, times within the tolerance
        NEAR_DUP      = sum(IS_NEAR),
        # rows the near test cannot judge, so the figure above is not mistaken
        # for a statement about the whole year
        NO_REG        = sum(is.na(co_matricula) | !nzchar(co_matricula)),
        # ... and rows with no time of day on EITHER column to compare
        NO_TIME       = sum((is.na(dh_inicio) | totalbr_is_midnight(dh_inicio)) &
                            (is.na(dh_fim)    | totalbr_is_midnight(dh_fim))),
        .groups = "drop"
      )
  }) |> purrr::list_rbind()

  if (is.null(res) || nrow(res) == 0) return(tibble::tibble())

  pk_parts <- if (length(src$parts) > 0) {
    message("Reading the month parts for the pk test ...")
    purrr::map(src$parts, function(f) {
      x <- data.table::fread(file = f, sep = ";", select = c("pk", date_col),
                             colClasses = "character", na.strings = "",
                             showProgress = FALSE)
      tibble::tibble(YEAR = substr(x[[date_col]], 1, 4), pk = x$pk)
    }) |> purrr::list_rbind()
  } else NULL

  # a pk repeated ACROSS two month files is what an overlapping request window
  # produces, so the parts are pooled before the test rather than checked one by one
  parts_pk <- if (!is.null(pk_parts) && nrow(pk_parts) > 0) {
    p <- pk_parts
    if (!is.null(years)) p <- p[p$YEAR %in% as.character(years), ]
    dplyr::group_by(p, YEAR) |>
      dplyr::summarise(SAME_PK_PARTS = sum(duplicated(pk)), .groups = "drop")
  } else NULL

  out <- res |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(dplyr::across(c(ROWS, SAME_PK, NEAR_DUP, NO_REG, NO_TIME),
                                   sum), .groups = "drop")
  if (!is.null(parts_pk)) out <- dplyr::left_join(out, parts_pk, by = "YEAR")

  out |>
    dplyr::mutate(
      NEAR_PCT    = round(100 * NEAR_DUP / ROWS, 3),
      NO_REG_PCT  = round(100 * NO_REG   / ROWS, 1),
      NO_TIME_PCT = round(100 * NO_TIME  / ROWS, 1),
      # the share the near test could actually look at
      TESTED_PCT  = round(100 * (1 - (NO_REG + NO_TIME) / ROWS), 1)
    ) |>
    dplyr::arrange(YEAR)
}

# =============================================================================
# totalbr_duplicate_examples(years, tol_min, n, ...)
#
# The near-duplicate rows themselves, side by side with the row each one repeats,
# and the gap in minutes. This is what to look at before calling anything a
# duplicate — and what to send to ICEA.
# =============================================================================
totalbr_duplicate_examples <- function(years    = NULL,
                                       tol_min  = TOTALBR_NEAR_MIN,
                                       n        = 10L,
                                       raw_dir  = here::here("data-raw", "totalbr"),
                                       date_col = "dt_dia") {
  src   <- totalbr_sources(raw_dir)
  paths <- unique(c(src$csv, src$parquet))   # the downloaded years first

  for (path in paths) {
    d <- totalbr_read_dup_cols(path, date_col, years)
    if (is.null(d)) next
    near <- totalbr_flag_near(d, tol_min)
    if (length(near) == 0) next

    dt <- data.table::as.data.table(d)
    dt[, ROW_ID := .I]
    dt[, GRP := paste(co_matricula, co_addep, co_addes, sep = "")]
    # every row of the groups that contain a flagged row, so the repeat is shown
    # next to what it repeats rather than on its own
    grps <- unique(dt$GRP[near])
    grps <- head(grps, n)
    out  <- dt[GRP %in% grps]
    # the whole group is shown for context, so mark which rows the rule actually
    # caught — the others are there only to be compared against
    out[, FLAGGED := ROW_ID %in% near]
    data.table::setorderv(out, c("GRP", "dh_inicio"))
    out[, GAP_MIN := round(as.numeric(difftime(dh_inicio,
                                               data.table::shift(dh_inicio),
                                               units = "mins")), 1), by = GRP]

    message("Examples from ", basename(path),
            " (same registration, same aerodrome pair, within ", tol_min, " min)")
    return(tibble::as_tibble(out)[, c("co_matricula", "co_indicativo",
                                      "co_addep", "co_addes", "dh_inicio",
                                      "dh_fim", "GAP_MIN", "FLAGGED", "pk")])
  }

  message("No near-duplicates found.")
  tibble::tibble()
}

# =============================================================================
# totalbr_midnight_share(raw_dir, date_col, cols)
#
# Per year and per column: how many rows carry a time of exactly 00:00:00 — that
# is, a date with no time of day.
#
# Not a duplication measure. It is the measure that says how far the duplication
# numbers can be trusted, because the fallback is NOT confined to dt_dia:
# dh_inicio and dh_fim are zeroed on part of the data too. Rows like that all sit
# at the same instant, so any rule comparing times would call them duplicates of
# one another. Read this table before reading a duplication percentage.
# =============================================================================
totalbr_midnight_share <- function(raw_dir  = here::here("data-raw", "totalbr"),
                                   date_col = "dt_dia",
                                   cols     = c("dt_dia", "dh_inicio", "dh_fim")) {
  src <- totalbr_sources(raw_dir)

  one_col <- function(path, col) {
    if (grepl("\\.parquet$", path)) {
      ds <- arrow::open_dataset(path)
      if (!all(c(col, date_col) %in% names(ds))) return(NULL)
      cs <- rlang::sym(col)
      tryCatch({
        totalbr_add_year_day(ds, date_col, totalbr_is_text_date(ds, date_col)) |>
          dplyr::mutate(MIDNIGHT = lubridate::hour(!!cs) == 0 &
                                   lubridate::minute(!!cs) == 0 &
                                   lubridate::second(!!cs) == 0) |>
          dplyr::group_by(YEAR, MIDNIGHT) |>
          dplyr::summarise(N = dplyr::n(), .groups = "drop") |>
          dplyr::collect() |>
          dplyr::mutate(COLUMN = col)
      }, error = function(e) NULL)
    } else {
      head1 <- data.table::fread(file = path, sep = ";", nrows = 0,
                                 showProgress = FALSE)
      if (!all(c(col, date_col) %in% names(head1))) return(NULL)
      x <- data.table::fread(file = path, sep = ";",
                             select = unique(c(col, date_col)),
                             colClasses = "character", na.strings = "",
                             showProgress = FALSE)
      tibble::tibble(
        YEAR = substr(x[[date_col]], 1, 4),
        # a bare date, or a date whose time part is exactly midnight
        MIDNIGHT = is.na(x[[col]]) | substr(x[[col]], 12, 19) %in% c("00:00:00", "")
      ) |>
        dplyr::count(YEAR, MIDNIGHT, name = "N") |>
        dplyr::mutate(COLUMN = col)
    }
  }

  grid  <- expand.grid(path = c(src$parquet, src$csv), col = cols,
                       stringsAsFactors = FALSE)
  parts <- Filter(Negate(is.null),
                  purrr::map2(grid$path, grid$col, one_col))
  if (length(parts) == 0) return(tibble::tibble())

  dplyr::bind_rows(parts) |>
    dplyr::group_by(YEAR, COLUMN) |>
    dplyr::summarise(
      ROWS         = sum(N),
      MIDNIGHT     = sum(N[MIDNIGHT]),
      MIDNIGHT_PCT = round(100 * sum(N[MIDNIGHT]) / sum(N), 2),
      .groups = "drop"
    ) |>
    dplyr::arrange(YEAR, match(COLUMN, cols))
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(arrow); library(data.table); library(here)
  })
  args  <- commandArgs(trailingOnly = TRUE)
  years <- if (length(args) == 0) NULL else args
  print(as.data.frame(check_totalbr_duplicates(years)))
}
