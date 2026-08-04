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
    # the year set is built in R and injected: left as a call inside filter(),
    # arrow turns a multi-year vector into a list<int32> scalar and fails to cast
    if (!is.null(years)) {
      yrs <- as.character(years)
      q <- dplyr::filter(q, YEAR %in% !!yrs)
    }
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
  d <- totalbr_normalise(d)
  d$SOURCE <- basename(path)
  d
}

# Reading a 1 GB archive into memory takes minutes, and a long silence is
# indistinguishable from a hang. Every heavy read reports what it got and how long
# it took, so the wait is visibly progress.
totalbr_report_read <- function(what, n, started) {
  message(sprintf("  %s: %s row(s) in %.0fs", what, format(n, big.mark = ","),
                  as.numeric(difftime(Sys.time(), started, units = "secs"))))
}

# ---- the shape every entry point needs before it can compare rows -----------
totalbr_normalise <- function(d) {
  # The row hash arrives UPPERCASE from the parquet archive and lowercase from the
  # API. Compared as they come, the same row from both sources looks like two
  # different rows and no de-duplication on pk can ever match across them.
  if ("pk" %in% names(d)) d$pk <- toupper(trimws(as.character(d$pk)))
  # times as POSIXct whichever way they were stored, and the epoch sentinel
  # (1970-01-01, which ODIN uses in place of an empty value) treated as missing
  # so it cannot masquerade as a movement at the start of time
  for (nm in intersect(c("dh_inicio", "dh_fim", "dh_eobt", "dh_eet"), names(d))) {
    d[[nm]] <- totalbr_parse_time(d[[nm]])
  }
  d
}

# The CSVs hold ISO 8601 stamps with a T separator (2025-01-01T00:03:00), and
# as.POSIXct's default formats do NOT include that form: it falls through to the
# date-only format and returns MIDNIGHT for every row, silently. That turned every
# downloaded row into "no time known" and made the near-duplicate test pass over
# the whole file without comparing anything. The separator is normalised first.
totalbr_parse_time <- function(v) {
  # Force the display timezone to UTC even when the value is already a timestamp.
  # arrow hands back POSIXct with no tzone set, which prints in the session's
  # local zone, while a value parsed from the CSV prints in UTC. Two such columns
  # side by side in one table show two different clocks with nothing to say so —
  # that is how a +50 minute difference was read off a printout as -10. Setting
  # tzone changes the display only; the instant is untouched.
  if (inherits(v, "POSIXct")) {
    attr(v, "tzone") <- "UTC"
    return(v)
  }
  v <- as.character(v)
  v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
  out <- as.POSIXct(sub("T", " ", v, fixed = TRUE), tz = "UTC")
  # the epoch (1970-01-01) is ODIN's stand-in for an empty value; left as a time
  # it would put thousands of rows at one instant and read as duplication
  out[!is.na(out) & out < as.POSIXct("1980-01-01", tz = "UTC")] <- NA
  out
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
# Which row each flagged row repeats, and by how many minutes. The count only
# needs the flagged row; showing the evidence needs both, so the pair is kept.
totalbr_near_pairs <- function(d, tol_min) {
  dt <- data.table::as.data.table(d)
  dt[, ROW_ID := .I]
  # Exact pk repeats are already counted by the first test, and they trivially
  # satisfy this one — same aircraft, same place, zero minutes apart — so leaving
  # them in makes the near test report the same rows twice and fills the examples
  # with pairs that share a pk instead of the case this rule exists to find:
  # duplication the pk does NOT identify. Only the first copy of each pk is kept.
  dt <- dt[!duplicated(pk)]
  dt[, GRP := paste(co_matricula, co_addep, co_addes, sep = "")]

  flagged <- list()
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
    sub[, PARTNER_ID := data.table::shift(ROW_ID), by = GRP]
    sub[, GAP := as.numeric(difftime(get(tcol), data.table::shift(get(tcol)),
                                     units = "mins")), by = GRP]
    hit <- sub[!is.na(GAP) & abs(GAP) <= tol_min,
               list(ROW_ID, PARTNER_ID, GAP_MIN = round(GAP, 1), TIME_COL = tcol)]
    if (nrow(hit) > 0) flagged[[length(flagged) + 1L]] <- hit
  }
  if (length(flagged) == 0)
    return(data.table::data.table(ROW_ID = integer(), PARTNER_ID = integer(),
                                  GAP_MIN = numeric(), TIME_COL = character()))
  # a pair can be caught on both time columns; keep it once
  unique(data.table::rbindlist(flagged), by = c("ROW_ID", "PARTNER_ID"))
}

# just the flagged row ids, for the counts
totalbr_flag_near <- function(d, tol_min) {
  unique(totalbr_near_pairs(d, tol_min)$ROW_ID)
}

# =============================================================================
# check_totalbr_duplicates(years, tol_min, raw_dir, date_col)
#
# Per year: duplication under both definitions, plus the rows the second test
# COULD NOT be applied to (no registration), so the number is read for what it
# covers rather than assumed to cover everything.
# =============================================================================
check_totalbr_duplicates <- function(years    = NULL,
                                     source   = c("all", "csv", "parquet"),
                                     tol_min  = TOTALBR_NEAR_MIN,
                                     raw_dir  = here::here("data-raw", "totalbr"),
                                     date_col = "dt_dia") {
  source <- match.arg(source)
  src    <- totalbr_sources(raw_dir, include_parts = TRUE)
  # The archive and the API are different producers with different faults, so a
  # figure pooling them describes neither. "csv" is the API's own answer.
  paths <- switch(source,
                  all     = unique(c(src$parquet, src$csv)),
                  csv     = src$csv,
                  parquet = src$parquet)
  if (length(paths) == 0) {
    message("No ", source, " source in ", raw_dir); return(tibble::tibble())
  }
  # The pk test also reads the per-month PARTS, which are the API's: the merge
  # de-duplicates on pk, so by the time a year file exists it can no longer show
  # whether the API repeated a row.
  use_parts <- source %in% c("all", "csv")

  res <- purrr::map(paths, function(path) {
    message("Reading ", basename(path), " ...")
    t0 <- Sys.time()
    d <- totalbr_read_dup_cols(path, date_col, years)
    if (is.null(d)) return(NULL)
    totalbr_report_read(basename(path), nrow(d), t0)
    message("  checking ", basename(path), " ...")

    # the near test runs on the pk-unique rows, so NEAR_DUP is duplication BEYOND
    # what SAME_PK already reports, never the same rows counted a second time
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
        # the two exclusions OVERLAP — a row can lack both — so the untestable
        # total is the union, not the sum. Adding them drove TESTED_PCT negative.
        UNTESTABLE    = sum((is.na(co_matricula) | !nzchar(co_matricula)) |
                            ((is.na(dh_inicio) | totalbr_is_midnight(dh_inicio)) &
                             (is.na(dh_fim)    | totalbr_is_midnight(dh_fim)))),
        .groups = "drop"
      )
  }) |> purrr::list_rbind()

  if (is.null(res) || nrow(res) == 0) return(tibble::tibble())

  pk_parts <- if (use_parts && length(src$parts) > 0) {
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
    dplyr::summarise(dplyr::across(c(ROWS, SAME_PK, NEAR_DUP, NO_REG, NO_TIME,
                                     UNTESTABLE), sum), .groups = "drop")
  if (!is.null(parts_pk)) out <- dplyr::left_join(out, parts_pk, by = "YEAR")

  # A file where almost nothing is testable is usually a reading problem, not a
  # property of the data — that is exactly how the T-separator parse bug showed
  # itself (NO_TIME_PCT of 100). Say so rather than let a zero look like a result.
  if (any(out$NO_TIME / out$ROWS > 0.9))
    message("WARNING: over 90% of rows have no usable time. Check that the ",
            "timestamps are being parsed, not that the data lacks them.")

  out |>
    dplyr::mutate(
      NEAR_PCT    = round(100 * NEAR_DUP / ROWS, 3),
      NO_REG_PCT  = round(100 * NO_REG   / ROWS, 1),
      NO_TIME_PCT = round(100 * NO_TIME  / ROWS, 1),
      # the share the near test could actually look at
      TESTED_PCT  = round(100 * (1 - UNTESTABLE / ROWS), 1)
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
                                       what     = c("near", "pk"),
                                       tol_min  = TOTALBR_NEAR_MIN,
                                       n        = 10L,
                                       raw_dir  = here::here("data-raw", "totalbr"),
                                       date_col = "dt_dia") {
  what <- match.arg(what)
  src   <- totalbr_sources(raw_dir)
  paths <- unique(c(src$csv, src$parquet))   # the downloaded years first

  for (path in paths) {
    d <- totalbr_read_dup_cols(path, date_col, years)
    if (is.null(d)) next
    near <- totalbr_flag_near(d, tol_min)
    if (length(near) == 0) next

    dt <- data.table::as.data.table(d)
    dt[, ROW_ID := .I]

    if (what == "pk") {
      # the exact repeats: every row whose pk occurs more than once
      out <- dt[pk %in% dt[duplicated(pk), unique(pk)]]
      if (nrow(out) == 0) next
      out <- out[pk %in% head(unique(out$pk), n)]
      data.table::setorderv(out, c("pk", "dh_inicio"))
      message("Exact pk repeats in ", basename(path))
      return(tibble::as_tibble(out)[, c("SOURCE", "pk", "co_matricula",
                                        "co_indicativo", "co_addep", "co_addes",
                                        "dh_inicio", "dh_fim")])
    }

    # same rule as the count: the exact pk repeats belong to the other view
    dt <- dt[!duplicated(pk)]
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
    # SOURCE first: the function walks the sources and returns the first that has
    # anything, so without it a result from the parquet archive reads as if it came
    # from the downloaded year — which is exactly how a clean 2026 download looked
    # like it had duplicates that were really the archive's.
    return(tibble::as_tibble(out)[, c("SOURCE", "co_matricula", "co_indicativo",
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
        # narrowed first: a query spanning the list-typed columns fails in arrow
        dplyr::select(ds, dplyr::all_of(unique(c(col, date_col)))) |>
          totalbr_add_year_day(date_col, totalbr_is_text_date(ds, date_col)) |>
          dplyr::mutate(
            # a date with no time at all
            IS_MIDNIGHT = lubridate::hour(!!cs) == 0 &
                          lubridate::minute(!!cs) == 0 &
                          lubridate::second(!!cs) == 0,
            # a time rounded to the hour: 13:00:00 is as unable to separate two
            # movements within that hour as midnight is within the day
            IS_ON_HOUR  = lubridate::minute(!!cs) == 0 &
                          lubridate::second(!!cs) == 0) |>
          dplyr::group_by(YEAR, IS_MIDNIGHT, IS_ON_HOUR) |>
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
        IS_MIDNIGHT = is.na(x[[col]]) |
                      substr(x[[col]], 12, 19) %in% c("00:00:00", ""),
        # minutes and seconds both zero: the stamp resolves to the hour only
        IS_ON_HOUR  = is.na(x[[col]]) | substr(x[[col]], 15, 19) %in% c("00:00", "")
      ) |>
        dplyr::count(YEAR, IS_MIDNIGHT, IS_ON_HOUR, name = "N") |>
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
      ROWS     = sum(N),
      # the logical flags keep their own names: reusing a name inside summarise
      # makes the later expression see the value just created, not the vector
      MIDNIGHT = sum(N[IS_MIDNIGHT]),
      ON_HOUR  = sum(N[IS_ON_HOUR]),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      MIDNIGHT_PCT = round(100 * MIDNIGHT / ROWS, 2),
      ON_HOUR_PCT  = round(100 * ON_HOUR  / ROWS, 2)
    ) |>
    dplyr::arrange(YEAR, match(COLUMN, cols))
}

# =============================================================================
# totalbr_stamp_agreement(raw_dir, date_col)
#
# Per year: how often dt_dia equals dh_inicio, and by how much they differ when
# they do not.
#
# This is the measure that says whether dt_dia is the movement time at all. The
# API payload has the two equal on every sampled row; where they diverge, dt_dia
# is something else — a filing time, a rounded stamp, or a bare date — and any
# key built on it groups movements that are not the same movement.
# =============================================================================
totalbr_stamp_agreement <- function(raw_dir  = here::here("data-raw", "totalbr"),
                                    date_col = "dt_dia") {
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

    to_time <- function(v) if (inherits(v, "POSIXct")) v
                           else as.POSIXct(as.character(v), tz = "UTC")
    a <- to_time(d[[date_col]]); b <- to_time(d$dh_inicio)
    tibble::tibble(
      YEAR = format(a, "%Y"),
      SAME = !is.na(a) & !is.na(b) & a == b,
      DIFF_MIN = abs(as.numeric(difftime(a, b, units = "mins")))
    )
  }

  parts <- Filter(Negate(is.null), lapply(c(src$parquet, src$csv), one))
  if (length(parts) == 0) return(tibble::tibble())

  dplyr::bind_rows(parts) |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(
      ROWS      = dplyr::n(),
      SAME_PCT  = round(100 * mean(SAME, na.rm = TRUE), 1),
      MED_DIFF_MIN = round(stats::median(DIFF_MIN[!SAME], na.rm = TRUE), 1),
      MAX_DIFF_MIN = round(max(DIFF_MIN[!SAME], na.rm = TRUE), 1),
      .groups = "drop"
    )
}

# =============================================================================
# totalbr_source_overlap(raw_dir, date_col)
#
# Do the sources overlap? Per year: the rows each source holds, and how many row
# hashes appear in BOTH.
#
# This is not a hypothetical. The parquet archive labelled 2023-2025 carries rows
# dated 2026-01-01 in the first hours UTC — the evening of 31 December in Brazil,
# so its extraction was cut at a local-time boundary and spilled into the next
# year. Those rows are also in the downloaded 2026 file. Anything summing the two
# sources counts them twice.
#
# The comparison folds case first: the archive writes the hash uppercase and the
# API lowercase, so an unfolded comparison finds no overlap however much there is.
# =============================================================================
totalbr_source_overlap <- function(raw_dir  = here::here("data-raw", "totalbr"),
                                   date_col = "dt_dia") {
  src <- totalbr_sources(raw_dir)
  if (length(src$parquet) == 0 || length(src$csv) == 0) {
    message("Need both a parquet archive and at least one downloaded CSV.")
    return(tibble::tibble())
  }

  keys <- function(path) {
    d <- if (grepl("\\.parquet$", path)) {
      ds <- arrow::open_dataset(path)
      if (!all(c("pk", date_col) %in% names(ds))) return(NULL)
      # narrowed before the mutate, so the list columns never enter the query
      q <- dplyr::select(ds, dplyr::all_of(unique(c("pk", date_col))))
      q <- totalbr_add_year_day(q, date_col, totalbr_is_text_date(ds, date_col))
      dplyr::collect(dplyr::select(q, dplyr::all_of(c("pk", "YEAR"))))
    } else {
      x <- data.table::fread(file = path, sep = ";",
                             select = unique(c("pk", date_col)),
                             colClasses = "character", na.strings = "",
                             showProgress = FALSE)
      tibble::tibble(pk = x$pk, YEAR = substr(x[[date_col]], 1, 4))
    }
    if (is.null(d)) return(NULL)
    d$pk <- toupper(trimws(as.character(d$pk)))
    d$KIND <- if (grepl("\\.parquet$", path)) "parquet" else "csv"
    d
  }

  all <- dplyr::bind_rows(Filter(Negate(is.null),
                                 lapply(c(src$parquet, src$csv), keys)))
  if (nrow(all) == 0) return(tibble::tibble())

  all |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(
      PARQUET = sum(KIND == "parquet"),
      CSV     = sum(KIND == "csv"),
      IN_BOTH = length(intersect(pk[KIND == "parquet"], pk[KIND == "csv"])),
      .groups = "drop"
    ) |>
    dplyr::arrange(YEAR)
}

# =============================================================================
# totalbr_eet_check(raw_dir, date_col)
#
# What dh_eet actually holds, judged by whether it produces a plausible flight.
#
# Read as "estimated arrival", dh_eet - dh_eobt is the estimated elapsed time. In
# the 2026 download that reads correctly: GLO2028 SBGL-SBFL comes out at about 75
# minutes, which is the real block time. In the 2019 sample it does not — the
# difference there equals the EOBT's own time of day, which is not a flight.
#
# So the field cannot be judged from a summary statistic alone: rows where both
# stamps are a bare date give a difference of zero and pile up at the centre,
# which is what made an earlier reading of this column wrong. Those rows are
# excluded here and reported separately.
# =============================================================================
totalbr_eet_check <- function(raw_dir  = here::here("data-raw", "totalbr"),
                              date_col = "dt_dia") {
  src  <- totalbr_sources(raw_dir)
  want <- c("dh_eobt", "dh_eet", date_col)

  one <- function(path) {
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
    to_time <- function(v) if (inherits(v, "POSIXct")) v
                           else as.POSIXct(as.character(v), tz = "UTC")
    eobt <- to_time(d$dh_eobt); eet <- to_time(d$dh_eet)
    tibble::tibble(
      SOURCE  = basename(path),
      YEAR    = substr(as.character(to_time(d[[date_col]])), 1, 4),
      # rows where either stamp is a bare date cannot say anything about a duration
      TIMELESS = totalbr_is_midnight(eobt) | totalbr_is_midnight(eet) |
                 is.na(eobt) | is.na(eet),
      MINUTES = as.numeric(difftime(eet, eobt, units = "mins"))
    )
  }

  parts <- Filter(Negate(is.null), lapply(c(src$parquet, src$csv), one))
  if (length(parts) == 0) return(tibble::tibble())

  dplyr::bind_rows(parts) |>
    dplyr::group_by(SOURCE, YEAR) |>
    dplyr::summarise(
      ROWS         = dplyr::n(),
      TIMELESS_PCT = round(100 * mean(TIMELESS), 1),
      # a plausible flight is minutes long, not negative and not days
      MED_MINUTES  = round(stats::median(MINUTES[!TIMELESS], na.rm = TRUE), 1),
      PLAUSIBLE_PCT = round(100 * mean(MINUTES[!TIMELESS] > 0 &
                                       MINUTES[!TIMELESS] <= 20 * 60,
                                       na.rm = TRUE), 1),
      NEGATIVE_PCT = round(100 * mean(MINUTES[!TIMELESS] < 0, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(SOURCE, YEAR)
}

# =============================================================================
# totalbr_duplicate_rows(years, source, tol_min, raw_dir, date_col, out_file)
#
# THE ROWS THEMSELVES — every record flagged as duplicated, with all its columns,
# restricted to the source you name. Not a sample and not a count: this is the
# list to inspect, or to send to ICEA.
#
#   totalbr_duplicate_rows(2026, "csv")                       # what we downloaded
#   totalbr_duplicate_rows(2026, "csv", out_file = "dup.csv")  # and write it out
#
# `source` matters. The archive and the API are different producers with
# different faults, and mixing them is how a clean download came to look duplicated:
#   "csv"     only the years downloaded from the API   <- the usual choice
#   "parquet" only the handed-over archive
#   "all"     both
#
# Each returned row carries:
#   REASON   "same pk" — the identical row hash twice
#            "near"    — same registration, same aerodrome pair, times within tol_min
#   PAIR     rows that belong together share a number, so a repeat sits next to
#            the row it repeats
#   GAP_MIN  minutes between them (0 for a pk repeat)
#   ROLE     "repeat" for the flagged row, "original" for the one it repeats
# =============================================================================
totalbr_duplicate_rows <- function(years    = NULL,
                                   source   = c("csv", "parquet", "all"),
                                   tol_min  = TOTALBR_NEAR_MIN,
                                   raw_dir  = here::here("data-raw", "totalbr"),
                                   date_col = "dt_dia",
                                   out_file = NULL) {
  source <- match.arg(source)
  src    <- totalbr_sources(raw_dir)
  paths  <- switch(source,
                   csv     = src$csv,
                   parquet = src$parquet,
                   all     = c(src$csv, src$parquet))
  if (length(paths) == 0) {
    message("No ", source, " source in ", raw_dir); return(tibble::tibble())
  }

  out <- purrr::map(paths, function(path) {
    # every column, because the point is to look at the records, not to count them
    d <- if (grepl("\\.parquet$", path)) {
      # every column is wanted here, so the list-typed columns (li_prnav,
      # li_rvsm) cannot be selected away; a year filter pushed down over them
      # fails in arrow, so the data is pulled first and filtered in R
      ds <- arrow::open_dataset(path)
      d0 <- dplyr::collect(ds)
      d0$YEAR <- format(totalbr_parse_time(d0[[date_col]]), "%Y", tz = "UTC")
      if (!is.null(years)) d0 <- d0[d0$YEAR %in% as.character(years), ]
      d0
    } else {
      x <- data.table::fread(file = path, sep = ";", colClasses = "character",
                             na.strings = "", showProgress = FALSE)
      x <- tibble::as_tibble(x)
      x$YEAR <- substr(x[[date_col]], 1, 4)
      if (!is.null(years)) x <- x[x$YEAR %in% as.character(years), ]
      x
    }
    if (nrow(d) == 0) return(NULL)
    d <- totalbr_normalise(d)
    d$SOURCE <- basename(path)

    dt <- data.table::as.data.table(d)
    dt[, ROW_ID := .I]

    # ---- 1. the same pk twice ----------------------------------------------
    rep_pk <- dt$pk[duplicated(dt$pk)]
    pk_rows <- if (length(rep_pk) > 0) {
      x <- dt[pk %in% unique(rep_pk)]
      data.table::setorderv(x, c("pk", "dh_inicio"))
      x[, PAIR := paste0("PK", .GRP), by = pk]
      # the first copy is the original, every later one a repeat
      x[, ROLE := ifelse(seq_len(.N) == 1L, "original", "repeat"), by = pk]
      x[, `:=`(REASON = "same pk", GAP_MIN = 0)]
      x
    } else NULL

    # ---- 2. same aircraft, same place, almost the same time ----------------
    pairs <- totalbr_near_pairs(d, tol_min)
    near_rows <- if (nrow(pairs) > 0) {
      pairs[, PAIR := paste0("NEAR", .I)]
      both <- data.table::rbindlist(list(
        merge(dt, pairs[, list(ROW_ID, PAIR, GAP_MIN)], by = "ROW_ID")[
          , ROLE := "repeat"],
        merge(dt, pairs[, list(ROW_ID = PARTNER_ID, PAIR, GAP_MIN)],
              by = "ROW_ID")[, ROLE := "original"]
      ))
      both[, REASON := "near"]
      both
    } else NULL

    res <- data.table::rbindlist(Filter(Negate(is.null), list(pk_rows, near_rows)),
                                 fill = TRUE)
    if (nrow(res) == 0) return(NULL)
    data.table::setorderv(res, c("PAIR", "ROLE"))
    tibble::as_tibble(res)
  }) |> purrr::list_rbind()

  if (is.null(out) || nrow(out) == 0) {
    message("No duplicated rows found in the ", source, " source",
            if (!is.null(years)) paste0(" for ", paste(years, collapse = ", ")) else "",
            ".")
    return(tibble::tibble())
  }

  out <- dplyr::relocate(out, dplyr::any_of(c("SOURCE", "REASON", "PAIR", "ROLE",
                                              "GAP_MIN")))
  message(nrow(out), " row(s) in ", dplyr::n_distinct(out$PAIR), " pair(s).")

  if (!is.null(out_file)) {
    # semicolon-delimited, like the raw files, so it opens the same way
    utils::write.table(out, out_file, sep = ";", row.names = FALSE,
                       quote = TRUE, na = "", fileEncoding = "UTF-8")
    message("Written to ", out_file)
  }
  out
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
