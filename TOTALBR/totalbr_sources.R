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

# THE ZONE THE STAMPS ARE REALLY IN.
#
# The parquet declares timestamp[us, tz=Europe/Paris], but that label is an
# accident of where the file was written: the values are UTC, confirmed by DECEA.
# The label being wrong is not cosmetic — it decides what the numbers mean:
#
#   The WRITTEN CLOCK is the truth. "2026-01-01 00:59:40" is 2026, and belongs to
#   2026, whatever instant the Paris label implies.
#
#   The stored INSTANT is therefore an hour or two off, and anything comparing
#   instants inherits that error. The archive appeared to stamp dh_inicio 50
#   minutes after the API; with the label removed the real figure is 50 - 60 =
#   -10 minutes, which is what the raw printout showed before it was "corrected".
#
# So the operation is force_tz — KEEP the clock, replace the label — never
# with_tz, which keeps the instant and moves the clock. In arrow, the field's own
# zone already displays the written clock, so year() needs no shift at all.
TOTALBR_TZ <- "UTC"

# The zone a parquet column claims, read from the schema. Bounds for filtering
# have to be built in it: "written clock 2025-01-01" is the instant that reads as
# 2025-01-01 in the stored zone, not in ours.
totalbr_stored_tz <- function(ds, col) {
  ty <- ds$schema$GetFieldByName(col)$type$ToString()
  m  <- regmatches(ty, regexpr("tz=[^]]+", ty))
  if (length(m) == 0) "UTC" else sub("^tz=", "", m)
}

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
  # NO SHIFT. The written clock is the real one, and arrow's year() reads the
  # field in its own zone — which is exactly the written clock. Converting to
  # another zone here would move the reading off the value that was recorded.
  q <- dplyr::mutate(q, YEAR = as.character(lubridate::year(!!s)))
  if (day) q <- dplyr::mutate(q, DAY = as.character(as.Date(!!s)))
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
                               cols        = c("dt_dia", "dh_inicio", "dh_fim"),
                               raw_dir     = here::here("data-raw", "totalbr"),
                               date_col    = "dt_dia") {
  src <- totalbr_sources(raw_dir)

  one <- function(path) {
    want <- unique(c(date_col, cols))
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
    # force_tz: keep the clock that was written, relabel it UTC. attr(tzone) would
    # keep the instant and move the clock — the opposite of what a wrong label needs.
    to_t <- function(v) {
      if (inherits(v, "POSIXct")) return(lubridate::force_tz(v, "UTC"))
      as.POSIXct(sub("T", " ", as.character(v), fixed = TRUE), tz = "UTC")
    }
    off <- tz_offset_h * 3600
    # every requested column, in both zones. dh_fim belongs here as much as the
    # other two: a flight that starts on 31 December ends on 1 January, and which
    # side of the boundary it counts on depends on the column as much as the zone.
    purrr::map(cols, function(cl) {
      v <- to_t(d[[cl]])
      dplyr::bind_rows(
        tibble::tibble(SOURCE = basename(path),
                       DEFINITION = sprintf("%s (UTC)", cl),
                       YEAR = format(v, "%Y", tz = "UTC")),
        tibble::tibble(SOURCE = basename(path),
                       DEFINITION = sprintf("%s (UTC%+d)", cl, tz_offset_h),
                       YEAR = format(v + off, "%Y", tz = "UTC")))
    }) |> purrr::list_rbind()
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
# totalbr_read_year(year, tz, source, cols, raw_dir, date_col)
#
# The rows of one year, filtered correctly. Written because filtering this file
# by hand invites four mistakes, three of them silent:
#
#   case_when() selects VALUES, not rows — filter() selects rows.
#   2025-01-01 without quotes is arithmetic: R reads 2025 - 1 - 1 = 2023, and
#     2025-12-31 = 1982. No error, just a comparison against the wrong number.
#   <= "2025-12-31" keeps only the midnight of the 31st and drops the rest of
#     the day; the upper bound has to be < the 1st of January following.
#   The stored column carries tz=Europe/Paris, so a boundary without a named
#     zone is whatever the session happens to be in.
#
# The zone is an argument because it is a decision, not a detail: counting 2025
# in UTC gives 2,109,856 rows and in America/Sao_Paulo 2,109,631.
# =============================================================================
totalbr_read_year <- function(year     = 2025,
                              tz       = TOTALBR_TZ,
                              source   = c("parquet", "csv"),
                              cols     = NULL,
                              raw_dir  = here::here("data-raw", "totalbr"),
                              date_col = "dt_dia") {
  source <- match.arg(source)
  src    <- totalbr_sources(raw_dir)
  paths  <- if (source == "parquet") src$parquet else src$csv
  if (length(paths) == 0) {
    message("No ", source, " source in ", raw_dir); return(tibble::tibble())
  }
  # half-open interval [1 Jan of the year, 1 Jan of the next): the only form that
  # keeps the whole of 31 December without reaching into the next year.
  #
  # The bounds are built in the zone the COLUMN claims, not in ours. The stamps
  # are UTC values wearing a Europe/Paris label, so the instant that reads as
  # "2025-01-01 00:00" in the stored zone is the one the written clock means. Ask
  # for it in UTC instead and the filter lands an hour off — which is how a row
  # written 2026-01-01 00:59 came back inside 2025.
  bound_tz <- tz
  if (source == "parquet") {
    ds0 <- arrow::open_dataset(paths[1])
    if (date_col %in% names(ds0)) bound_tz <- totalbr_stored_tz(ds0, date_col)
  }
  from <- as.POSIXct(sprintf("%d-01-01", year),     tz = bound_tz)
  to   <- as.POSIXct(sprintf("%d-01-01", year + 1), tz = bound_tz)
  message("Year ", year, " as written-clock [", sprintf("%d-01-01", year), ", ",
          sprintf("%d-01-01", year + 1), ")",
          if (bound_tz != tz) paste0(" (bounds built in the stored zone ",
                                     bound_tz, ")") else "")

  # Show the result in the zone it was FILTERED in. The archive's columns carry
  # tz=Europe/Paris, so a row kept as 2025-12-31 23:59:40 UTC prints as
  # "2026-01-01 00:59:40" and reads like a filter that leaked into the next year.
  # It did not; the clock on screen was simply a different one. Same trap that
  # turned a +50 minute difference into an apparent -10.
  # force_tz on the way out: the written clock is the value, so it is shown
  # unchanged and simply labelled with the zone it really is.
  show_in_tz <- function(d) {
    for (nm in names(d))
      if (inherits(d[[nm]], "POSIXct")) d[[nm]] <- lubridate::force_tz(d[[nm]], tz)
    d
  }

  purrr::map(paths, function(path) {
    if (grepl("\\.parquet$", path)) {
      ds <- arrow::open_dataset(path)
      if (!date_col %in% names(ds)) return(NULL)
      q <- if (is.null(cols)) ds
           else dplyr::select(ds, dplyr::all_of(unique(c(date_col, cols))))
      # !!from / !!to: the bounds are computed in R and injected, never left as
      # calls for arrow to translate
      show_in_tz(
        dplyr::collect(dplyr::filter(q, !!rlang::sym(date_col) >= !!from,
                                        !!rlang::sym(date_col) <  !!to)))
    } else {
      x <- data.table::fread(file = path, sep = ";",
                             select = if (is.null(cols)) NULL
                                      else unique(c(date_col, cols)),
                             colClasses = "character", na.strings = "",
                             showProgress = FALSE)
      x <- tibble::as_tibble(x)
      v <- as.POSIXct(sub("T", " ", x[[date_col]], fixed = TRUE), tz = "UTC")
      x[!is.na(v) & v >= from & v < to, ]   # CSV columns stay text, no zone to set
    }
  }) |> purrr::list_rbind()
}

# =============================================================================
# totalbr_year_span(year, tz, source, raw_dir, date_col)
#
# What a year filter actually kept: the first and last written clock in the
# result, and the row count for each of the first and last three days.
#
# A year that ends at "00:59 on 31 December" is either a filter cutting 23 hours
# off the last day, or a boundary landing an hour into the NEXT year — the two
# look similar in a printout and mean opposite things. Counting the edge days
# separates them: a full last day carries a normal day's traffic, a truncated one
# carries a fraction of it.
# =============================================================================
totalbr_year_span <- function(year     = 2025,
                              tz       = TOTALBR_TZ,
                              source   = c("parquet", "csv"),
                              raw_dir  = here::here("data-raw", "totalbr"),
                              date_col = "dt_dia") {
  source <- match.arg(source)
  d <- totalbr_read_year(year, tz = tz, source = source,
                         cols = c("co_indicativo"), raw_dir = raw_dir,
                         date_col = date_col)
  if (nrow(d) == 0) return(tibble::tibble())

  v <- d[[date_col]]
  if (!inherits(v, "POSIXct"))
    v <- as.POSIXct(sub("T", " ", as.character(v), fixed = TRUE), tz = "UTC")
  day <- format(v, "%Y-%m-%d", tz = tz)

  message("Rows: ", format(nrow(d), big.mark = ","),
          " | first: ", format(min(v, na.rm = TRUE), "%Y-%m-%d %H:%M:%S", tz = tz),
          " | last: ",  format(max(v, na.rm = TRUE), "%Y-%m-%d %H:%M:%S", tz = tz))

  cnt <- tibble::tibble(DAY = day) |> dplyr::count(DAY, name = "ROWS")
  edges <- c(head(sort(unique(day)), 3), tail(sort(unique(day)), 3))
  cnt |>
    dplyr::filter(DAY %in% edges) |>
    dplyr::arrange(DAY) |>
    # a day with a fraction of the year's daily average is a cut, not a quiet day
    dplyr::mutate(VS_DAILY_MEAN = round(ROWS / mean(cnt$ROWS), 2),
                  NOTE = dplyr::if_else(ROWS < 0.5 * mean(cnt$ROWS),
                                        "PARTIAL — the filter cut this day",
                                        "full day"))
}

# =============================================================================
# totalbr_year_boundary_rows(year, tz_offset_h, cols, raw_dir, date_col)
#
# The rows that CHANGE YEAR when the counting zone changes — the flights any
# redefinition moves across the boundary, listed one by one.
#
# A word on what this can and cannot answer. The gap against the published figure
# is a NET: our 2,109,631 against their 2,109,588 is +43, which could be 43 rows
# too many, or 500 too many and 457 too few. Without the official list row by row
# there is no set of "43 flights" to point at, and any list claiming to be one
# would be invented.
#
# What is real and inspectable is this population: rows whose year depends on the
# zone. On a boundary of three hours it is a few hundred flights, small enough to
# read, and the 43 are somewhere inside it.
# =============================================================================
totalbr_year_boundary_rows <- function(year        = 2025,
                                       tz_offset_h = -3,
                                       cols        = c("co_indicativo", "co_addep",
                                                       "co_addes", "co_matricula",
                                                       "pk"),
                                       raw_dir     = here::here("data-raw", "totalbr"),
                                       date_col    = "dt_dia") {
  src <- totalbr_sources(raw_dir)
  out <- purrr::map(c(src$parquet, src$csv), function(path) {
    want <- unique(c(date_col, "dh_inicio", "dh_fim", cols))
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
      if (inherits(v, "POSIXct")) return(lubridate::force_tz(v, "UTC"))
      as.POSIXct(sub("T", " ", as.character(v), fixed = TRUE), tz = "UTC")
    }
    v  <- to_t(d[[date_col]])
    y1 <- format(v, "%Y", tz = "UTC")
    y2 <- format(v + tz_offset_h * 3600, "%Y", tz = "UTC")
    keep <- !is.na(y1) & !is.na(y2) & y1 != y2 &
            (y1 == as.character(year) | y2 == as.character(year))
    if (!any(keep)) return(NULL)
    d <- d[which(keep), ]
    tibble::tibble(
      SOURCE = basename(path),
      YEAR_UTC   = y1[keep],
      YEAR_LOCAL = y2[keep],
      # which way it moves: INTO the year under local time, or OUT of it
      MOVES = ifelse(y2[keep] == as.character(year), "into", "out of"),
      DT_DIA    = format(v[keep], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      dh_inicio = format(to_t(d$dh_inicio), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      dh_fim    = format(to_t(d$dh_fim),    "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>
      dplyr::bind_cols(d[, intersect(cols, names(d)), drop = FALSE])
  }) |> purrr::list_rbind()

  if (is.null(out) || nrow(out) == 0) {
    message("No row changes year between UTC and UTC", sprintf("%+d", tz_offset_h),
            " for ", year, ".")
    return(tibble::tibble())
  }
  message(nrow(out), " row(s) change year: ",
          sum(out$MOVES == "into"), " move into ", year, ", ",
          sum(out$MOVES == "out of"), " move out. Net ",
          sprintf("%+d", sum(out$MOVES == "into") - sum(out$MOVES == "out of")),
          ".")
  dplyr::arrange(out, SOURCE, DT_DIA)
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
