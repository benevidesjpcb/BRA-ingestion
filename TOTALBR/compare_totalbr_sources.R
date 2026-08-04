#!/usr/bin/env Rscript
# =============================================================================
# compare_totalbr_sources.R
#
# The parquet archive (the "golden", 2023-2025) against the same years downloaded
# from the ODIN API. Three questions, in the order they have to be answered:
#
#   1. WHAT MATCHES AT ALL. Not a formality: if the two sources compute the row
#      hash differently, every row looks unique to its own side and a comparison
#      on pk would report "nothing in common" while the data is in fact the same.
#      So the comparison offers three keys and reports the match rate of each —
#      a low rate is a finding about the key, not about the data.
#
#        "pk"         the row hash, case-folded (the archive writes it uppercase,
#                     the API lowercase)
#        "flight"     callsign + aerodrome pair + dh_inicio to the minute
#        "flight_day" the same, but only to the day — matches across a timezone
#                     shift, which is how a systematic offset is detected
#
#   2. WHAT IS IN ONE AND NOT THE OTHER, with the rows themselves.
#
#   3. WHERE THEY DISAGREE ON ROWS THEY BOTH HAVE, column by column.
#
#   source(here::here("TOTALBR", "compare_totalbr_sources.R"))
#   compare_totalbr_sources(2023:2025)                  # the counts
#   compare_totalbr_examples(2024, "only_parquet")      # rows the API lacks
#   compare_totalbr_examples(2024, "only_csv")          # rows the archive lacks
#   compare_totalbr_fields(2024)                        # where they disagree
#
# Nothing is merged or corrected here. Which source wins is a decision about the
# study, not about the files.
# =============================================================================

source(here::here("TOTALBR", "check_totalbr_duplicates.R"))

# Columns the comparison needs from each source, whichever key is used.
TOTALBR_CMP_COLS <- c("pk", "co_indicativo", "co_addep", "co_addes",
                      "co_matricula", "co_modelo", "dh_inicio", "dh_fim",
                      "dh_eobt")

# ---- the key, built the same way on both sides ------------------------------
totalbr_build_key <- function(d, key) {
  switch(key,
    pk = d$pk,
    # the API and the archive are read into UTC by totalbr_normalise(), so the
    # stamp is formatted in UTC on both sides or the key would encode the
    # session's timezone rather than the flight
    flight = paste(d$co_indicativo, d$co_addep, d$co_addes,
                   format(d$dh_inicio, "%Y-%m-%dT%H:%M", tz = "UTC"),
                   sep = ""),
    flight_day = paste(d$co_indicativo, d$co_addep, d$co_addes,
                       format(d$dh_inicio, "%Y-%m-%d", tz = "UTC"),
                       sep = ""),
    stop("Unknown key '", key, "'.")
  )
}

# ---- read one side, with only what the comparison needs ---------------------
totalbr_cmp_read <- function(paths, years, date_col, all_cols = FALSE) {
  purrr::map(paths, function(path) {
    want <- unique(c(TOTALBR_CMP_COLS, date_col))
    d <- if (grepl("\\.parquet$", path)) {
      ds <- arrow::open_dataset(path)
      if (!all(want %in% names(ds))) {
        message("  ", basename(path), " lacks: ",
                paste(setdiff(want, names(ds)), collapse = ", "))
        return(NULL)
      }
      # SELECT BEFORE FILTERING. li_prnav and li_rvsm are list<int32>, and a
      # filter placed over the whole dataset makes arrow carry them through the
      # query, where they fail with "Unsupported cast from list<item: int32>".
      # Narrowing to the wanted columns first keeps the list columns out of it.
      if (all_cols) {
        # every column is wanted, so the list columns cannot be dropped: pull the
        # data into R first and filter the year there
        d0 <- dplyr::collect(ds)
        d0$YEAR <- format(totalbr_parse_time(d0[[date_col]]), "%Y", tz = "UTC")
        if (!is.null(years)) d0 <- d0[d0$YEAR %in% as.character(years), ]
        d0
      } else {
        q <- dplyr::select(ds, dplyr::all_of(want))
        q <- totalbr_add_year_day(q, date_col, totalbr_is_text_date(ds, date_col))
        # !!yrs, never as.character(years) inline: arrow translates the call
        # itself, and an R vector of length > 1 becomes a list<int32> scalar it
        # then cannot cast to utf8. With a single year it happens to work, which
        # is why this only broke on 2024:2025.
        if (!is.null(years)) {
          yrs <- as.character(years)
          q <- dplyr::filter(q, YEAR %in% !!yrs)
        }
        dplyr::collect(q)
      }
    } else {
      head1 <- data.table::fread(file = path, sep = ";", nrows = 0,
                                 showProgress = FALSE)
      if (!all(want %in% names(head1))) {
        message("  ", basename(path), " lacks: ",
                paste(setdiff(want, names(head1)), collapse = ", "))
        return(NULL)
      }
      x <- data.table::fread(file = path, sep = ";",
                             select = if (all_cols) NULL else want,
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
  }) |> purrr::list_rbind()
}

totalbr_cmp_sides <- function(years, raw_dir, date_col, all_cols = FALSE) {
  src <- totalbr_sources(raw_dir)
  if (length(src$parquet) == 0 || length(src$csv) == 0)
    stop("Need both the parquet archive and the downloaded CSVs in ", raw_dir)
  message("Reading the archive ...")
  a <- totalbr_cmp_read(src$parquet, years, date_col, all_cols)
  message("Reading the API download ...")
  b <- totalbr_cmp_read(src$csv, years, date_col, all_cols)
  if (is.null(a) || nrow(a) == 0) stop("The archive holds no rows for those years.")
  if (is.null(b) || nrow(b) == 0)
    stop("The download holds no rows for those years — fetch them first with ",
         "download_totalbr(", paste(range(years), collapse = ":"), ").")
  list(parquet = a, csv = b)
}

# =============================================================================
# compare_totalbr_sources(years, key, raw_dir, date_col)
#
# Per year: how many rows each source holds, how many match, and how many exist
# on one side only.
#
# Run it with each key before believing any of them. If "pk" matches almost
# nothing while "flight" matches almost everything, the hashes are computed per
# source and pk cannot be used to compare them — that is a fact about the
# identifier, not a difference in the data.
# =============================================================================
compare_totalbr_sources <- function(years    = 2023:2025,
                                    key      = c("pk", "flight", "flight_day"),
                                    raw_dir  = here::here("data-raw", "totalbr"),
                                    date_col = "dt_dia") {
  key   <- match.arg(key)
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)

  a <- sides$parquet; b <- sides$csv
  a$KEY <- totalbr_build_key(a, key)
  b$KEY <- totalbr_build_key(b, key)

  purrr::map(sort(unique(c(a$YEAR, b$YEAR))), function(yr) {
    ka <- a$KEY[a$YEAR == yr]
    kb <- b$KEY[b$YEAR == yr]
    # unique keys, so a duplicate on one side does not inflate the overlap
    ua <- unique(ka); ub <- unique(kb)
    both <- length(intersect(ua, ub))
    tibble::tibble(
      YEAR          = yr,
      KEY           = key,
      PARQUET       = length(ka),
      CSV           = length(kb),
      MATCHED       = both,
      ONLY_PARQUET  = length(setdiff(ua, ub)),
      ONLY_CSV      = length(setdiff(ub, ua)),
      MATCH_PCT     = round(100 * both / max(length(ua), 1), 1)
    )
  }) |> purrr::list_rbind()
}

# =============================================================================
# compare_totalbr_examples(years, side, key, n, ...)
#
# The rows behind the counts: what one source has and the other does not.
#
#   side = "only_parquet"  in the archive, absent from the API download
#   side = "only_csv"      in the download, absent from the archive
#   side = "both"          matched rows, one from each source side by side
# =============================================================================
compare_totalbr_examples <- function(years    = 2023:2025,
                                     side     = c("only_parquet", "only_csv", "both"),
                                     key      = c("pk", "flight", "flight_day"),
                                     n        = 20L,
                                     raw_dir  = here::here("data-raw", "totalbr"),
                                     date_col = "dt_dia") {
  side  <- match.arg(side); key <- match.arg(key)
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv
  a$KEY <- totalbr_build_key(a, key)
  b$KEY <- totalbr_build_key(b, key)

  out <- switch(side,
    only_parquet = a[!a$KEY %in% b$KEY, ],
    only_csv     = b[!b$KEY %in% a$KEY, ],
    both = {
      shared <- head(intersect(a$KEY, b$KEY), n)
      dplyr::bind_rows(a[a$KEY %in% shared, ], b[b$KEY %in% shared, ]) |>
        dplyr::arrange(KEY, SOURCE)
    })

  if (nrow(out) == 0) {
    message("Nothing on the '", side, "' side with key '", key, "'.")
    return(tibble::tibble())
  }
  message(nrow(out), " row(s) on the '", side, "' side; showing ", min(n, nrow(out)))
  head(dplyr::select(out, dplyr::any_of(c("SOURCE", "YEAR", "co_indicativo",
                                          "co_addep", "co_addes", "co_matricula",
                                          "co_modelo", "dh_inicio", "dh_fim",
                                          "dh_eobt", "pk"))),
       if (side == "both") nrow(out) else n)
}

# =============================================================================
# compare_totalbr_fields(years, key, max_rows, ...)
#
# For rows BOTH sources hold: per column, the share of matched rows where the two
# disagree. This is where a difference that no row count can show turns up — the
# same flight carried by both, with a different registration, a different type or
# a stamp minutes apart.
#
# Matching on "flight" by default, because that key is built from fields the two
# sources are expected to agree on; comparing on pk would only compare rows that
# already agree by construction.
# =============================================================================
compare_totalbr_fields <- function(years    = 2023:2025,
                                   key      = c("flight", "pk", "flight_day"),
                                   max_rows = 200000L,
                                   all_cols = FALSE,
                                   raw_dir  = here::here("data-raw", "totalbr"),
                                   date_col = "dt_dia") {
  key   <- match.arg(key)
  # By default only the identity/aircraft/time columns are read. all_cols = TRUE
  # brings in ds_rota and the li_* arrays as well, at the cost of holding every
  # column of both sources in memory — several GB over three years.
  sides <- totalbr_cmp_sides(years, raw_dir, date_col, all_cols = all_cols)
  a <- sides$parquet; b <- sides$csv
  a$KEY <- totalbr_build_key(a, key)
  b$KEY <- totalbr_build_key(b, key)

  # one row per key on each side: a key repeated within a source cannot be
  # compared pairwise without a rule for which copy, so those are dropped and
  # reported rather than silently paired off
  dup_a <- sum(duplicated(a$KEY)); dup_b <- sum(duplicated(b$KEY))
  a <- a[!duplicated(a$KEY), ]; b <- b[!duplicated(b$KEY), ]
  if (dup_a + dup_b > 0)
    message("Dropped ", dup_a, " archive and ", dup_b,
            " download row(s) whose key repeats within their own source.")

  shared <- intersect(a$KEY, b$KEY)
  if (length(shared) == 0) {
    message("No rows matched on key '", key, "'.")
    return(tibble::tibble())
  }
  if (length(shared) > max_rows) {
    message("Comparing a sample of ", max_rows, " of ", length(shared),
            " matched rows.")
    shared <- sample(shared, max_rows)
  }

  a <- a[match(shared, a$KEY), ]; b <- b[match(shared, b$KEY), ]

  cols <- setdiff(intersect(names(a), names(b)),
                  c("KEY", "SOURCE", "YEAR", "pk"))
  purrr::map(cols, function(cl) {
    va <- a[[cl]]; vb <- b[[cl]]
    if (inherits(va, "POSIXct") || inherits(vb, "POSIXct")) {
      va <- format(as.POSIXct(va, tz = "UTC"), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      vb <- format(as.POSIXct(vb, tz = "UTC"), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    }
    va <- trimws(as.character(va)); vb <- trimws(as.character(vb))
    # both empty counts as agreement; one empty is a difference worth seeing, so
    # it is reported on its own rather than folded into DIFFERENT
    empty_a <- is.na(va) | va == ""; empty_b <- is.na(vb) | vb == ""
    tibble::tibble(
      COLUMN         = cl,
      DIFFERENT_PCT  = round(100 * mean(!empty_a & !empty_b & va != vb), 2),
      ONLY_PARQUET_PCT = round(100 * mean(!empty_a & empty_b), 2),
      ONLY_CSV_PCT     = round(100 * mean(empty_a & !empty_b), 2)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(DIFFERENT_PCT))
}

# =============================================================================
# compare_totalbr_diagnose(years, ...)
#
# When the keys match almost nothing but the row counts agree, the data is the
# same traffic and the KEY is wrong. This says which part of it.
#
# It takes the key apart and reports the overlap of each piece on its own, then
# measures the clock difference directly: for every downloaded flight it finds the
# nearest archive record of the same callsign and route, and reports how far apart
# they are. A single dominant offset — 180 minutes, say — is a timezone, not
# missing data, and the fix is to shift one side rather than to re-download.
# =============================================================================
compare_totalbr_diagnose <- function(years    = 2024,
                                     raw_dir  = here::here("data-raw", "totalbr"),
                                     date_col = "dt_dia") {
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv

  pct <- function(x, y) round(100 * length(intersect(x, y)) /
                              max(length(unique(x)), 1), 1)

  parts <- tibble::tibble(
    PIECE = c("co_indicativo", "co_addep", "co_addes",
              "callsign + route", "calendar day of dh_inicio",
              "callsign + route + day"),
    OVERLAP_PCT = c(
      pct(a$co_indicativo, b$co_indicativo),
      pct(a$co_addep, b$co_addep),
      pct(a$co_addes, b$co_addes),
      pct(paste(a$co_indicativo, a$co_addep, a$co_addes),
          paste(b$co_indicativo, b$co_addep, b$co_addes)),
      pct(format(a$dh_inicio, "%Y-%m-%d", tz = "UTC"),
          format(b$dh_inicio, "%Y-%m-%d", tz = "UTC")),
      pct(paste(a$co_indicativo, a$co_addep, a$co_addes,
                format(a$dh_inicio, "%Y-%m-%d", tz = "UTC")),
          paste(b$co_indicativo, b$co_addep, b$co_addes,
                format(b$dh_inicio, "%Y-%m-%d", tz = "UTC")))
    )
  )
  message("Overlap of each part of the key (share of the archive's values ",
          "found in the download):")
  print(as.data.frame(parts))

  # ---- how far apart are the clocks? --------------------------------------
  # A rolling join to the NEAREST record of the same callsign and route: no
  # assumption about the size or the sign of the difference, which is the point —
  # a key built on the time cannot find its own offset.
  A <- data.table::as.data.table(a)[
    !is.na(dh_inicio), list(K = paste(co_indicativo, co_addep, co_addes),
                            TA = dh_inicio)]
  B <- data.table::as.data.table(b)[
    !is.na(dh_inicio), list(K = paste(co_indicativo, co_addep, co_addes),
                            TB = dh_inicio)]
  if (nrow(A) == 0 || nrow(B) == 0) {
    message("No usable dh_inicio on one side."); return(invisible(parts))
  }
  A[, TJ := TA]; B[, TJ := TB]
  data.table::setkey(A, K, TJ)
  data.table::setkey(B, K, TJ)
  joined <- A[B, roll = "nearest", nomatch = 0L]
  if (nrow(joined) == 0) {
    message("No callsign+route in common — the difference is not the clock.")
    return(invisible(parts))
  }
  joined[, DIFF_MIN := as.numeric(difftime(TB, TA, units = "mins"))]

  offsets <- joined[, .N, by = list(OFFSET_MIN = round(DIFF_MIN))][order(-N)]
  message("\nMinutes between a downloaded flight and the nearest archive record ",
          "of the same callsign and route (top 10):")
  print(as.data.frame(head(offsets, 10)))
  message("Exactly 0: ", offsets[OFFSET_MIN == 0, sum(N)], " of ", nrow(joined),
          " (", round(100 * offsets[OFFSET_MIN == 0, sum(N)] / nrow(joined), 1),
          "%)")

  invisible(list(parts = parts, offsets = offsets))
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(arrow); library(data.table); library(here)
  })
  args  <- commandArgs(trailingOnly = TRUE)
  years <- if (length(args) == 0) 2023:2025 else as.integer(args)
  for (k in c("pk", "flight", "flight_day"))
    print(as.data.frame(compare_totalbr_sources(years, k)))
}
