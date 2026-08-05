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
#        "flight_day" callsign + aerodrome pair + the calendar day of dh_inicio
#                     — the working key: the sources stamp dh_inicio about 50
#                     minutes apart, so anything finer than a day cannot match
#        "eobt"       callsign + aerodrome pair + the filed off-block time
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
TOTALBR_CMP_COLS <- c("pk", "id", "co_indicativo", "co_addep", "co_addes",
                      "co_matricula", "co_modelo", "dh_inicio", "dh_fim",
                      "dh_eobt")

# ---- the key, built the same way on both sides ------------------------------
totalbr_build_key <- function(d, key) {
  switch(key,
    pk = d$pk,
    # the API and the archive are read into UTC by totalbr_normalise(), so the
    # stamp is formatted in UTC on both sides or the key would encode the
    # session's timezone rather than the flight
    # There is deliberately no minute-level flight key. It matched 8 rows in a
    # year: the sources stamp dh_inicio ~50 minutes apart, so a key built on the
    # minute can only ever fail. The offset itself is measured by
    # compare_totalbr_diagnose(); as a key it was noise.
    flight_day = paste(d$co_indicativo, d$co_addep, d$co_addes,
                       format(d$dh_inicio, "%Y-%m-%d", tz = "UTC"),
                       sep = ""),
    # dh_eobt is the filed off-block time — a planned value both sources copy from
    # the same flight plan, where dh_inicio is an observed moment each may define
    # differently. When the two disagree on dh_inicio by a constant, this is the
    # key that still lines the rows up.
    eobt = paste(d$co_indicativo, d$co_addep, d$co_addes,
                 format(d$dh_eobt, "%Y-%m-%dT%H:%M", tz = "UTC"),
                 sep = ""),
    # The day of the FILED time. flight_day has a systematic blind spot: the two
    # sources stamp dh_inicio 50 minutes apart, so a flight the download puts at
    # 00:05 sits at 23:15 of the previous day in the archive, and a key built on
    # the calendar day of dh_inicio can never match the first 50 minutes of any
    # day — the rows show as one-sided on BOTH sides while nothing is missing.
    # dh_eobt is the same value in both sources, so its day does not drift.
    eobt_day = paste(d$co_indicativo, d$co_addep, d$co_addes,
                     format(d$dh_eobt, "%Y-%m-%d", tz = "UTC"),
                     sep = ""),
    # PER FLIGHT, and drift-proof. eobt_day is one key for every rotation of a
    # route in a day, so an archive holding three and a download holding two
    # still "match" and the missing flight is never reported. Numbering the
    # rotations within the day — earliest first — matches the first flight to the
    # first, the second to the second, and leaves the surplus one unmatched, which
    # is the thing being looked for.
    #
    # Ordered on dh_eobt because that is the field the two sources agree on: a
    # re-filed off-block time changes the value but rarely the running order.
    eobt_seq = {
      base <- paste(d$co_indicativo, d$co_addep, d$co_addes,
                    format(d$dh_eobt, "%Y-%m-%d", tz = "UTC"), sep = "")
      ord  <- order(base, d$dh_eobt, d$dh_inicio, na.last = TRUE)
      seq_no <- integer(length(base))
      seq_no[ord] <- stats::ave(seq_along(ord), base[ord], FUN = seq_along)
      paste(base, seq_no, sep = "")
    },
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

# Reading the archive takes ~14 minutes. Every comparison function needs the same
# two sides, so they are kept for the session instead of being re-read per call.
# refresh = TRUE forces a re-read after the files change.
.totalbr_cmp_cache <- new.env(parent = emptyenv())

totalbr_cmp_sides <- function(years, raw_dir, date_col, all_cols = FALSE,
                              refresh = FALSE) {
  # the column set is part of the cache key: adding a column to TOTALBR_CMP_COLS
  # must not be served from a cache read before it existed
  ck <- paste(paste(years, collapse = ","), raw_dir, date_col, all_cols,
              paste(TOTALBR_CMP_COLS, collapse = ","))
  if (!refresh && !is.null(.totalbr_cmp_cache[[ck]])) {
    message("(reusing the sides read earlier this session; refresh = TRUE to re-read)")
    return(.totalbr_cmp_cache[[ck]])
  }
  src <- totalbr_sources(raw_dir)
  if (length(src$parquet) == 0 || length(src$csv) == 0)
    stop("Need both the parquet archive and the downloaded CSVs in ", raw_dir)
  # both sides are hundreds of MB; announce each and report what came back, so a
  # long read is visibly progress rather than a possible hang
  message("Reading the archive ...")
  t0 <- Sys.time()
  a <- totalbr_cmp_read(src$parquet, years, date_col, all_cols)
  totalbr_report_read("archive", if (is.null(a)) 0L else nrow(a), t0)
  message("Reading the API download ...")
  t1 <- Sys.time()
  b <- totalbr_cmp_read(src$csv, years, date_col, all_cols)
  totalbr_report_read("download", if (is.null(b)) 0L else nrow(b), t1)
  if (is.null(a) || nrow(a) == 0) stop("The archive holds no rows for those years.")
  if (is.null(b) || nrow(b) == 0)
    stop("The download holds no rows for those years — fetch them first with ",
         "download_totalbr(", paste(range(years), collapse = ":"), ").")
  res <- list(parquet = a, csv = b)
  .totalbr_cmp_cache[[ck]] <- res
  res
}

# =============================================================================
# compare_totalbr_sources(years, key, raw_dir, date_col)
#
# Per year: how many rows each source holds, how many match, and how many exist
# on one side only.
#
# Run it with each key before believing any of them. If "pk" matches almost
# nothing while "flight_day" matches almost everything, the hashes are computed
# per source and pk cannot be used to compare them — that is a fact about the
# identifier, not a difference in the data.
# =============================================================================
compare_totalbr_sources <- function(years    = 2023:2025,
                                    key      = c("eobt_seq", "eobt_day", "flight_day", "eobt", "pk"),
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
    # unique keys, so a key held twice on one side does not inflate the overlap
    ua <- unique(ka); ub <- unique(kb)
    both <- length(intersect(ua, ub))
    only_a <- setdiff(ua, ub); only_b <- setdiff(ub, ua)
    tibble::tibble(
      YEAR          = yr,
      KEY           = key,
      PARQUET       = length(ka),
      CSV           = length(kb),
      MATCHED       = both,
      ONLY_PARQUET  = length(only_a),
      ONLY_CSV      = length(only_b),
      # ROWS_* count records, KEYS above count distinct key values. With a coarse
      # key one value covers several rows, so the two differ — and reporting only
      # one of them made this table disagree with totalbr_unmatched(), which
      # returns rows. Both are here so neither number can be read as the other.
      ROWS_ONLY_PARQUET = sum(ka %in% only_a),
      ROWS_ONLY_CSV     = sum(kb %in% only_b),
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
                                     key      = c("eobt_seq", "eobt_day", "flight_day", "eobt", "pk"),
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
# Matching on "flight_day" by default: it is the key the two sources actually
# agree on. Comparing on pk would only compare rows that agree by construction.
# =============================================================================
compare_totalbr_fields <- function(years    = 2023:2025,
                                   key      = c("eobt_seq", "eobt_day", "flight_day", "eobt", "pk"),
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
          "of the same callsign and route, on dh_inicio (top 10):")
  print(as.data.frame(head(offsets, 10)))
  message("Exactly 0: ", offsets[OFFSET_MIN == 0, sum(N)], " of ", nrow(joined),
          " (", round(100 * offsets[OFFSET_MIN == 0, sum(N)] / nrow(joined), 1),
          "%)")

  # The same measurement on the FILED time. dh_inicio is observed and each source
  # may define it differently; dh_eobt is copied from the flight plan, so if the
  # two agree there the rows can still be lined up and compared.
  offsets_eobt <- NULL
  if (all(c("dh_eobt") %in% names(a)) && all(c("dh_eobt") %in% names(b))) {
    A2 <- data.table::as.data.table(a)[
      !is.na(dh_eobt), list(K = paste(co_indicativo, co_addep, co_addes),
                            TA = dh_eobt)]
    B2 <- data.table::as.data.table(b)[
      !is.na(dh_eobt), list(K = paste(co_indicativo, co_addep, co_addes),
                            TB = dh_eobt)]
    if (nrow(A2) > 0 && nrow(B2) > 0) {
      A2[, TJ := TA]; B2[, TJ := TB]
      data.table::setkey(A2, K, TJ); data.table::setkey(B2, K, TJ)
      j2 <- A2[B2, roll = "nearest", nomatch = 0L]
      if (nrow(j2) > 0) {
        j2[, DIFF_MIN := as.numeric(difftime(TB, TA, units = "mins"))]
        offsets_eobt <- j2[, .N, by = list(OFFSET_MIN = round(DIFF_MIN))][order(-N)]
        message("\nThe same, on dh_eobt (top 10):")
        print(as.data.frame(head(offsets_eobt, 10)))
        message("Exactly 0: ", offsets_eobt[OFFSET_MIN == 0, sum(N)], " of ",
                nrow(j2), " (",
                round(100 * offsets_eobt[OFFSET_MIN == 0, sum(N)] / nrow(j2), 1),
                "%)")
        if (offsets_eobt[OFFSET_MIN == 0, sum(N)] / nrow(j2) > 0.5)
          message("\n=> dh_eobt agrees. Compare with key = \"eobt\": the sources ",
                  "differ on what dh_inicio means, not on which flights they hold.")
      }
    }
  }

  invisible(list(parts = parts, offsets = offsets, offsets_eobt = offsets_eobt))
}

# =============================================================================
# compare_totalbr_missing_values(column, years, n, ...)
#
# The values of one column that exist in one source and not the other — with how
# many ROWS each accounts for.
#
# Both numbers are needed and they say different things. A column can have 10% of
# its distinct values missing while those values carry 0.3% of the traffic: in
# TOTALBR most distinct callsigns are general-aviation registrations flown a
# handful of times a year, so a share of distinct values is not a share of
# flights. The summary reports both; the list is sorted by rows, so whatever
# actually matters is at the top.
#
#   compare_totalbr_missing_values("co_indicativo", 2024)
#   compare_totalbr_missing_values("co_addes", 2024, n = 100)
# =============================================================================
compare_totalbr_missing_values <- function(column   = "co_indicativo",
                                           years    = 2024,
                                           n        = 50L,
                                           raw_dir  = here::here("data-raw", "totalbr"),
                                           date_col = "dt_dia") {
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv
  if (!column %in% names(a) || !column %in% names(b))
    stop("Column '", column, "' is not in both sources.")

  va <- as.character(a[[column]]); vb <- as.character(b[[column]])
  ca <- table(va[!is.na(va) & nzchar(va)])
  cb <- table(vb[!is.na(vb) & nzchar(vb)])

  only_a <- setdiff(names(ca), names(cb))
  only_b <- setdiff(names(cb), names(ca))

  summary <- tibble::tibble(
    SIDE          = c("only in archive", "only in download"),
    VALUES        = c(length(only_a), length(only_b)),
    VALUES_PCT    = c(round(100 * length(only_a) / length(ca), 1),
                      round(100 * length(only_b) / length(cb), 1)),
    ROWS          = c(sum(ca[only_a]), sum(cb[only_b])),
    # the number that says whether this matters: a tenth of the values can be a
    # three-hundredth of the traffic
    ROWS_PCT      = c(round(100 * sum(ca[only_a]) / sum(ca), 2),
                      round(100 * sum(cb[only_b]) / sum(cb), 2))
  )
  message("Values of ", column, " present in one source only:")
  print(as.data.frame(summary))

  lst <- dplyr::bind_rows(
    tibble::tibble(SIDE = "only in archive",  VALUE = only_a,
                   N_ROWS = as.integer(ca[only_a])),
    tibble::tibble(SIDE = "only in download", VALUE = only_b,
                   N_ROWS = as.integer(cb[only_b]))
  ) |>
    dplyr::arrange(dplyr::desc(N_ROWS))

  message("\nTop ", min(n, nrow(lst)), " by rows:")
  print(as.data.frame(head(lst, n)))
  invisible(list(summary = summary, values = lst))
}

# =============================================================================
# compare_totalbr_pk_examples(years, n, ...)
#
# Zero pk matches between two sources holding 93% of the same flights is either a
# fact about the hash or a bug in this code, and a count cannot tell them apart.
# This lines up the SAME flight from both sides and prints the two pk values.
#
#   two different 32-character hex strings  -> the hash is computed per source
#                                              (different salt, or over different
#                                              fields). Nothing to fix; pk is not
#                                              a cross-source identifier.
#   one truncated, padded, quoted, or empty -> a reading fault, and ours to fix.
#
# It also reports the length and alphabet of pk on each side, which catches a
# truncation or an encoding difference without reading a single example.
# =============================================================================
compare_totalbr_pk_examples <- function(years    = 2024,
                                        n        = 10L,
                                        raw_dir  = here::here("data-raw", "totalbr"),
                                        date_col = "dt_dia") {
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv

  shape <- function(v, label) {
    v <- v[!is.na(v) & nzchar(v)]
    tibble::tibble(
      SIDE      = label,
      N         = length(v),
      NCHAR_MIN = min(nchar(v)), NCHAR_MAX = max(nchar(v)),
      # a hash should be hex and nothing else; anything else is a formatting fault
      ALL_HEX   = all(grepl("^[0-9A-F]+$", head(v, 100000))),
      EXAMPLE   = v[1]
    )
  }
  message("Shape of pk on each side:")
  print(as.data.frame(dplyr::bind_rows(shape(a$pk, "archive"),
                                       shape(b$pk, "download"))))

  # the same flight on both sides, via the key they do agree on
  a$KEY <- totalbr_build_key(a, "flight_day")
  b$KEY <- totalbr_build_key(b, "flight_day")
  a2 <- a[!duplicated(a$KEY), ]; b2 <- b[!duplicated(b$KEY), ]
  shared <- intersect(a2$KEY, b2$KEY)
  if (length(shared) == 0) {
    message("No flight matched on flight_day, so no pair can be shown.")
    return(invisible(NULL))
  }
  shared <- head(shared, n)
  ia <- match(shared, a2$KEY); ib <- match(shared, b2$KEY)

  out <- tibble::tibble(
    co_indicativo = a2$co_indicativo[ia],
    co_addep      = a2$co_addep[ia],
    co_addes      = a2$co_addes[ia],
    dh_inicio_archive  = a2$dh_inicio[ia],
    dh_inicio_download = b2$dh_inicio[ib],
    pk_archive    = a2$pk[ia],
    pk_download   = b2$pk[ib]
  )
  message("\nThe same flight, both pk values side by side:")
  out
}

# =============================================================================
# compare_totalbr_gap_profile(years, side, ...)
#
# The rows that exist on one side only, summarised instead of listed — because
# 124,898 rows are not read one by one, and the question is whether they form a
# pattern.
#
#   concentrated in a month      -> the extraction stopped, or the download did
#   concentrated at an aerodrome -> a filter difference between the sources
#   spread evenly, tiny per day  -> late-arriving records, ordinary
# =============================================================================
compare_totalbr_gap_profile <- function(years    = 2024,
                                        side     = c("only_parquet", "only_csv"),
                                        raw_dir  = here::here("data-raw", "totalbr"),
                                        date_col = "dt_dia") {
  side  <- match.arg(side)
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv
  a$KEY <- totalbr_build_key(a, "flight_day")
  b$KEY <- totalbr_build_key(b, "flight_day")

  gap <- if (side == "only_parquet") a[!a$KEY %in% b$KEY, ] else b[!b$KEY %in% a$KEY, ]
  ref <- if (side == "only_parquet") a else b
  if (nrow(gap) == 0) {
    message("Nothing on the '", side, "' side."); return(invisible(NULL))
  }
  message(nrow(gap), " row(s) on the '", side, "' side (",
          round(100 * nrow(gap) / nrow(ref), 1), "% of that source).")

  by_month <- tibble::tibble(
    MONTH = format(gap$dh_inicio, "%Y-%m", tz = "UTC")) |>
    dplyr::count(MONTH, name = "GAP_ROWS") |>
    dplyr::left_join(
      tibble::tibble(MONTH = format(ref$dh_inicio, "%Y-%m", tz = "UTC")) |>
        dplyr::count(MONTH, name = "SOURCE_ROWS"),
      by = "MONTH") |>
    dplyr::mutate(PCT_OF_MONTH = round(100 * GAP_ROWS / SOURCE_ROWS, 1))
  message("\nBy month (PCT_OF_MONTH is what matters — an even spread is ordinary):")
  print(as.data.frame(by_month))

  # Hour of day. The two sources stamp dh_inicio ~10 minutes apart, so a flight
  # near midnight falls on different calendar days on each side and flight_day
  # cannot match it — on EITHER side. That artefact would show as a spike in the
  # first and last hours; anything flat is a real difference.
  by_hour <- tibble::tibble(
    HOUR = format(gap$dh_inicio, "%H", tz = "UTC")) |>
    dplyr::count(HOUR, name = "GAP_ROWS") |>
    dplyr::mutate(PCT_OF_GAP = round(100 * GAP_ROWS / sum(GAP_ROWS), 1))
  message("\nBy hour of day (a spike at 00 and 23 is the midnight artefact of the ",
          "10-minute shift, not a real gap):")
  print(as.data.frame(by_hour))

  # Callsign shape. A Brazilian registration used as a callsign (PP/PR/PS/PT/PU
  # plus three letters) is general aviation; three letters plus digits is an
  # airline. Which class is missing says whether this is a filter or a gap.
  is_reg <- grepl("^P[PRSTU][A-Z]{3}$", gap$co_indicativo)
  is_air <- grepl("^[A-Z]{3}[0-9]", gap$co_indicativo)
  message("\nWhat kind of traffic is one-sided:")
  print(as.data.frame(tibble::tibble(
    CLASS = c("registration (general aviation)", "airline callsign", "other"),
    ROWS  = c(sum(is_reg), sum(is_air), sum(!is_reg & !is_air)),
    PCT   = round(100 * c(sum(is_reg), sum(is_air),
                          sum(!is_reg & !is_air)) / nrow(gap), 1))))

  top <- function(col, label) {
    tibble::tibble(VALUE = as.character(gap[[col]])) |>
      dplyr::count(VALUE, name = "GAP_ROWS") |>
      dplyr::arrange(dplyr::desc(GAP_ROWS)) |>
      head(10) |>
      dplyr::mutate(FIELD = label)
  }
  message("\nMost frequent values among the one-sided rows:")
  print(as.data.frame(dplyr::bind_rows(top("co_addep", "co_addep"),
                                       top("co_addes", "co_addes"),
                                       top("co_indicativo", "co_indicativo"))))

  invisible(list(rows = gap, by_month = by_month, by_hour = by_hour))
}

# =============================================================================
# compare_totalbr_time_shift(years, ...)
#
# How far apart the two sources put the same flight — measured on flights they
# BOTH hold, matched on flight_day, one column at a time.
#
# This supersedes the rolling-join estimate in compare_totalbr_diagnose(). That
# one looks for the nearest record of the same callsign and route, so when a
# flight is missing from one side it silently pairs a different flight and the
# modal "offset" describes those mismatches rather than the shift. Matching first
# and differencing second cannot do that.
#
# Reading the three time columns together is the point: if dh_inicio, dh_fim and
# dh_eobt are ALL shifted by the same amount, the sources disagree about the
# clock. If only dh_inicio moves, they disagree about what dh_inicio means, and
# that is a definition difference no shift should paper over.
# =============================================================================
compare_totalbr_time_shift <- function(years    = 2024,
                                       raw_dir  = here::here("data-raw", "totalbr"),
                                       date_col = "dt_dia") {
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv
  a$KEY <- totalbr_build_key(a, "flight_day")
  b$KEY <- totalbr_build_key(b, "flight_day")

  # one row per key per side: a key that repeats within a source has no single
  # counterpart, and pairing it arbitrarily would invent a difference
  a <- a[!duplicated(a$KEY), ]; b <- b[!duplicated(b$KEY), ]
  shared <- intersect(a$KEY, b$KEY)
  if (length(shared) == 0) {
    message("No flight matched on flight_day."); return(tibble::tibble())
  }
  ia <- match(shared, a$KEY); ib <- match(shared, b$KEY)
  message(length(shared), " matched flight(s).")

  purrr::map(c("dh_inicio", "dh_fim", "dh_eobt"), function(cl) {
    if (!cl %in% names(a) || !cl %in% names(b)) return(NULL)
    # download minus archive, in minutes: negative means the download stamps the
    # flight EARLIER than the archive does
    d <- as.numeric(difftime(b[[cl]][ib], a[[cl]][ia], units = "mins"))
    d <- d[!is.na(d)]
    if (length(d) == 0) return(NULL)
    tab <- sort(table(round(d)), decreasing = TRUE)
    tibble::tibble(
      COLUMN       = cl,
      N            = length(d),
      MODE_MIN     = as.numeric(names(tab)[1]),
      MODE_PCT     = round(100 * tab[[1]] / length(d), 1),
      MEDIAN_MIN   = round(stats::median(d), 1),
      IDENTICAL_PCT = round(100 * mean(d == 0), 1)
    )
  }) |> purrr::list_rbind()
}

# =============================================================================
# totalbr_unmatched(years, key, out_file, ...)
#
# Every row that exists in ONE source and not the other, as a dataset to open and
# search — not a summary. One row per record, carrying the fields needed to go
# and look the flight up in the other file by hand:
#
#   YEAR, SOURCE, co_indicativo, co_addep, co_addes, dt_dia, dh_inicio,
#   co_matricula, pk, id, MATCH_KEY
#
# SOURCE says which file the row came from ("parquet" = the archive, "csv" = the
# API download). MATCH_KEY is the exact string the comparison used, and it is in
# the output on purpose: it is what decides whether two rows are "the same
# flight", so it should be checkable rather than taken on trust. Copy a
# MATCH_KEY, look for it in the other source, and either it is genuinely absent
# or the key is wrong — both are worth knowing.
#
#   totalbr_unmatched(2024)                                  # in memory
#   totalbr_unmatched(2024, out_file = "unmatched-2024.csv")  # written out
# =============================================================================
totalbr_unmatched <- function(years    = 2024,
                              key      = c("eobt_seq", "eobt_day", "flight_day", "eobt", "pk"),
                              out_file = NULL,
                              raw_dir  = here::here("data-raw", "totalbr"),
                              date_col = "dt_dia") {
  key   <- match.arg(key)
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv
  a$MATCH_KEY <- totalbr_build_key(a, key)
  b$MATCH_KEY <- totalbr_build_key(b, key)

  # every row whose key is absent from the other source. Rows, not keys: a key
  # held twice on one side contributes both of its rows, because both are things
  # you would go looking for.
  only_a <- a[!a$MATCH_KEY %in% b$MATCH_KEY, ]
  only_b <- b[!b$MATCH_KEY %in% a$MATCH_KEY, ]

  take <- function(d, src) {
    if (nrow(d) == 0) return(NULL)
    tibble::tibble(
      YEAR          = d$YEAR,
      SOURCE        = src,
      co_indicativo = d$co_indicativo,
      co_addep      = d$co_addep,
      co_addes      = d$co_addes,
      # the date column as text, in UTC, so it reads the same in the file as it
      # does in the comparison
      dt_dia        = format(totalbr_parse_time(d[[date_col]]),
                             "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      dh_inicio     = format(d$dh_inicio, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      co_matricula  = d$co_matricula,
      pk            = d$pk,
      id            = if ("id" %in% names(d)) d$id else NA_character_,
      MATCH_KEY     = d$MATCH_KEY
    )
  }

  out <- dplyr::bind_rows(take(only_a, "parquet"), take(only_b, "csv"))
  if (is.null(out) || nrow(out) == 0) {
    message("Every row matched on key '", key, "'.")
    return(tibble::tibble())
  }
  out <- dplyr::arrange(out, YEAR, SOURCE, dt_dia)

  message(nrow(out), " unmatched row(s) on key '", key, "': ",
          sum(out$SOURCE == "parquet"), " only in the archive, ",
          sum(out$SOURCE == "csv"), " only in the download.")

  if (!is.null(out_file)) {
    # semicolon-delimited UTF-8, like the raw files, so it opens the same way
    utils::write.table(out, out_file, sep = ";", row.names = FALSE,
                       quote = TRUE, na = "", fileEncoding = "UTF-8")
    message("Written to ", out_file)
  }
  out
}

# ---- greedy one-to-one nearest pairing, given a group and a time ------------
# Shared by the matcher and by the leftover decomposition, so both use exactly
# the same rule and their numbers can be added up.
totalbr_pair_nearest <- function(ka, ta, kb, tb, tol_min) {
  A <- data.table::data.table(A_ID = seq_along(ka), K = ka, TA = ta)[!is.na(TA)]
  B <- data.table::data.table(B_ID = seq_along(kb), K = kb, TB = tb)[!is.na(TB)]
  if (nrow(A) == 0 || nrow(B) == 0)
    return(data.table::data.table(A_ID = integer(), B_ID = integer(),
                                  DIFF_MIN = numeric()))
  A[, TJ := TA]; B[, TJ := TB]
  data.table::setkey(A, K, TJ); data.table::setkey(B, K, TJ)
  m <- A[B, roll = "nearest", nomatch = 0L]
  if (nrow(m) == 0)
    return(data.table::data.table(A_ID = integer(), B_ID = integer(),
                                  DIFF_MIN = numeric()))
  m[, DIFF_MIN := abs(as.numeric(difftime(TB, TA, units = "mins")))]
  m <- m[DIFF_MIN <= tol_min]
  # one to one, closest first on each side in turn
  data.table::setorder(m, A_ID, DIFF_MIN); m <- m[!duplicated(A_ID)]
  data.table::setorder(m, B_ID, DIFF_MIN); m <- m[!duplicated(B_ID)]
  m[, list(A_ID, B_ID, DIFF_MIN)]
}

# =============================================================================
# totalbr_leftovers_explained(years, tol_min, shift_min, ...)
#
# The rows left over by totalbr_match_flights(), decomposed.
#
# Those leftovers came back SYMMETRIC — 191,156 and 190,006 for 2025, differing
# by exactly the 1,150 rows the two files differ by overall. Symmetry is the
# signature of a field disagreeing rather than a flight missing: the pairing
# groups on callsign + departure + destination, so if any one of the three
# differs, the same flight is left over on BOTH sides at once.
#
# Each pass takes what is still unpaired and relaxes one part of the group:
#
#   route only     callsign ignored  -> the callsign differs between sources
#   callsign only  route ignored     -> an aerodrome differs
#   wide window    same group, 24h   -> only the timing differs
#   residual       nothing recovers  -> a genuinely one-sided flight
#
# The residual is the answer to "what is really in one and not the other".
# =============================================================================
totalbr_leftovers_explained <- function(years     = 2025,
                                        tol_min   = 120,
                                        shift_min = 50,
                                        raw_dir   = here::here("data-raw", "totalbr"),
                                        date_col  = "dt_dia") {
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv
  ta <- a$dh_inicio + shift_min * 60
  tb <- b$dh_inicio

  ia <- seq_len(nrow(a)); ib <- seq_len(nrow(b))
  steps <- list()

  run <- function(label, ka, kb, tol) {
    m <- totalbr_pair_nearest(ka[ia], ta[ia], kb[ib], tb[ib], tol)
    # the pairing works on positions within the current leftovers, so map back
    got_a <- ia[m$A_ID]; got_b <- ib[m$B_ID]
    steps[[length(steps) + 1L]] <<- tibble::tibble(
      PASS = label, PAIRED = nrow(m),
      LEFT_ARCHIVE = length(ia) - nrow(m),
      LEFT_DOWNLOAD = length(ib) - nrow(m))
    ia <<- setdiff(ia, got_a); ib <<- setdiff(ib, got_b)
  }

  message("Rows with no dh_inicio, which no pass can ever pair: archive ",
          sum(is.na(ta)), ", download ", sum(is.na(tb)))

  key3 <- function(d) paste(d$co_indicativo, d$co_addep, d$co_addes, sep = "")
  key_route <- function(d) paste(d$co_addep, d$co_addes, sep = "")
  key_call  <- function(d) d$co_indicativo

  run("callsign + route",        key3(a),      key3(b),      tol_min)
  run("route only (callsign differs)", key_route(a), key_route(b), tol_min)
  run("callsign only (aerodrome differs)", key_call(a), key_call(b), tol_min)
  run("callsign + route, 24h window", key3(a), key3(b), 24 * 60)

  out <- purrr::list_rbind(steps)
  message("Rows: archive ", nrow(a), ", download ", nrow(b))
  print(as.data.frame(out))

  res_a <- a[ia, ]; res_b <- b[ib, ]

  # A row with no usable time cannot be paired at all: the join drops it, so it
  # survives every pass and looks like "nothing recovers it" when in fact nothing
  # ever tried. Reported first, because it changes what the residual means.
  cannot <- function(d, label) {
    if (nrow(d) == 0) return(NULL)
    no_time <- is.na(d$dh_inicio)
    no_call <- is.na(d$co_indicativo) | !nzchar(d$co_indicativo)
    tibble::tibble(SIDE = label, RESIDUAL = nrow(d),
                   NO_TIME = sum(no_time), NO_CALLSIGN = sum(no_call),
                   UNTESTABLE_PCT = round(100 * mean(no_time | no_call), 1))
  }
  message("\nOf the residual, how much could never be paired in the first place:")
  print(as.data.frame(dplyr::bind_rows(cannot(res_a, "only archive"),
                                       cannot(res_b, "only download"))))

  by_month <- function(d, label) {
    if (nrow(d) == 0) return(NULL)
    tibble::tibble(SIDE = label,
                   MONTH = format(d$dh_inicio, "%Y-%m", tz = "UTC")) |>
      dplyr::count(SIDE, MONTH, name = "ROWS")
  }
  message("\nResidual by month (a spike means an extraction boundary, not traffic):")
  print(as.data.frame(dplyr::bind_rows(by_month(res_a, "only archive"),
                                       by_month(res_b, "only download"))))

  cls <- function(d, label) {
    if (nrow(d) == 0) return(NULL)
    reg <- grepl("^P[PRSTU][A-Z]{3}$", d$co_indicativo)
    air <- grepl("^[A-Z]{3}[0-9]", d$co_indicativo)
    tibble::tibble(SIDE = label, ROWS = nrow(d),
                   REGISTRATION = sum(reg), AIRLINE = sum(air),
                   OTHER = sum(!reg & !air))
  }
  # Scope test. Brazilian aerodromes are SB/SD/SI/SJ/SN/SS/SW; anything else is
  # foreign, and AFIL/ZZZZ is no aerodrome at all. If one source's residual is
  # mostly international while the other's is domestic, the two are not the same
  # dataset with gaps — they are different definitions of "a Brazilian movement".
  br <- function(x) grepl("^S[BDIJNSW]", x)
  scope <- function(d, label) {
    if (nrow(d) == 0) return(NULL)
    dom <- br(d$co_addep) & br(d$co_addes)
    non <- d$co_addep %in% c("AFIL", "ZZZZ") | d$co_addes %in% c("AFIL", "ZZZZ")
    tibble::tibble(SIDE = label, ROWS = nrow(d),
                   DOMESTIC = sum(dom & !non),
                   INTERNATIONAL = sum(!dom & !non),
                   NO_AERODROME = sum(non),
                   INTERNATIONAL_PCT = round(100 * mean(!dom & !non), 1))
  }
  message("\nThe residual by scope — is one side holding traffic the other never counted?")
  print(as.data.frame(dplyr::bind_rows(scope(res_a, "only archive"),
                                       scope(res_b, "only download"))))

  top_apt <- function(d, label) {
    if (nrow(d) == 0) return(NULL)
    tibble::tibble(SIDE = label,
                   VALUE = c(as.character(d$co_addep), as.character(d$co_addes))) |>
      dplyr::count(SIDE, VALUE, name = "ROWS") |>
      dplyr::arrange(dplyr::desc(ROWS)) |> head(10)
  }
  message("\nBusiest aerodromes in the residual:")
  print(as.data.frame(dplyr::bind_rows(top_apt(res_a, "only archive"),
                                       top_apt(res_b, "only download"))))

  message("\nThe residual — nothing recovered it — by traffic class:")
  print(as.data.frame(dplyr::bind_rows(cls(res_a, "only archive"),
                                       cls(res_b, "only download"))))

  invisible(list(passes = out, only_archive = res_a, only_download = res_b))
}

# =============================================================================
# totalbr_match_flights(years, tol_min, shift_min, ...)
#
# Matching by NEARNESS instead of by an equal key — because every exact key here
# is brittle for a reason that is now measured:
#
#   dh_inicio  differs by a constant 50 minutes between the sources
#   dh_eobt    changes whenever the plan is re-filed
#
# So a key on either one reports differences that are its own doing. On 2025 the
# two files differ by 1,150 rows in total, while key-based comparison called
# ~604,000 rows unmatched on each side. That gap is the method, not the data.
#
# Here, within each (callsign, departure, destination), the archive's clock is
# aligned by shift_min and each downloaded flight is paired with the nearest
# remaining archive flight inside tol_min. The pairing is ONE TO ONE: an archive
# row can be used once, so a genuine extra rotation stays unmatched instead of
# being absorbed by its neighbour.
#
# What is left over is the real difference between the two sources.
#
#   totalbr_match_flights(2025)                    # counts
#   totalbr_match_flights(2025, tol_min = 30)      # stricter
# =============================================================================
totalbr_match_flights <- function(years     = 2025,
                                  tol_min   = 120,
                                  shift_min = 50,
                                  raw_dir   = here::here("data-raw", "totalbr"),
                                  date_col  = "dt_dia") {
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  a <- sides$parquet; b <- sides$csv

  A <- data.table::data.table(
    A_ID = seq_len(nrow(a)),
    K    = paste(a$co_indicativo, a$co_addep, a$co_addes, sep = ""),
    # the archive is the side that runs early on dh_inicio, so it is the side
    # moved; the shift is measured, not assumed (compare_totalbr_time_shift())
    TA   = a$dh_inicio + shift_min * 60)
  B <- data.table::data.table(
    B_ID = seq_len(nrow(b)),
    K    = paste(b$co_indicativo, b$co_addep, b$co_addes, sep = ""),
    TB   = b$dh_inicio)
  A <- A[!is.na(TA)]; B <- B[!is.na(TB)]

  A[, TJ := TA]; B[, TJ := TB]
  data.table::setkey(A, K, TJ)
  data.table::setkey(B, K, TJ)
  m <- A[B, roll = "nearest", nomatch = 0L]
  if (nrow(m) == 0) {
    message("Nothing paired at all — check that the callsigns match.")
    return(invisible(NULL))
  }
  m[, DIFF_MIN := abs(as.numeric(difftime(TB, TA, units = "mins")))]
  m <- m[DIFF_MIN <= tol_min]

  # ONE TO ONE. The rolling join alone can hand the same archive row to several
  # downloaded rows; keeping the closest pairing for each side in turn leaves a
  # surplus rotation genuinely unpaired, which is what makes the leftovers mean
  # something.
  data.table::setorder(m, A_ID, DIFF_MIN)
  m <- m[!duplicated(A_ID)]
  data.table::setorder(m, B_ID, DIFF_MIN)
  m <- m[!duplicated(B_ID)]

  out <- tibble::tibble(
    YEAR              = paste(range(years), collapse = "-"),
    TOL_MIN           = tol_min,
    SHIFT_MIN         = shift_min,
    ARCHIVE_ROWS      = nrow(a),
    DOWNLOAD_ROWS     = nrow(b),
    MATCHED           = nrow(m),
    ONLY_ARCHIVE      = nrow(a) - nrow(m),
    ONLY_DOWNLOAD     = nrow(b) - nrow(m),
    MED_DIFF_MIN      = round(stats::median(m$DIFF_MIN), 1),
    MATCH_PCT         = round(100 * nrow(m) / nrow(a), 1)
  )
  attr(out, "matched") <- m
  attr(out, "only_archive")  <- setdiff(seq_len(nrow(a)), m$A_ID)
  attr(out, "only_download") <- setdiff(seq_len(nrow(b)), m$B_ID)
  out
}

# =============================================================================
# totalbr_unmatched_nearest(years, tol_min, shift_min, out_file, ...)
#
# The leftovers of totalbr_match_flights(), as the same dataset totalbr_unmatched()
# produces: one row per record, with SOURCE and the fields needed to look the
# flight up by hand. These are the rows with no counterpart within the tolerance,
# so they are the difference itself rather than an artefact of a key.
# =============================================================================
totalbr_unmatched_nearest <- function(years     = 2025,
                                      tol_min   = 120,
                                      shift_min = 50,
                                      out_file  = NULL,
                                      raw_dir   = here::here("data-raw", "totalbr"),
                                      date_col  = "dt_dia") {
  res <- totalbr_match_flights(years, tol_min, shift_min, raw_dir, date_col)
  if (is.null(res)) return(tibble::tibble())
  print(as.data.frame(res))

  sides <- totalbr_cmp_sides(years, raw_dir, date_col)
  take <- function(d, idx, src) {
    if (length(idx) == 0) return(NULL)
    d <- d[idx, ]
    tibble::tibble(
      YEAR = d$YEAR, SOURCE = src,
      co_indicativo = d$co_indicativo, co_addep = d$co_addep,
      co_addes = d$co_addes,
      dt_dia    = format(totalbr_parse_time(d[[date_col]]),
                         "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      dh_inicio = format(d$dh_inicio, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      dh_eobt   = format(d$dh_eobt,   "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      co_matricula = d$co_matricula, pk = d$pk,
      id = if ("id" %in% names(d)) d$id else NA_character_)
  }
  out <- dplyr::bind_rows(
    take(sides$parquet, attr(res, "only_archive"),  "parquet"),
    take(sides$csv,     attr(res, "only_download"), "csv"))
  if (is.null(out) || nrow(out) == 0) {
    message("Every flight found a counterpart within ", tol_min, " minutes.")
    return(tibble::tibble())
  }
  out <- dplyr::arrange(out, SOURCE, dh_inicio)
  message(nrow(out), " row(s) with no counterpart within ", tol_min, " minutes.")
  if (!is.null(out_file)) {
    utils::write.table(out, out_file, sep = ";", row.names = FALSE,
                       quote = TRUE, na = "", fileEncoding = "UTF-8")
    message("Written to ", out_file)
  }
  out
}

# =============================================================================
# totalbr_lookup(callsign, day, window_days, tol_min, shift_min, ...)
#
# Every row from BOTH sources for one callsign, with the pairing this code
# actually made: which row was matched to which, and by how many minutes.
#
# Two things it does that a plain filter must not omit, both learned by getting
# them wrong:
#
#   IT WIDENS THE DAY. The archive stamps dh_inicio 50 minutes earlier, so a
#   rotation the download files at 00:12 sits at 23:22 of the PREVIOUS day in the
#   archive. Filtering both sources to one calendar day therefore shows the
#   download's first rotation beside the archive's LAST one — two different
#   flights, side by side, looking like a comparison. They were never compared.
#   The window is +/- window_days so the real counterpart is on screen.
#
#   IT SHOWS THE PAIRING. PAIR groups the rows this code treats as one flight and
#   GAP_MIN is their separation after the shift; PAIR of NA means the row was left
#   unmatched. Without it the reader has to guess what was compared, which is how
#   two unrelated rotations came to be read as a mismatch.
#
#   totalbr_lookup("TAM3756", "2025-03-14")
# =============================================================================
totalbr_lookup <- function(callsign,
                           day         = NULL,
                           window_days = 1L,
                           tol_min     = 120,
                           shift_min   = 50,
                           years       = NULL,
                           raw_dir     = here::here("data-raw", "totalbr"),
                           date_col    = "dt_dia") {
  if (is.null(years) && !is.null(day))
    years <- unique(format(as.Date(day) + c(-1, 0, 1), "%Y"))
  sides <- totalbr_cmp_sides(years, raw_dir, date_col)

  days <- if (is.null(day)) NULL
          else format(rep(as.Date(day), each = 2 * window_days + 1) +
                        seq(-window_days, window_days), "%Y-%m-%d")

  pick <- function(d, src) {
    keep <- d$co_indicativo %in% callsign
    if (!is.null(days))
      keep <- keep & format(d$dh_inicio, "%Y-%m-%d", tz = "UTC") %in% days
    if (!any(keep, na.rm = TRUE)) return(NULL)
    d <- d[which(keep), ]
    tibble::tibble(
      SOURCE = src, co_indicativo = d$co_indicativo,
      co_addep = d$co_addep, co_addes = d$co_addes,
      # the archive's clock, moved by the measured shift, is what the pairing
      # compares — so it is what the table shows
      T_ALIGNED = if (src == "parquet") d$dh_inicio + shift_min * 60 else d$dh_inicio,
      dh_inicio = d$dh_inicio, dh_fim = d$dh_fim, dh_eobt = d$dh_eobt,
      co_matricula = d$co_matricula, co_modelo = d$co_modelo, pk = d$pk)
  }
  a <- pick(sides$parquet, "parquet"); b <- pick(sides$csv, "csv")
  if (is.null(a) && is.null(b)) {
    message("Neither source has ", paste(callsign, collapse = ", "))
    return(tibble::tibble())
  }

  # the same pairing rule the comparison uses, on these rows only
  pair <- if (!is.null(a) && !is.null(b)) {
    k <- function(d) paste(d$co_addep, d$co_addes, sep = "")
    totalbr_pair_nearest(k(a), a$T_ALIGNED, k(b), b$T_ALIGNED, tol_min)
  } else data.table::data.table(A_ID = integer(), B_ID = integer(),
                                DIFF_MIN = numeric())

  if (!is.null(a)) { a$PAIR <- NA_integer_; a$GAP_MIN <- NA_real_ }
  if (!is.null(b)) { b$PAIR <- NA_integer_; b$GAP_MIN <- NA_real_ }
  if (nrow(pair) > 0) {
    a$PAIR[pair$A_ID] <- seq_len(nrow(pair))
    b$PAIR[pair$B_ID] <- seq_len(nrow(pair))
    a$GAP_MIN[pair$A_ID] <- round(pair$DIFF_MIN, 1)
    b$GAP_MIN[pair$B_ID] <- round(pair$DIFF_MIN, 1)
  }

  out <- dplyr::bind_rows(a, b) |>
    dplyr::mutate(dplyr::across(c(T_ALIGNED, dh_inicio, dh_fim, dh_eobt),
                                ~ format(.x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))) |>
    dplyr::arrange(!is.na(PAIR), PAIR, T_ALIGNED)

  message(sum(!is.na(out$PAIR)) / 2, " pair(s); ",
          sum(is.na(out$PAIR)), " row(s) with no counterpart within ",
          tol_min, " minutes.")
  out
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(arrow); library(data.table); library(here)
  })
  args  <- commandArgs(trailingOnly = TRUE)
  years <- if (length(args) == 0) 2023:2025 else as.integer(args)
  for (k in c("flight_day", "eobt", "pk"))
    print(as.data.frame(compare_totalbr_sources(years, k)))
}
