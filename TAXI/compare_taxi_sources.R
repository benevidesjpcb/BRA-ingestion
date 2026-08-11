#!/usr/bin/env Rscript
# =============================================================================
# compare_taxi_sources.R
#
# Compares the SAME YEAR of taxi data coming from two places: the file
# downloaded from the ODIN API (data-raw/dstaxi/dsTaxiYYYY.csv) and the file you
# already had from another source (typically data-raw/dsTaxiYYYY.csv, from the
# SharePoint zip).
#
#   source(here::here("TAXI", "compare_taxi_sources.R"))
#   compare_taxi_sources(2026)                 # the summary
#   taxi_source_examples(2026)                 # rows only in one side
#   taxi_field_diffs(2026)                     # same movement, different values
#   taxi_unmatched(2026, out_file = "diff.csv")   # every unmatched row, to audit
#
# WHY FILE SIZE IS NOT THE MEASURE
# The downloader writes every field quoted ("Arr";"PRGUB";...); an export from
# another tool usually does not. That is ~2 bytes per field, 15 fields per row,
# millions of rows — tens of MB of difference with byte-identical content. Only a
# row-level comparison says anything.
#
# WHAT COUNTS AS "THE SAME MOVEMENT"
# dstaxi has no unique key, so one is built from what identifies a movement:
# the callsign, the phase, the movement stamp and the aerodrome pair. Two rows
# with the same key are the same event reported twice; a key present on one side
# only is a real difference between the sources.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# columns compared field by field once the movements are paired
TAXI_CMP_COLS <- c("matricula", "tipoaeronave", "box", "pista",
                   "dh_vra", "match_vra", "tipovoo", "vra_tipo_linha",
                   "companhia")

# ---- where the two files are -------------------------------------------------
# Left = the API download, right = whatever you had before. Both are looked up by
# name; pass explicit paths to compare anything else.
taxi_source_paths <- function(year,
                              api_dir = here::here("data-raw", "dstaxi"),
                              old_dir = here::here("data-raw")) {
  member <- paste0("dsTaxi", year, ".csv")
  list(api = file.path(api_dir, member), old = file.path(old_dir, member))
}

# ---- reading -----------------------------------------------------------------
# header = TRUE and fill = Inf are both required: the files carry 14 header names
# above 15-field rows, and fread would otherwise demote the header to data (or
# stop at the first long row).
taxi_read <- function(path, cols = NULL) {
  if (!file.exists(path)) stop("Not found: ", path)
  d <- data.table::fread(file = path, sep = ";", colClasses = "character",
                         na.strings = "", showProgress = FALSE,
                         fill = Inf, header = TRUE)
  data.table::setnames(d, names(d), tolower(names(d)))
  if (!is.null(cols)) {
    keep <- intersect(cols, names(d))
    d <- d[, ..keep]
  }
  d[]
}

# Text that means the same thing must compare equal: trailing blanks, case and
# the ISO "T" separator are formatting, not data.
taxi_norm <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  toupper(x)
}
taxi_norm_time <- function(x) {
  x <- taxi_norm(x)
  # 2026-01-01T00:00:00 and 2026-01-01 00:00:00 are the same instant; seconds are
  # kept, because two movements of the same callsign can be a minute apart
  x <- sub("T", " ", x, fixed = TRUE)
  substr(x, 1, 19)
}

taxi_key <- function(d) {
  paste(taxi_norm(d$indicativo), taxi_norm(d$mov),
        taxi_norm_time(d$dh_bimtra),
        taxi_norm(d$adpartida), taxi_norm(d$addestino), sep = "|")
}

.taxi_prep <- function(path) {
  d <- taxi_read(path)
  d[, KEY := taxi_key(d)]
  d[, MONTH := substr(taxi_norm_time(dh_bimtra), 6, 7)]
  d[]
}

# =============================================================================
# compare_taxi_sources(year) — the summary
#
#   ROWS_API / ROWS_OLD : rows in each file
#   KEYS_API / KEYS_OLD : distinct movements in each
#   DUP_API  / DUP_OLD  : rows that repeat a key WITHIN one file
#   BOTH                : movements present in both
#   ONLY_API / ONLY_OLD : movements present in one file only
#   FIELD_DIFF          : movements in both whose compared fields disagree
# =============================================================================
compare_taxi_sources <- function(year,
                                 paths = taxi_source_paths(year),
                                 quiet = FALSE) {
  if (!quiet) message("Reading ", paths$api)
  a <- .taxi_prep(paths$api)
  if (!quiet) message("Reading ", paths$old)
  b <- .taxi_prep(paths$old)

  ka <- a$KEY; kb <- b$KEY
  ua <- unique(ka); ub <- unique(kb)

  # movements reported by both files whose compared fields disagree — NOT the
  # number of fields, which is what a summary would return
  fd <- tryCatch(nrow(taxi_field_diffs(year, paths = paths, summary_only = FALSE,
                                       .a = a, .b = b)),
                 error = function(e) NA_integer_)

  tibble::tibble(
    YEAR       = year,
    ROWS_API   = length(ka),
    ROWS_OLD   = length(kb),
    KEYS_API   = length(ua),
    KEYS_OLD   = length(ub),
    DUP_API    = length(ka) - length(ua),
    DUP_OLD    = length(kb) - length(ub),
    BOTH       = length(intersect(ua, ub)),
    ONLY_API   = length(setdiff(ua, ub)),
    ONLY_OLD   = length(setdiff(ub, ua)),
    FIELD_DIFF = fd
  )
}

# ---- rows that exist on one side only ---------------------------------------
# SOURCE says which file the row came from, so an example can be looked up in the
# right place. Sorted by time, because a difference concentrated at the start or
# the end of the year is a coverage window, not a data difference.
taxi_source_examples <- function(year, n = 10, paths = taxi_source_paths(year)) {
  a <- .taxi_prep(paths$api)
  b <- .taxi_prep(paths$old)
  only_a <- a[!KEY %in% b$KEY][order(dh_bimtra)]
  only_b <- b[!KEY %in% a$KEY][order(dh_bimtra)]
  cols <- intersect(c("indicativo", "mov", "adpartida", "addestino",
                      "dh_bimtra", "dh_vra", "matricula", "box", "pista"),
                    names(a))
  rbind(
    utils::head(only_a, n)[, c(cols), with = FALSE][, SOURCE := "API"],
    utils::head(only_b, n)[, c(cols), with = FALSE][, SOURCE := "OLD"]
  )[]
}

# ---- where the difference sits ----------------------------------------------
# Same question as the examples, but counted: by month, and by aerodrome. A gap
# that is one month wide is a download window; a gap spread over every month at
# a handful of aerodromes is a scope difference between the sources.
taxi_diff_profile <- function(year, by = c("month", "airport"),
                              paths = taxi_source_paths(year)) {
  by <- match.arg(by)
  a <- .taxi_prep(paths$api)
  b <- .taxi_prep(paths$old)
  only_a <- a[!KEY %in% b$KEY]
  only_b <- b[!KEY %in% a$KEY]

  grp <- function(d) {
    if (by == "month") taxi_norm(d$MONTH)
    else ifelse(taxi_norm(d$mov) == "ARR", taxi_norm(d$addestino),
                taxi_norm(d$adpartida))
  }
  ta <- table(grp(only_a)); tb <- table(grp(only_b))
  lv <- sort(union(names(ta), names(tb)))
  out <- tibble::tibble(
    GROUP    = lv,
    ONLY_API = as.integer(ta[lv]),
    ONLY_OLD = as.integer(tb[lv])
  )
  out$ONLY_API[is.na(out$ONLY_API)] <- 0L
  out$ONLY_OLD[is.na(out$ONLY_OLD)] <- 0L
  out[order(-(out$ONLY_API + out$ONLY_OLD)), ]
}

# ---- same movement, different values ----------------------------------------
# The interesting case: both sources report the flight, and disagree about the
# stand, the runway, the registration or the block time. Returns one row per
# field with how often it differs, or the differing rows themselves.
taxi_field_diffs <- function(year, cols = TAXI_CMP_COLS, summary_only = TRUE,
                             paths = taxi_source_paths(year),
                             .a = NULL, .b = NULL) {
  a <- if (is.null(.a)) .taxi_prep(paths$api) else .a
  b <- if (is.null(.b)) .taxi_prep(paths$old) else .b

  # one row per key on each side: a key repeated within a file cannot be paired
  # unambiguously, and pairing it arbitrarily would invent differences
  a1 <- a[!duplicated(KEY)]
  b1 <- b[!duplicated(KEY)]
  cols <- intersect(cols, intersect(names(a1), names(b1)))
  if (length(cols) == 0) stop("None of the requested columns exist in both files.")

  m <- merge(a1[, c("KEY", cols), with = FALSE],
             b1[, c("KEY", cols), with = FALSE],
             by = "KEY", suffixes = c(".api", ".old"))

  diff_flag <- function(col) {
    va <- taxi_norm(m[[paste0(col, ".api")]])
    vb <- taxi_norm(m[[paste0(col, ".old")]])
    if (grepl("^dh_", col)) {
      va <- taxi_norm_time(m[[paste0(col, ".api")]])
      vb <- taxi_norm_time(m[[paste0(col, ".old")]])
    }
    # NA on both sides is agreement, not a difference
    !(is.na(va) & is.na(vb)) & (is.na(va) | is.na(vb) | va != vb)
  }
  flags <- vapply(cols, diff_flag, logical(nrow(m)))
  if (is.null(dim(flags))) flags <- matrix(flags, nrow = nrow(m))

  if (summary_only) {
    out <- tibble::tibble(
      FIELD    = cols,
      DIFFERS  = as.integer(colSums(flags)),
      COMPARED = nrow(m)
    )
    out$PCT <- round(100 * out$DIFFERS / pmax(out$COMPARED, 1), 3)
    return(out[order(-out$DIFFERS), ])
  }
  m[rowSums(flags) > 0][]
}

# ---- the full audit trail ---------------------------------------------------
# Writes every unmatched movement, both directions, with SOURCE — so a flight can
# be looked up in the file it came from and checked by hand.
taxi_unmatched <- function(year, out_file = NULL, paths = taxi_source_paths(year)) {
  a <- .taxi_prep(paths$api)
  b <- .taxi_prep(paths$old)
  only_a <- a[!KEY %in% b$KEY][, SOURCE := "API"]
  only_b <- b[!KEY %in% a$KEY][, SOURCE := "OLD"]
  out <- rbind(only_a, only_b, fill = TRUE)[order(dh_bimtra)]
  if (!is.null(out_file)) {
    data.table::fwrite(out, out_file, sep = ";")
    message("Wrote ", nrow(out), " row(s) to ", out_file)
  }
  out[]
}
