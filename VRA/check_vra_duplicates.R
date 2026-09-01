#!/usr/bin/env Rscript
# =============================================================================
# check_vra_duplicates.R
#
# WHAT THE REPEATED ROWS ARE, before anything is deleted.
#
#   source(here::here("VRA", "check_vra_duplicates.R"))
#   p <- vra_key_profile(2024)      # how much repeats, and IN WHICH COLUMNS
#   e <- vra_key_examples(2024)     # the repeated groups themselves, side by side
#
# WHY NOT JUST COUNT THEM
# A count says how much to delete; it does not say whether deleting is right. Two
# rows under one key can be:
#
#   the same movement filed twice          -> identical everywhere; either copy will do
#   a flight re-filed after a change       -> differs on the realised times or the
#                                             situation; the LATER filing is the truth
#                                             and the earlier one is history
#   two real flights sharing a key         -> differs on aerodromes or on the
#                                             realised times by hours; the key is
#                                             wrong, not the data
#
# Only the third means "the key does not work". The first two mean "the key works
# and something has to be chosen". The column-by-column report below is what
# tells them apart, and it is the same question TOTALBR answered before its own
# de-duplication: which columns vary within a group decides whether collapsing
# loses anything.
#
# NOTHING HERE DELETES A ROW. It reports.
# =============================================================================

source(here::here("VRA", "download_vra.R"))

# The key under test.
#
# WHY THESE FIVE, AND NOT operator + number + scheduled departure
# That first candidate collapsed flights that are plainly different, for two
# reasons found in the data:
#
#   A FLIGHT NUMBER IS NOT A ROUTE. AAL Z0930 on 14/01/2026 appears as
#   SBGR -> KFLL and again as KFLL -> KMIA: two legs of one number, two
#   movements, two rows. Origin and destination separate them.
#
#   dt_partida_prevista IS OFTEN EMPTY. Those same rows carry "" -- they are
#   cd_di = 4, non-scheduled services, which have no planned departure at all. A
#   blank component makes the key "AAL|Z0930|", so every Z0930 in the file
#   collides, including the same route on 14/01 and on 21/02. A key component
#   that can be empty does not divide anything; it merges.
#
# dt_referencia is the day the API itself partitions on and is never empty, so it
# is what carries the date here. dt_partida_prevista stays OUT of the key for
# exactly the reason above -- it is still read, as a column, where it exists.
VRA_KEY_COLS <- c("sg_empresa_icao", "nr_voo",
                  "sg_icao_origem", "sg_icao_destino", "dt_referencia")

vra_build_key <- function(d, key_cols = VRA_KEY_COLS) {
  miss <- setdiff(key_cols, names(d))
  if (length(miss) > 0) stop("Missing key column(s): ", paste(miss, collapse = ", "))
  do.call(paste, c(lapply(key_cols, function(k) trimws(as.character(d[[k]]))),
                   list(sep = "|")))
}

# =============================================================================
# vra_key_profile(years, key_cols, raw_dir)
#
# Two tables, and the second is the one that matters.
#
#   $summary : rows, distinct keys, repeated rows, percentage
#   $columns : for each column, in how many REPEATED GROUPS its value varies.
#              A column that never varies is carried identically by every copy;
#              a column that varies in most groups is what the copies are FOR.
# =============================================================================
vra_key_profile <- function(years    = NULL,
                            key_cols = VRA_KEY_COLS,
                            raw_dir  = here::here("data-raw", "vra"),
                            quiet    = FALSE) {

  d <- vra_read(years, raw_dir = raw_dir, quiet = quiet)
  if (nrow(d) == 0) return(NULL)

  k   <- vra_build_key(d, key_cols)
  dup <- k %in% k[duplicated(k)]

  # A KEY COMPONENT THAT IS EMPTY MERGES ROWS SILENTLY, which is how the first
  # candidate collapsed three unrelated Z0930 movements into one group. Say how
  # often each component is blank before any conclusion is drawn from the counts.
  blanks <- vapply(key_cols, function(cl)
    sum(is.na(d[[cl]]) | !nzchar(trimws(as.character(d[[cl]])))), integer(1))
  if (any(blanks > 0) && !quiet) {
    message("Blank key components -- these MERGE rows rather than separate them:")
    print(as.data.frame(tibble::tibble(
      COLUMN = key_cols, BLANK = as.integer(blanks),
      PCT    = round(100 * as.integer(blanks) / nrow(d), 2))), row.names = FALSE)
    message("")
  }

  summary <- tibble::tibble(
    ROWS       = nrow(d),
    DISTINCT   = dplyr::n_distinct(k),
    IN_A_GROUP = sum(dup),                  # every row of a repeated key, not the surplus
    SURPLUS    = sum(duplicated(k)),        # what a naive de-duplication would delete
    GROUPS     = dplyr::n_distinct(k[dup]),
    PCT        = round(100 * sum(duplicated(k)) / nrow(d), 2))

  if (summary$GROUPS == 0) {
    message("No repeated key. The candidate holds on this data.")
    return(list(summary = summary, columns = tibble::tibble()))
  }

  # Within each repeated group, does this column take more than one value? Done
  # on the repeated rows only -- the rest are single-row groups and would dilute
  # every proportion towards zero.
  g   <- data.table::as.data.table(d[dup, , drop = FALSE])
  g[, .K := k[dup]]
  cols <- setdiff(names(g), c(".K", key_cols))
  varies <- vapply(cols, function(cl) {
    v <- g[, list(N = data.table::uniqueN(get(cl))), by = .K]
    sum(v$N > 1L)
  }, integer(1))

  columns <- tibble::tibble(
    COLUMN      = cols,
    GROUPS_VARY = as.integer(varies),
    PCT_GROUPS  = round(100 * as.integer(varies) / summary$GROUPS, 1)) |>
    dplyr::arrange(dplyr::desc(GROUPS_VARY))

  if (!quiet) {
    message("")
    print(as.data.frame(summary), row.names = FALSE)
    message("\nWhere the copies differ (share of the ", summary$GROUPS,
            " repeated groups):")
    print(as.data.frame(columns), row.names = FALSE)
    message("\nA column varying in ~0% of groups is carried identically by every copy.",
            "\nA column varying in most groups is what the copies exist to record.")
  }
  list(summary = summary, columns = columns)
}

# =============================================================================
# vra_key_examples(years, n, key_cols, raw_dir, only_col)
#
# The repeated groups themselves, whole rows, one group after another, so a
# judgement can be made by looking rather than by inferring from a percentage.
#
#   only_col : restrict to groups whose copies differ on THAT column, which is
#              how a suspicion raised by $columns gets confirmed -- ask for the
#              groups where sg_icao_destino varies and see whether they are two
#              real flights.
# =============================================================================
vra_key_examples <- function(years    = NULL,
                             n        = 5L,
                             key_cols = VRA_KEY_COLS,
                             only_col = NULL,
                             raw_dir  = here::here("data-raw", "vra"),
                             quiet    = TRUE) {

  d <- vra_read(years, raw_dir = raw_dir, quiet = quiet)
  if (nrow(d) == 0) return(tibble::tibble())

  k   <- vra_build_key(d, key_cols)
  dup <- k %in% k[duplicated(k)]
  if (!any(dup)) { message("No repeated key."); return(tibble::tibble()) }

  g <- data.table::as.data.table(d[dup, , drop = FALSE])
  g[, .K := k[dup]]

  keys <- unique(g$.K)
  if (!is.null(only_col)) {
    if (!only_col %in% names(g)) stop("No column '", only_col, "'.")
    v    <- g[, list(N = data.table::uniqueN(get(only_col))), by = .K]
    keys <- v$.K[v$N > 1L]
    message(length(keys), " group(s) differ on ", only_col)
    if (length(keys) == 0) return(tibble::tibble())
  }
  keys <- utils::head(keys, n)
  out  <- g[.K %in% keys]
  data.table::setorderv(out, c(".K", "dt_partida_real"))
  tibble::as_tibble(out)
}
