#!/usr/bin/env Rscript
# =============================================================================
# compare_taxi_cgna.R
#
# The same `dstaxi` year from the TWO APIs that serve it: ODIN
# (data-raw/dstaxi/dsTaxiYYYY.csv) and the CGNA
# (data-raw/dstaxi/dsTaxiYYYYcgna.csv).
#
#   source(here::here("TAXI", "compare_taxi_cgna.R"))
#   taxi_cgna_daily(2026)              # START HERE -- rows per day, both sides
#   compare_taxi_cgna(2026)            # the summary
#   taxi_cgna_field_diffs(2026)        # same movement, different values
#   taxi_cgna_examples(2026)           # rows only one side has
#
# The comparison itself is the one in compare_taxi_sources.R -- same key, same
# normalisation, same field-by-field diff. Only the pair of files changes. What
# is added here is the DAILY PROFILE, because the first question about two
# sources of very different size is not "which movements differ" but "is one of
# them truncated", and a total cannot tell those apart.
#
# HOW TO READ THE DAILY PROFILE
#   * a CGNA day at a suspiciously round number (1000, 5000, 10000) on MANY days
#     -> a page limit: the endpoint is paginating and only the first page is
#        being kept. The fix is in the downloader, not here.
#   * a handful of days present on one side only -> a coverage window: the
#     download simply has not fetched them.
#   * every day present on both sides at a stable ratio -> not truncation at
#     all: the two sources have different scope (aerodromes, phases, flight
#     types), and taxi_cgna_scope() says which.
# =============================================================================

source(here::here("TAXI", "compare_taxi_sources.R"))

# left = ODIN, right = CGNA. The comparator's own vocabulary is api/old; the
# labels in what it returns are renamed below so no one has to remember which
# of the two "old" means here.
taxi_cgna_paths <- function(year, dir = here::here("data-raw", "dstaxi")) {
  list(api = file.path(dir, sprintf("dsTaxi%d.csv", year)),
       old = file.path(dir, sprintf("dsTaxi%dcgna.csv", year)))
}

.cgna_relabel <- function(x) {
  names(x) <- sub("_API$", "_ODIN", sub("_OLD$", "_CGNA", names(x)))
  names(x) <- sub("^ONLY_API$", "ONLY_ODIN", sub("^ONLY_OLD$", "ONLY_CGNA",
                                                 names(x)))
  x
}

# ---- the summary -------------------------------------------------------------
compare_taxi_cgna <- function(year, quiet = FALSE) {
  .cgna_relabel(compare_taxi_sources(year, paths = taxi_cgna_paths(year),
                                     quiet = quiet))
}

taxi_cgna_examples <- function(year, n = 10)
  taxi_source_examples(year, n = n, paths = taxi_cgna_paths(year))

taxi_cgna_field_diffs <- function(year, cols = TAXI_CMP_COLS, summary_only = TRUE)
  taxi_field_diffs(year, cols = cols, summary_only = summary_only,
                   paths = taxi_cgna_paths(year))

taxi_cgna_field_values <- function(year, field, n = 15)
  taxi_field_values(year, field, n = n, paths = taxi_cgna_paths(year))

taxi_cgna_lookup <- function(year, callsign, day = NULL)
  taxi_lookup(year, callsign, day = day, paths = taxi_cgna_paths(year))

# =============================================================================
# taxi_cgna_daily(year) -- rows per day on each side
#
# One row per calendar day present in either file:
#   ODIN / CGNA   rows on each side that day
#   RATIO         ODIN / CGNA, so a constant factor is visible at a glance
#   FLAG          "cgna missing" (the day is absent from the CGNA file),
#                 "odin missing", or "" when both have it
#
# This is the truncation test. A downloader that keeps only the first page of a
# paginated answer produces the SAME count on almost every day; a source that
# genuinely reports less produces a count that moves with traffic.
# =============================================================================
taxi_cgna_daily <- function(year, paths = taxi_cgna_paths(year)) {
  count_by_day <- function(path) {
    if (!file.exists(path)) {
      message("Not found (treated as empty): ", path); return(NULL)
    }
    d <- taxi_read(path, cols = c("dh_bimtra", "indicativo", "mov",
                                  "adpartida", "addestino"))
    day <- substr(taxi_norm_time(d$dh_bimtra), 1, 10)
    k   <- taxi_key(d)
    data.table::data.table(DAY = day, KEY = k)[!is.na(DAY)]
  }
  a <- count_by_day(paths$api)
  b <- count_by_day(paths$old)

  agg <- function(d) if (is.null(d)) data.table::data.table(
    DAY = character(0), N = integer(0), KEYS = integer(0))
    else d[, .(N = .N, KEYS = data.table::uniqueN(KEY)), by = DAY]

  ta <- agg(a); tb <- agg(b)
  days <- sort(union(ta$DAY, tb$DAY))
  out <- data.table::data.table(
    DAY  = days,
    ODIN = ta$N[match(days, ta$DAY)],
    CGNA = tb$N[match(days, tb$DAY)]
  )
  out[is.na(ODIN), ODIN := 0L][is.na(CGNA), CGNA := 0L]
  out[, RATIO := round(ODIN / pmax(CGNA, 1), 2)]
  out[, FLAG := data.table::fifelse(CGNA == 0L, "cgna missing",
               data.table::fifelse(ODIN == 0L, "odin missing", ""))]

  # The page-limit signature, said out loud rather than left to be spotted in a
  # 365-row table: one count repeating across many days is not what traffic does.
  nz <- out[CGNA > 0L, CGNA]
  if (length(nz) >= 5) {
    top <- sort(table(nz), decreasing = TRUE)[1]
    if (as.integer(top) >= max(3L, ceiling(0.3 * length(nz))))
      message(sprintf(
        "NOTE: %d of %d CGNA day(s) hold exactly %s row(s). A repeated count is the\n",
        as.integer(top), length(nz), names(top)),
        "      signature of a page limit, not of traffic -- check whether the\n",
        "      endpoint paginates.")
  }
  out[]
}

# =============================================================================
# taxi_cgna_scope(year, by) -- WHERE the extra rows are, not how many
#
# When the daily profile rules truncation out, the difference is scope. This
# counts each side by aerodrome, by phase or by flight type, so a source that
# simply does not carry (say) general aviation, or only the main aerodromes,
# shows up as a category present on one side and empty on the other.
# =============================================================================
taxi_cgna_scope <- function(year, by = c("airport", "mov", "tipovoo",
                                         "vra_tipo_linha", "companhia"),
                            paths = taxi_cgna_paths(year), n = 25) {
  by <- match.arg(by)
  grab <- function(path) {
    if (!file.exists(path)) return(character(0))
    d <- taxi_read(path)
    if (by == "airport")
      taxi_norm(ifelse(taxi_norm(d$mov) == "ARR", d$addestino, d$adpartida))
    else if (by %in% names(d)) taxi_norm(d[[by]])
    else character(0)
  }
  va <- grab(paths$api); vb <- grab(paths$old)
  ta <- table(va, useNA = "ifany"); tb <- table(vb, useNA = "ifany")
  lv <- union(names(ta), names(tb))
  out <- data.table::data.table(
    GROUP = lv,
    ODIN  = as.integer(ta[lv]),
    CGNA  = as.integer(tb[lv])
  )
  out[is.na(ODIN), ODIN := 0L][is.na(CGNA), CGNA := 0L]
  out[, ONLY := data.table::fifelse(CGNA == 0L, "ODIN only",
              data.table::fifelse(ODIN == 0L, "CGNA only", ""))]
  utils::head(out[order(-(ODIN + CGNA))], n)[]
}
