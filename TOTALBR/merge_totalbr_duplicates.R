#!/usr/bin/env Rscript
# =============================================================================
# merge_totalbr_duplicates.R
#
# COLLAPSES the duplicate rows of a flight into ONE row. This is how the
# duplication is resolved; deletion applies to exactly one case:
#
#   totalbr_drop_duplicate_pk()  drops a row whose pk was already seen - the
#                                same row stored twice, so nothing is lost
#   totalbr_merge_duplicates()   fuses the rows of one flight into a single row
#
#   source(here::here("TOTALBR", "merge_totalbr_duplicates.R"))
#   pairs  <- totalbr_near_pairs(d, tol_min = TOTALBR_NEAR_MIN)
#   merged <- totalbr_merge_duplicates(d, pairs)
#
# WHY MERGE
# The same flight is reported by more than one unit — APPAN and APPBR, say — and
# each report covers the stretch that unit saw. Neither row is wrong and neither
# is complete: one may carry the registration and the other not, one starts
# earlier and the other ends later. Dropping either loses real information.
#
# THE RULES (as specified)
#   dh_inicio, dt_dia : from the EARLIEST row of the group
#   dh_fim            : from the LATEST row
#   everything else   : the first value that is actually filled in, taking the
#                       rows in order of how informative they are — the one with
#                       a registration first, then an actual movement over a
#                       filed plan, then the earliest.
#
# So co_matricula comes from the row that has it, and the aerodrome pair likewise
# — a blank never overwrites a value.
#
#   li_orgaos and the other li_* arrays : the UNION of every row's values.
#
# That last one is not a detail: li_orgaos is what differs between the rows —
# APPAN on one, APPBR on the other — so "the first filled value" would drop the
# very unit whose report justified merging. The merged row lists both,
# pipe-separated.
#
# The extremes are taken over EVERY row of the group. A row with
# dh_fim == dh_inicio is not a filed plan and not an intention — it is the flight
# as one unit captured it on radar, at a single instant — so it dates the flight
# just as much as a row with a span. A group where every row is a single instant
# is marked ZERO_SPAN_ONLY: its SPAN_MIN of 0 describes the records, not the
# flight.
#
# GROUPS, NOT PAIRS
# A flight can appear three times (a plan and two unit reports). The pairs are
# therefore treated as edges of a graph and every connected group is collapsed
# together, so three rows become one row and not "one merge plus a leftover".
#
# WHAT IS ADDED
#   N_MERGED   how many rows went into this one (1 = untouched)
#   SPAN_MIN   dh_fim - dh_inicio of the merged row, in minutes
#   MERGED_PK  the pk of every row that went in, pipe-separated, so the merge is
#              auditable back to the source rows
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required. install.packages('data.table')")
})

# ---- connected groups from the pair list --------------------------------------
# Union-find over the row ids: pairs are edges, and a flight reported three times
# forms one group of three rather than two overlapping pairs.
totalbr_pair_groups <- function(pairs, n) {
  parent <- seq_len(n)
  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]      # path compression
      x <- parent[x]
    }
    x
  }
  a <- as.integer(pairs$ROW_ID); b <- as.integer(pairs$PARTNER_ID)
  for (i in seq_along(a)) {
    ra <- find(a[i]); rb <- find(b[i])
    if (ra != rb) parent[rb] <- ra
  }
  vapply(seq_len(n), find, integer(1))
}

# the first value that is actually there; blanks and NA never win over a value
.first_filled <- function(x) {
  ok <- !is.na(x)
  if (is.character(x)) ok <- ok & nzchar(trimws(x))
  if (!any(ok)) return(x[1])
  x[which(ok)[1]]
}

# ---- columns that must be UNIONED, not picked --------------------------------
# li_orgaos is the reason the rows differ: one says APPAN, the other APPBR. Taking
# "the first filled value" there keeps one unit and silently drops the other,
# erasing exactly what the merge exists to preserve. The same holds for the other
# li_* arrays, which the download stores pipe-separated.
TOTALBR_UNION_COLS <- c("li_orgaos", "li_tipovoo", "li_regravoo",
                        "li_prnav", "li_rvsm")

.union_values <- function(x) {
  v <- if (is.list(x)) unlist(x) else as.character(x)
  v <- unlist(strsplit(v[!is.na(v)], "|", fixed = TRUE))
  v <- unique(trimws(v))
  v <- v[nzchar(v)]
  if (length(v) == 0) return(NA_character_)
  paste(sort(v), collapse = "|")
}

# =============================================================================
# totalbr_merge_duplicates(d, pairs, time_cols)
#
#   d     : the data frame the pairs were computed on (row order matters —
#           ROW_ID is the row number in THIS frame)
#   pairs : the output of totalbr_near_pairs()
#
# Returns every row of `d`: the groups collapsed into one row each, plus the rows
# that were never flagged, in chronological order.
# =============================================================================
totalbr_merge_duplicates <- function(d, pairs,
                                     start_col = "dh_inicio",
                                     end_col   = "dh_fim",
                                     day_col   = "dt_dia",
                                     union_cols = TOTALBR_UNION_COLS,
                                     # the work is done in data.table for speed;
                                     # what comes back is a tibble, because that
                                     # is what the rest of the analysis uses
                                     as_tibble = TRUE) {

  dt <- data.table::as.data.table(d)
  n  <- nrow(dt)
  if (!nrow(pairs)) {
    dt[, `:=`(N_MERGED = 1L, MERGED_PK = if ("pk" %in% names(dt)) pk else NA_character_)]
    return(if (as_tibble && requireNamespace("tibble", quietly = TRUE))
             tibble::as_tibble(dt) else dt[])
  }
  for (cl in c(start_col, end_col))
    if (!cl %in% names(dt)) stop("Column not found: ", cl)

  dt[, ROW_ID := .I]
  dt[, GRP_ID := totalbr_pair_groups(pairs, n)]

  touched <- unique(c(as.integer(pairs$ROW_ID), as.integer(pairs$PARTNER_ID)))
  grp <- dt[ROW_ID %in% touched]
  rest <- dt[!ROW_ID %in% touched]

  # Order inside each group decides which value wins for every non-time column:
  # a row with a registration beats one without; a row covering a span beats a
  # single-instant capture (dh_fim == dh_inicio), because it saw more of the
  # flight; ties go to the earliest.
  has_reg <- if ("co_matricula" %in% names(grp))
    !is.na(grp$co_matricula) & nzchar(trimws(as.character(grp$co_matricula))) else rep(FALSE, nrow(grp))
  is_plan <- !is.na(grp[[start_col]]) & !is.na(grp[[end_col]]) &
             grp[[start_col]] == grp[[end_col]]
  grp[, `:=`(.HAS_REG = as.integer(!has_reg),      # 0 first = has registration
             .IS_PLAN = as.integer(is_plan))]      # 0 first = row with a span
  data.table::setorderv(grp, c("GRP_ID", ".HAS_REG", ".IS_PLAN", start_col))

  val_cols <- setdiff(names(grp),
                      c("GRP_ID", "ROW_ID", ".HAS_REG", ".IS_PLAN"))

  merged <- grp[, c(
    lapply(.SD, .first_filled),
    list(N_MERGED  = .N,
         MERGED_PK = if ("pk" %in% names(grp)) paste(pk, collapse = "|") else NA_character_)
  ), by = GRP_ID, .SDcols = val_cols]

  # The times are NOT "first filled": they are the EXTREMES of the group, over
  # every row in it. That is the whole point of merging.
  #
  # A row with dh_fim == dh_inicio is NOT a filed plan and not an intention: it is
  # the flight as one unit captured it on radar, at a single instant. It dates the
  # flight exactly as much as a row with a span does — so PRATC seen at 05:43 by
  # one unit and tracked 06:07-06:59 by another is one flight from 05:43 to 06:59.
  # (An earlier version of this file excluded those rows from the extremes. That
  # was wrong about what the data is, and it shortened real flights.)
  ext <- grp[, list(
    .START = suppressWarnings(min(get(start_col), na.rm = TRUE)),
    .END   = suppressWarnings(max(get(end_col),   na.rm = TRUE)),
    .ZERO_ONLY = all(.IS_PLAN == 1L)
  ), by = GRP_ID]
  # dt_dia follows the earliest row, not the group's own dt_dia values, so the
  # merged flight is dated by when it started
  if (day_col %in% names(grp)) {
    # dated by the earliest row, matching .START
    first_day <- grp[order(GRP_ID, get(start_col)),
                     list(.DAY = get(day_col)[1]), by = GRP_ID]
    ext <- merge(ext, first_day, by = "GRP_ID")
  }

  merged <- merge(merged, ext, by = "GRP_ID")
  merged[, (start_col) := .START]
  merged[is.finite(.END), (end_col) := .END]
  if (day_col %in% names(merged) && ".DAY" %in% names(merged))
    merged[, (day_col) := .DAY]
  # every row of the group was a single-instant capture, so the merged row has no
  # span of its own: SPAN_MIN will be 0 and that is a fact about the records, not
  # about the flight
  merged[, ZERO_SPAN_ONLY := .ZERO_ONLY]
  merged[, c(".START", ".END", ".ZERO_ONLY",
             intersect(".DAY", names(merged))) := NULL]

  # the union columns: every unit that reported the flight, not just the first
  ucols <- intersect(union_cols, names(grp))
  if (length(ucols) > 0) {
    uni <- grp[, lapply(.SD, .union_values), by = GRP_ID, .SDcols = ucols]
    merged <- merge(merged[, setdiff(names(merged), ucols), with = FALSE],
                    uni, by = "GRP_ID")
  }

  rest[, `:=`(N_MERGED = 1L,
              MERGED_PK = if ("pk" %in% names(rest)) pk else NA_character_)]
  rest[, c("GRP_ID") := NULL]
  merged[, c("GRP_ID") := NULL]
  if ("ROW_ID" %in% names(merged)) merged[, ROW_ID := NULL]
  if ("ROW_ID" %in% names(rest))   rest[, ROW_ID := NULL]

  out <- data.table::rbindlist(list(merged, rest), use.names = TRUE, fill = TRUE)
  out[, SPAN_MIN := round(as.numeric(difftime(get(end_col), get(start_col),
                                              units = "mins")), 1)]
  data.table::setorderv(out, start_col)

  message(sprintf("Merged %d row(s) into %d flight(s); %d row(s) untouched -> %d total.",
                  length(touched), nrow(merged), nrow(rest), nrow(out)))
  if (as_tibble && requireNamespace("tibble", quietly = TRUE))
    tibble::as_tibble(out) else out[]
}

# =============================================================================
# totalbr_merge_flights(d, gap_min) — the whole job in one call
#
# Groups the records of ONE FLIGHT and merges them. Use this rather than pairing:
#
#   merged <- totalbr_merge_flights(d, gap_min = 45)
#
# WHY NOT PAIRS
# totalbr_near_pairs() matches ONE record to ONE partner. That was built when the
# zero-duration rows looked like filed plans — one plan, one movement. They are
# not: each row is the flight as ONE UNIT captured it, so a flight has as many
# records as units that saw it. PRATC SDIM->SBVT on 2026-01-01 has four: APPSP at
# 05:43, then ACCCW, APPRJ and APPVT covering 06:07-06:59. One-to-one pairing
# merged three and left APPSP standing alone as a second, phantom flight.
#
# HOW THE GROUPING WORKS
# Records sharing callsign + departure + destination are sorted by start time.
# A record joins the group being built when it starts within `gap_min` of the
# LATEST END so far in that group; otherwise it opens a new flight. Chaining on
# the running end, not on the first record, is what lets a 05:43 instant, a
# 06:07-06:59 track and anything between them form one flight.
#
# CHOOSING gap_min
# It is the shortest turnaround that still separates two real flights of the same
# aircraft on the same route. Too large and a shuttle's consecutive legs merge;
# too small and one flight splits into several. The gap between the records of
# the same flight is minutes; a genuine next leg needs the aircraft to land, turn
# around and depart again. 45 minutes is the default, and
# totalbr_cluster_profile() shows what the data does around it.
# =============================================================================
totalbr_merge_flights <- function(d, gap_min = 45,
                                  start_col = "dh_inicio",
                                  end_col   = "dh_fim",
                                  key_cols  = c("co_indicativo", "co_addep", "co_addes"),
                                  ...) {
  edges <- totalbr_flight_edges(d, gap_min, start_col, end_col, key_cols)
  totalbr_merge_duplicates(d, edges, start_col = start_col, end_col = end_col, ...)
}

# The grouping, expressed as edges between consecutive records of one flight, so
# the union-find in totalbr_merge_duplicates() rebuilds the groups. Returns the
# same shape as totalbr_near_pairs(), which keeps every downstream tool working.
totalbr_flight_edges <- function(d, gap_min = 45,
                                 start_col = "dh_inicio",
                                 end_col   = "dh_fim",
                                 key_cols  = c("co_indicativo", "co_addep", "co_addes")) {
  empty <- data.table::data.table(ROW_ID = integer(), PARTNER_ID = integer(),
                                  GAP_MIN = numeric(), KIND = character())
  dt <- data.table::as.data.table(d)
  dt[, ROW_ID := .I]
  key_cols <- intersect(key_cols, names(dt))
  if (length(key_cols) == 0) stop("None of the key columns exist: ",
                                  paste(key_cols, collapse = ", "))
  dt[, .KEY := do.call(paste, c(as.list(dt[, key_cols, with = FALSE]), list(sep = "|")))]

  # a record with no start time, or no callsign, is never grouped: there is
  # nothing to place it by, and guessing would invent a flight
  ok <- dt[!is.na(get(start_col)) & !is.na(.KEY) & nzchar(.KEY)]
  if (nrow(ok) < 2) return(empty)

  ok[, .S := as.numeric(get(start_col))]
  ok[, .E := data.table::fifelse(is.na(get(end_col)), .S, as.numeric(get(end_col)))]
  ok[.E < .S, .E := .S]          # an end before its own start must not extend the group
  data.table::setorderv(ok, c(".KEY", ".S"))

  # .RUN_END is the LATEST end among the records already in the group. Measuring
  # the next record against it — rather than against the group's first record —
  # is what chains a 05:43 instant to a 06:07-06:59 track into one flight.
  ok[, .PREV    := data.table::shift(ROW_ID), by = .KEY]
  ok[, .RUN_END := data.table::shift(cummax(.E)), by = .KEY]
  ok[, .GAP     := (.S - .RUN_END) / 60]

  e <- ok[!is.na(.PREV) & !is.na(.RUN_END) & .GAP <= gap_min,
          list(ROW_ID, PARTNER_ID = .PREV, GAP_MIN = round(.GAP, 1),
               KIND = "same flight")]
  if (nrow(e) == 0) return(empty)
  e[]
}

# ---- what the grouping does at different tolerances --------------------------
# Run before trusting gap_min: how many flights each tolerance produces, and the
# largest span it creates. A tolerance that starts swallowing consecutive legs
# shows up as the span jumping while the flight count barely moves.
totalbr_cluster_profile <- function(d, gaps = c(15, 30, 45, 60, 90, 120, 180),
                                    start_col = "dh_inicio", end_col = "dh_fim") {
  out <- lapply(gaps, function(g) {
    e <- totalbr_flight_edges(d, g, start_col, end_col)
    m <- totalbr_merge_duplicates(d, e, start_col = start_col, end_col = end_col,
                                  as_tibble = FALSE)
    data.frame(GAP_MIN = g, FLIGHTS = nrow(m),
               MERGED = sum(m$N_MERGED > 1),
               MAX_SPAN = suppressWarnings(max(m$SPAN_MIN, na.rm = TRUE)),
               P99_SPAN = round(stats::quantile(m$SPAN_MIN, .99, na.rm = TRUE), 1))
  })
  out <- do.call(rbind, out)
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out
}

# ---- how far apart are the paired rows? -------------------------------------
# The number that sets the tolerance. A plan and its movement sit minutes apart;
# a plan wrongly matched to the NEXT flight of the same aircraft sits hours
# apart. Read the upper quantiles: where they jump is where the pairing stops
# describing one flight. PRATC on 2026-01-01 was paired across 120 minutes,
# SBVT to SBMT - two hours is another flight for a taxi operator, not a delay.
totalbr_gap_profile <- function(pairs, probs = c(.5, .75, .9, .95, .99, 1)) {
  g <- data.table::as.data.table(pairs)
  out <- g[, c(list(N = .N),
               as.list(round(stats::quantile(abs(GAP_MIN), probs, na.rm = TRUE), 1))),
           by = KIND]
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out[]
}

# ---- see one merge, source rows and result side by side ----------------------
# The check worth doing before trusting the output: the merged row must start no
# later than its earliest source and end no earlier than its latest.
totalbr_merge_example <- function(d, pairs, merged, callsign,
                                  start_col = "dh_inicio", end_col = "dh_fim") {
  cols <- intersect(c("pk", "co_indicativo", "co_addep", "co_addes",
                      "co_matricula", start_col, end_col, "dh_eobt",
                      "N_MERGED", "MERGED_PK"), names(merged))
  src <- data.table::as.data.table(d)[co_indicativo == callsign]
  src <- src[, intersect(cols, names(src)), with = FALSE][, SOURCE := "original"]
  res <- data.table::as.data.table(merged)[co_indicativo == callsign]
  res <- res[, intersect(cols, names(res)), with = FALSE][, SOURCE := "merged"]
  out <- data.table::rbindlist(list(src, res), use.names = TRUE,
                               fill = TRUE)[order(get(start_col))]
  # returned as a tibble: this one is meant to be read on screen, and a
  # data.table prints the whole thing rather than a readable head
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out[]
}
