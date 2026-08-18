#!/usr/bin/env Rscript
# =============================================================================
# merge_totalbr_duplicates.R
#
# COLLAPSES duplicate rows into ONE row. NOTHING HERE DELETES: both kinds of
# duplication are resolved by merging, because in both the copies carry
# different payloads and dropping one loses filled fields.
#
#   totalbr_merge_duplicate_pk()  rows sharing a pk - one movement, recorded
#                                 twice; identity equal, payload not
#   totalbr_merge_flights()       rows of one flight, one per unit that saw it
#
#   source(here::here("TOTALBR", "prepare_totalbr.R"))   # sources this and more
#   merged <- totalbr_merge_flights(d, gap_min = 60)
#
# totalbr_drop_duplicate_pk() (in check_totalbr_duplicates.R) is kept only to
# reproduce the old delete-the-repeat behaviour for comparison. It loses 162
# dh_eet values on the archive; see the qmd.
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
#                       a registration first, then a row covering a span over a
#                       single-instant capture, then the earliest.
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
# A flight can appear three or four times, once per unit that tracked it. The
# pairs are therefore treated as edges of a graph and every connected group is
# collapsed together, so four rows become one row and not "one merge plus
# leftovers".
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

# How far apart two records of the SAME flight may be, in minutes. One value for
# the whole pipeline: when the default and the examples each carried their own
# number, a dataset inspected at 45 was written to file at 60 and the counts did
# not reconcile. Change it here, having looked at totalbr_cluster_profile().
TOTALBR_GAP_MIN <- 60

.union_values <- function(x) {
  v <- if (is.list(x)) unlist(x) else as.character(x)
  # the two sources separate differently: the API download pipe-separates after
  # the CSV round-trip, the parquet archive comma-separates. Splitting on only one
  # leaves "APPAN,APPAN" standing as a single token, and the union then reports a
  # unit list that is neither source's.
  v <- unlist(strsplit(v[!is.na(v)], "[|,]"))
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
#   merged <- totalbr_merge_flights(d, gap_min = 60)
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
# around and depart again. 60 minutes is the default — TOTALBR_GAP_MIN, set in
# one place so the pipeline and the examples cannot drift apart — and
# totalbr_cluster_profile() shows what the data does around it.
# =============================================================================
totalbr_merge_flights <- function(d, gap_min = TOTALBR_GAP_MIN,
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
totalbr_flight_edges <- function(d, gap_min = TOTALBR_GAP_MIN,
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

# =============================================================================
# totalbr_edge_profile(d, gap_min, unit_col)
#
# EVERY LINK THE GROUPING WOULD MAKE, split by whether the two records share a
# unit, and by how far apart they are.
#
# WHAT THE QUESTION IS
# The records of one flight are supposed to come from DIFFERENT units -- one per
# unit that saw it. So a link between two records of the SAME unit is suspect: a
# unit does not report one flight twice, which would make those two records two
# flights. PRMES on 2026-01-01 is the case that raised it: ten APPPS records of a
# helicopter shuttling SBPS <-> SD49, chained into one flight because each hop
# starts within gap_min of the previous one and a zero-duration row makes the
# group's running end its last START.
#
# BUT THE RULE CANNOT BE ADOPTED ON THAT ONE CASE. If a unit sometimes emits two
# records of the SAME flight, refusing to link them splits real flights instead.
# The data says which: shared-unit links concentrated at tiny gaps are two views
# of one flight, and separating them would be wrong; shared-unit links spread
# over tens of minutes are separate legs, and linking them is the error.
#
# Reports, decides nothing. Run it before changing how the grouping works.
# =============================================================================
totalbr_edge_profile <- function(d, gap_min = TOTALBR_GAP_MIN,
                                 unit_col  = "li_orgaos",
                                 start_col = "dh_inicio",
                                 end_col   = "dh_fim",
                                 breaks    = c(0, 0.5, 1, 2, 3, 5, 10, 15,
                                               30, 45, 60)) {
  e <- totalbr_flight_edges(d, gap_min, start_col, end_col)
  if (nrow(e) == 0) { message("No link at gap_min = ", gap_min); return(tibble::tibble()) }
  if (!unit_col %in% names(d))
    stop("No column '", unit_col, "' to judge the units by.")

  # the units of a row, as a set; the two separators are both in play because the
  # archive comma-separates and the download pipe-separates
  units <- strsplit(as.character(d[[unit_col]]), "[|,]")
  units <- lapply(units, function(u) unique(trimws(u[!is.na(u) & nzchar(trimws(u))])))

  dt <- data.table::as.data.table(e)
  dt[, SHARED := mapply(function(a, b) length(intersect(units[[a]], units[[b]])) > 0,
                        ROW_ID, PARTNER_ID)]
  # a row with no unit listed cannot answer the question either way
  dt[, KNOWN := mapply(function(a, b) length(units[[a]]) > 0 && length(units[[b]]) > 0,
                       ROW_ID, PARTNER_ID)]
  dt[, BUCKET := cut(GAP_MIN, breaks = unique(c(breaks, Inf)),
                     include.lowest = TRUE, right = TRUE)]

  # PER MINUTE, not per bucket. The buckets are deliberately uneven -- the
  # interesting structure is in the first minutes -- so raw counts across them
  # are not comparable, and reading them as if they were says a distribution is
  # flat when its density peaks tenfold at zero.
  wid <- diff(unique(c(breaks, Inf)))
  out <- dt[KNOWN == TRUE, list(LINKS = .N), by = list(SHARED, BUCKET)]
  out[, WIDTH   := wid[as.integer(BUCKET)]]
  out[, PER_MIN := round(LINKS / WIDTH, 1)]
  data.table::setorderv(out, c("SHARED", "BUCKET"))
  if (any(!dt$KNOWN))
    message(sum(!dt$KNOWN), " link(s) left out: one of the two rows lists no unit.")
  tibble::as_tibble(out)
}

# =============================================================================
# totalbr_zero_split(d, gap_min, unit_col)
#
# THE LINKS SPLIT BY WHETHER EITHER RECORD HAS A DURATION OF ITS OWN.
#
# WHY DURATION AND NOT MINUTES
# The shared-unit links are a continuum in time -- per minute they decay 208,
# 152, 98, 68, 49, 35, 25, 23, 15.5, 13, with no valley -- so no threshold
# divides duplicate reports from separate legs. It cannot: 15 minutes apart is a
# new leg for a helicopter shuttle and the same flight for an airliner.
#
# What differs is the SHAPE of the records. A row with dh_fim > dh_inicio is a
# tracked passage, and the group's running end means something: the flight was
# still airborne at that time, so a record starting soon after belongs to it. A
# row with dh_fim == dh_inicio is one instant. Two instants carry no evidence of
# a flight lasting from one to the other, and chaining them is what let PRMES's
# ten APPPS hops collapse into a single flight spanning the day.
#
# READ IT AS: within SHARED = TRUE, are the long-gap links the ones where BOTH
# rows are instants? If they are, refusing exactly those links fixes PRMES and
# touches nothing else -- no tolerance is changed and no threshold is invented.
# =============================================================================
totalbr_zero_split <- function(d, gap_min = TOTALBR_GAP_MIN,
                               unit_col  = "li_orgaos",
                               start_col = "dh_inicio",
                               end_col   = "dh_fim",
                               breaks    = c(0, 1, 5, 15, 30, 60)) {
  e <- totalbr_flight_edges(d, gap_min, start_col, end_col)
  if (nrow(e) == 0) { message("No link at gap_min = ", gap_min); return(tibble::tibble()) }

  units <- strsplit(as.character(d[[unit_col]]), "[|,]")
  units <- lapply(units, function(u) unique(trimws(u[!is.na(u) & nzchar(trimws(u))])))
  # a row is an instant when its end equals its start, or it has no end at all --
  # both mean the same thing here: nothing says the flight lasted
  st   <- as.numeric(d[[start_col]])
  en   <- as.numeric(d[[end_col]])
  zero <- is.na(en) | en <= st

  dt <- data.table::as.data.table(e)
  dt[, SHARED := mapply(function(a, b) length(intersect(units[[a]], units[[b]])) > 0,
                        ROW_ID, PARTNER_ID)]
  dt[, SHAPE := data.table::fifelse(zero[ROW_ID] & zero[PARTNER_ID], "both instants",
                data.table::fifelse(zero[ROW_ID] | zero[PARTNER_ID], "one instant",
                                    "both tracked"))]
  dt[, BUCKET := cut(GAP_MIN, breaks = unique(c(breaks, Inf)),
                     include.lowest = TRUE, right = TRUE)]

  out <- dt[, list(LINKS = .N), by = list(SHARED, SHAPE, BUCKET)]
  data.table::setorderv(out, c("SHARED", "SHAPE", "BUCKET"))
  tibble::as_tibble(out)
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

# =============================================================================
# totalbr_merge_duplicate_pk(d)
#
# Rows sharing a pk, collapsed into one row each instead of dropped.
#
# The premise of "just delete the repeat" is that pk hashes the whole row, so the
# copies are interchangeable. Measured on the parquet archive, they are not: 455
# repeated pk, 910 rows, and ALL 910 stay distinct when every column is compared.
# What never differs is the movement's identity -- id, dt_dia, dh_inicio, dh_fim,
# co_indicativo, co_addep, co_addes. What differs is the payload, most often
# li_orgaos (303 of the 455 pairs) and dh_eet (291):
#
#   li_orgaos     dh_eet
#   APPAN         NA
#   APPAN,APPAN   2023-03-02 21:20:00
#
# Keeping "the first copy" therefore discards a filled dh_eet, or a unit, at
# random -- the very loss the merge in validation 2 exists to prevent. So the
# same rule applies here, in its degenerate case: identity is already equal, so
# there is nothing to chain and nothing to judge; union the li_* columns, take
# the first filled value everywhere else, and no information is lost either way.
# =============================================================================
totalbr_merge_duplicate_pk <- function(d, union_cols = TOTALBR_UNION_COLS,
                                       quiet = FALSE) {
  if (!"pk" %in% names(d)) {
    if (!quiet) message("No pk column; nothing to merge.")
    return(d)
  }
  dt <- data.table::as.data.table(d)
  # the archive writes pk uppercase and the API lowercase, so compare normalised
  key <- toupper(trimws(as.character(dt$pk)))
  key[!nzchar(key)] <- NA_character_
  # NA is not a repeat of NA: rows without a pk are untestable, never touched
  dup_keys <- unique(key[duplicated(key) & !is.na(key)])
  if (length(dup_keys) == 0) {
    if (!quiet) message("Identical pk: none.")
    return(if (requireNamespace("tibble", quietly = TRUE))
             tibble::as_tibble(dt) else dt[])
  }

  dt[, .PK_KEY := key]
  hit  <- dt[.PK_KEY %in% dup_keys]
  rest <- dt[!(.PK_KEY %in% dup_keys)]

  val_cols   <- setdiff(names(hit), ".PK_KEY")
  union_cols <- intersect(union_cols, val_cols)
  plain_cols <- setdiff(val_cols, union_cols)

  merged <- hit[, c(
    lapply(.SD, .first_filled),
    lapply(mget(union_cols), .union_values),
    list(N_PK_MERGED = .N)
  ), by = .PK_KEY, .SDcols = plain_cols]

  out <- data.table::rbindlist(list(merged, rest), fill = TRUE, use.names = TRUE)
  out[is.na(N_PK_MERGED), N_PK_MERGED := 1L]
  out[, .PK_KEY := NULL]
  if (!quiet)
    message(sprintf("Identical pk: %d row(s) merged into %d; %d untouched -> %d total.",
                    nrow(hit), nrow(merged), nrow(rest), nrow(out)))
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out[]
}


# =============================================================================
# totalbr_clean_units(d)
#
# One canonical form for the li_* lists: the values de-duplicated inside the row,
# sorted, pipe-separated.
#
# A row is NOT one unit's view. Measured on 2026-01 (API) and 2025-01 (archive),
# only ~32% of raw rows name a single unit; two thirds already name two to six.
# What is a fault is the SAME unit listed twice in one row -- "APPAN,APPAN" --
# which occurs in 1.1% of the API's rows and 2.7% of the archive's, and which
# makes any count of units per row wrong before anything is merged.
#
# The two sources also disagree on the separator (archive: comma, download: pipe
# after the CSV round-trip), so canonical form has to be imposed, not assumed.
# =============================================================================
totalbr_clean_units <- function(d, union_cols = TOTALBR_UNION_COLS, quiet = FALSE) {
  cols <- intersect(union_cols, names(d))
  if (length(cols) == 0) return(d)
  n_rep <- 0L
  for (cc in cols) {
    v <- as.character(d[[cc]])
    # only rows holding a list can hold a repeat; a single token cannot repeat
    hit <- !is.na(v) & grepl("[|,]", v)
    if (!any(hit)) next
    # the same handful of unit lists recurs across millions of rows, so the work
    # is done once per DISTINCT value and mapped back -- 180k rows go from 17s to
    # well under a second
    u   <- unique(v[hit])
    toks <- strsplit(u, "[|,]")
    fixed <- vapply(toks, function(z) {
      z <- unique(trimws(z))
      z <- z[nzchar(z)]
      if (length(z) == 0) NA_character_ else paste(sort(z), collapse = "|")
    }, character(1))
    # a repeated unit is the fault; a comma or an unsorted list is only a format
    had_rep <- lengths(toks) > vapply(toks, function(z) length(unique(trimws(z))),
                                      integer(1))
    n_rep <- n_rep + sum(had_rep[match(v[hit], u)])
    v[hit] <- fixed[match(v[hit], u)]
    d[[cc]] <- v
  }
  if (!quiet)
    message(sprintf("li_* lists: %d row-value(s) had a unit listed twice; all lists now pipe-separated, sorted, distinct.",
                    n_rep))
  d
}
