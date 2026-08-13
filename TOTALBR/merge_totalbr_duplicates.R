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
# The extremes are taken over the REAL MOVEMENTS only. A filed plan carries an
# intention, not an event, so it must not set the start of a flight that went
# later; it still contributes dh_eobt, the registration and its unit. A group
# with no movement at all falls back to its plans and is marked PLAN_ONLY.
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
  # a row with a registration beats one without; an actual movement (a duration)
  # beats a filed plan (dh_fim == dh_inicio); ties go to the earliest.
  has_reg <- if ("co_matricula" %in% names(grp))
    !is.na(grp$co_matricula) & nzchar(trimws(as.character(grp$co_matricula))) else rep(FALSE, nrow(grp))
  is_plan <- !is.na(grp[[start_col]]) & !is.na(grp[[end_col]]) &
             grp[[start_col]] == grp[[end_col]]
  grp[, `:=`(.HAS_REG = as.integer(!has_reg),      # 0 first = has registration
             .IS_PLAN = as.integer(is_plan))]      # 0 first = real movement
  data.table::setorderv(grp, c("GRP_ID", ".HAS_REG", ".IS_PLAN", start_col))

  val_cols <- setdiff(names(grp),
                      c("GRP_ID", "ROW_ID", ".HAS_REG", ".IS_PLAN"))

  merged <- grp[, c(
    lapply(.SD, .first_filled),
    list(N_MERGED  = .N,
         MERGED_PK = if ("pk" %in% names(grp)) paste(pk, collapse = "|") else NA_character_)
  ), by = GRP_ID, .SDcols = val_cols]

  # The times are NOT "first filled": they are the extremes of the group, which is
  # the whole point of merging two partial reports of one flight.
  #
  # BUT ONLY OVER THE REAL MOVEMENTS. A filed plan (dh_fim == dh_inicio) carries
  # an INTENTION, not an event: PRATC on 2026-01-01 was filed for 05:43 and went
  # at 06:07. Letting the plan set dh_inicio makes the flight "start" 24 minutes
  # before it moved and inflates SPAN_MIN by the delay. The plan still joins the
  # merge and still contributes dh_eobt, the registration and its unit — it just
  # does not define when the flight happened.
  #
  # A group of plans only (no movement at all) falls back to the plans, because
  # then there is nothing else to date it by.
  ext <- grp[, {
    real <- .SD[.IS_PLAN == 0L]
    use  <- if (nrow(real) > 0) real else .SD
    list(.START = suppressWarnings(min(use[[start_col]], na.rm = TRUE)),
         .END   = suppressWarnings(max(use[[end_col]],   na.rm = TRUE)),
         .PLAN_ONLY = nrow(real) == 0L)
  }, by = GRP_ID, .SDcols = c(start_col, end_col, ".IS_PLAN")]
  # dt_dia follows the earliest row, not the group's own dt_dia values, so the
  # merged flight is dated by when it started
  if (day_col %in% names(grp)) {
    # same choice as the times: the day of the earliest real movement
    first_day <- grp[order(GRP_ID, .IS_PLAN, get(start_col)),
                     list(.DAY = get(day_col)[1]), by = GRP_ID]
    ext <- merge(ext, first_day, by = "GRP_ID")
  }

  merged <- merge(merged, ext, by = "GRP_ID")
  merged[, (start_col) := .START]
  merged[is.finite(.END), (end_col) := .END]
  if (day_col %in% names(merged) && ".DAY" %in% names(merged))
    merged[, (day_col) := .DAY]
  # PLAN_ONLY marks a merged row dated by a filed plan because no movement was
  # ever reported for it — worth knowing before treating its times as facts
  merged[, PLAN_ONLY := .PLAN_ONLY]
  merged[, c(".START", ".END", ".PLAN_ONLY",
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
