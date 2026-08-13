#!/usr/bin/env Rscript
# =============================================================================
# merge_totalbr_duplicates.R
#
# COLLAPSES the duplicate rows of a flight into ONE row, instead of discarding
# them. The two are different operations and both are legitimate:
#
#   totalbr_dedupe()             keeps one row, drops the other  (throws away)
#   totalbr_merge_duplicates()   fuses them into a single row    (keeps everything)
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
                                     day_col   = "dt_dia") {

  dt <- data.table::as.data.table(d)
  n  <- nrow(dt)
  if (!nrow(pairs)) {
    dt[, `:=`(N_MERGED = 1L, MERGED_PK = if ("pk" %in% names(dt)) pk else NA_character_)]
    return(dt[])
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

  # the times are NOT "first filled": they are the extremes of the group, which
  # is the whole point of merging two partial reports of one flight
  ext <- grp[, list(
    .START = suppressWarnings(min(get(start_col), na.rm = TRUE)),
    .END   = suppressWarnings(max(get(end_col), na.rm = TRUE))
  ), by = GRP_ID]
  # dt_dia follows the earliest row, not the group's own dt_dia values, so the
  # merged flight is dated by when it started
  if (day_col %in% names(grp)) {
    first_day <- grp[order(GRP_ID, get(start_col)),
                     list(.DAY = get(day_col)[1]), by = GRP_ID]
    ext <- merge(ext, first_day, by = "GRP_ID")
  }

  merged <- merge(merged, ext, by = "GRP_ID")
  merged[, (start_col) := .START]
  merged[is.finite(.END), (end_col) := .END]
  if (day_col %in% names(merged) && ".DAY" %in% names(merged))
    merged[, (day_col) := .DAY]
  merged[, c(".START", ".END", intersect(".DAY", names(merged))) := NULL]

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
  out[]
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
  data.table::rbindlist(list(src, res), use.names = TRUE,
                        fill = TRUE)[order(get(start_col))][]
}
