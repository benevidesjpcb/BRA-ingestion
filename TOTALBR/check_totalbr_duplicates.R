#!/usr/bin/env Rscript
# =============================================================================
# check_totalbr_duplicates.R
#
# ICEA/DECEA have reported that the ODIN data contains duplication. This script
# measures it, at four levels, because "duplicate" means four different things
# and they have different causes and different fixes:
#
#   L1  key       same `pk` twice. The pk is the row hash, so this is the source
#                 handing back the SAME ROW more than once. Read from the
#                 per-month parts, NOT the year file: the merge de-duplicates on
#                 pk, so by then the evidence is gone.
#   L2  record    same id + the same three timestamps, under DIFFERENT pk. The
#                 same movement stored twice with different hashes — the source
#                 does not recognise them as the same row, so nothing downstream
#                 will either.
#   L3  movement  same id at the same dt_dia, other fields free to differ. A
#                 movement recorded twice with a detail edited between them.
#   L4  daily     same callsign + route on the same calendar day. NOT duplication
#                 on its own: an aircraft can genuinely fly a route twice a day.
#                 This is the candidate pool, and the minute gap is what tells a
#                 second rotation from a repeat — see totalbr_duplicate_examples().
#
# Reading all four together is what makes the answer usable: L1 > 0 is a delivery
# problem, L2/L3 > 0 is a data problem upstream, and L4 alone is normal traffic.
#
#   source(here::here("TOTALBR", "check_totalbr_duplicates.R"))
#   check_totalbr_duplicates()
#   totalbr_duplicate_examples("movement", n = 5)
#
# One thing this deliberately does NOT do: repair anything. De-duplicating a
# national movement table changes every count computed from it, so the decision
# is DECEA's to make, not this script's to take silently.
# =============================================================================

source(here::here("TOTALBR", "totalbr_sources.R"))

# The unit separator: a byte that cannot occur inside these fields, so pasting
# the key parts together cannot accidentally make two different keys collide.
TOTALBR_KEY_SEP <- "\u001f"

# The four levels. `day = TRUE` means the level needs a derived calendar day.
totalbr_dup_levels <- list(
  key      = list(keys = c("pk"),
                  label = "identical pk (the same row returned twice)",
                  day = FALSE, parts = TRUE),
  record   = list(keys = c("id", "dt_dia", "dh_inicio", "dh_fim"),
                  label = "identical record under a different pk",
                  day = FALSE, parts = FALSE),
  movement = list(keys = c("id", "dt_dia"),
                  label = "same flight id at the same timestamp",
                  day = FALSE, parts = FALSE),
  daily    = list(keys = c("co_indicativo", "co_addep", "co_addes", "DAY"),
                  label = "same callsign and route on the same day (candidates)",
                  day = TRUE, parts = FALSE)
)

# ---- duplicate groups in one source -----------------------------------------
# Returns one row per YEAR: how many key values repeat, and how many rows are
# the surplus copies. Counting is pushed into arrow for the parquet, so a 1 GB
# archive is grouped without being read into memory.
totalbr_dup_in_source <- function(path, kind, keys, need_day, date_col) {

  # every key part as text, joined into one column: grouping on a single symbol
  # is the form both arrow and data.table support without surprises
  key_expr <- rlang::expr(paste(!!!lapply(keys, function(k)
                                  rlang::expr(as.character(!!rlang::sym(k)))),
                                sep = !!TOTALBR_KEY_SEP))

  if (kind == "parquet") {
    ds   <- arrow::open_dataset(path)
    cols <- setdiff(unique(c(keys, date_col)), "DAY")
    if (!all(cols %in% names(ds))) return(NULL)
    out <- tryCatch({
      q <- dplyr::select(ds, dplyr::all_of(cols))
      q <- totalbr_add_year_day(q, date_col, totalbr_is_text_date(ds, date_col),
                                day = need_day)
      q |>
        dplyr::mutate(KEY = !!key_expr) |>
        dplyr::group_by(YEAR, KEY) |>
        dplyr::summarise(N = dplyr::n(), .groups = "drop") |>
        dplyr::filter(N > 1) |>
        dplyr::collect()
    }, error = function(e) {
      message("  (cannot group ", paste(keys, collapse = "+"), " in ",
              basename(path), ": ", conditionMessage(e), ")")
      NULL
    })
    if (is.null(out)) return(NULL)
  } else {
    # `path` may be SEVERAL csv files, and they are read together on purpose: a
    # key repeated ACROSS two month files — which is what an overlapping request
    # window would produce — is invisible to a per-file check.
    cols <- setdiff(unique(c(keys, date_col)), "DAY")
    read_one <- function(f) {
      head1 <- data.table::fread(file = f, sep = ";", nrows = 0,
                                 showProgress = FALSE)
      if (!all(cols %in% names(head1))) return(NULL)
      d <- data.table::fread(file = f, sep = ";", select = cols,
                             colClasses = "character", na.strings = "",
                             showProgress = FALSE)
      if (nrow(d) == 0) NULL else tibble::as_tibble(d)
    }
    d <- dplyr::bind_rows(Filter(Negate(is.null), lapply(path, read_one)))
    if (nrow(d) == 0) return(NULL)
    d$YEAR <- substr(d[[date_col]], 1, 4)
    if (need_day) d$DAY <- substr(d[[date_col]], 1, 10)
    out <- d |>
      dplyr::mutate(KEY = !!key_expr) |>
      dplyr::count(YEAR, KEY, name = "N") |>
      dplyr::filter(N > 1)
  }

  if (nrow(out) == 0)
    return(tibble::tibble(YEAR = character(0), GROUPS = integer(0),
                          EXTRA_ROWS = integer(0)))

  out |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(
      GROUPS = dplyr::n(),
      # the surplus: a key seen 3 times contributes 2 extra rows, not 3
      EXTRA_ROWS = sum(N - 1L),
      .groups = "drop"
    )
}

# =============================================================================
# check_totalbr_duplicates(levels, raw_dir, date_col)
#
# The summary table: per level and year, the repeated key values, the surplus
# rows, and what share of the year that surplus is.
# =============================================================================
check_totalbr_duplicates <- function(levels   = names(totalbr_dup_levels),
                                     raw_dir  = here::here("data-raw", "totalbr"),
                                     date_col = "dt_dia") {

  src   <- totalbr_sources(raw_dir, include_parts = TRUE)
  # the denominator, so a surplus can be read as a percentage rather than a
  # bare number nobody can size
  totals <- totalbr_day_counts(raw_dir, date_col) |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(TOTAL_ROWS = sum(MOVEMENTS), .groups = "drop")

  res <- purrr::map(levels, function(lv) {
    spec <- totalbr_dup_levels[[lv]]
    if (is.null(spec)) stop("Unknown level '", lv, "'. Known: ",
                            paste(names(totalbr_dup_levels), collapse = ", "))

    # The pk level must read the per-month parts: the year file was already
    # de-duplicated on pk when the months were merged, so asking it whether the
    # API repeated a pk can only ever answer "no".
    paths <- if (isTRUE(spec$parts) && length(src$parts) > 0)
      list(parquet = src$parquet, csv = src$parts)
    else
      list(parquet = src$parquet, csv = src$csv)

    message("Level '", lv, "': ", spec$label)
    parts <- c(
      lapply(paths$parquet, function(p)
        totalbr_dup_in_source(p, "parquet", spec$keys, spec$day, date_col)),
      # all the CSVs in ONE call, so a key repeated across files is caught
      if (length(paths$csv) > 0)
        list(totalbr_dup_in_source(paths$csv, "csv", spec$keys, spec$day, date_col))
      else NULL
    )
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) return(NULL)

    dplyr::bind_rows(parts) |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(GROUPS = sum(GROUPS), EXTRA_ROWS = sum(EXTRA_ROWS),
                       .groups = "drop") |>
      dplyr::mutate(LEVEL = lv, WHAT = spec$label,
                    SOURCE = if (isTRUE(spec$parts)) "month parts" else "merged files")
  }) |> purrr::list_rbind()

  if (is.null(res) || nrow(res) == 0)
    return(tibble::tibble(LEVEL = character(0), YEAR = character(0),
                          GROUPS = integer(0), EXTRA_ROWS = integer(0),
                          PCT = numeric(0), WHAT = character(0)))

  res |>
    dplyr::left_join(totals, by = "YEAR") |>
    dplyr::mutate(PCT = round(100 * EXTRA_ROWS / TOTAL_ROWS, 3)) |>
    dplyr::select(LEVEL, WHAT, SOURCE, YEAR, GROUPS, EXTRA_ROWS, TOTAL_ROWS, PCT) |>
    dplyr::arrange(match(LEVEL, names(totalbr_dup_levels)), YEAR)
}

# =============================================================================
# totalbr_duplicate_examples(level, n, ...)
#
# The actual rows behind the most-repeated keys of a level, side by side, so the
# duplication can be READ rather than counted. For the `daily` level it also
# reports the minute gap between the repeats, which is the thing that separates
# a genuine second rotation from the same flight stored twice.
#
# Reads only the rows belonging to the sampled keys, so it stays cheap.
# =============================================================================
totalbr_duplicate_examples <- function(level    = "movement",
                                       n        = 5L,
                                       raw_dir  = here::here("data-raw", "totalbr"),
                                       date_col = "dt_dia") {
  spec <- totalbr_dup_levels[[level]]
  if (is.null(spec)) stop("Unknown level '", level, "'.")

  src   <- totalbr_sources(raw_dir, include_parts = TRUE)
  paths <- if (isTRUE(spec$parts) && length(src$parts) > 0)
    c(src$parquet, src$parts) else c(src$parquet, src$csv)
  if (length(paths) == 0) return(tibble::tibble())

  key_expr <- rlang::expr(paste(!!!lapply(spec$keys, function(k)
                                  rlang::expr(as.character(!!rlang::sym(k)))),
                                sep = !!TOTALBR_KEY_SEP))

  # a single source is enough for examples; the first one that has the columns
  for (path in paths) {
    is_parq <- grepl("\\.parquet$", path)
    d <- tryCatch({
      if (is_parq) {
        ds <- arrow::open_dataset(path)
        if (!all(setdiff(spec$keys, "DAY") %in% names(ds))) next
        q  <- totalbr_add_year_day(ds, date_col,
                                   totalbr_is_text_date(ds, date_col),
                                   day = spec$day)
        # the repeated keys first, then only their rows
        dup <- q |> dplyr::mutate(KEY = !!key_expr) |>
          dplyr::group_by(KEY) |> dplyr::summarise(N = dplyr::n(), .groups = "drop") |>
          dplyr::filter(N > 1) |> dplyr::arrange(dplyr::desc(N)) |>
          head(n) |> dplyr::collect()
        if (nrow(dup) == 0) next
        q |> dplyr::mutate(KEY = !!key_expr) |>
          dplyr::filter(KEY %in% dup$KEY) |> dplyr::collect()
      } else {
        raw <- data.table::fread(file = path, sep = ";", colClasses = "character",
                                 na.strings = "", showProgress = FALSE)
        if (!all(setdiff(spec$keys, "DAY") %in% names(raw))) next
        raw <- tibble::as_tibble(raw)
        if (spec$day) raw$DAY <- substr(raw[[date_col]], 1, 10)
        raw <- dplyr::mutate(raw, KEY = !!key_expr)
        dup <- raw |> dplyr::count(KEY, name = "N") |> dplyr::filter(N > 1) |>
          dplyr::arrange(dplyr::desc(N)) |> head(n)
        if (nrow(dup) == 0) next
        dplyr::filter(raw, KEY %in% dup$KEY)
      }
    }, error = function(e) NULL)

    if (is.null(d) || nrow(d) == 0) next

    d <- d |>
      dplyr::arrange(KEY, .data[[date_col]]) |>
      dplyr::group_by(KEY) |>
      dplyr::mutate(
        COPY = dplyr::row_number(),
        # minutes since the previous copy of the same key: near zero means the
        # same movement stored twice, hours mean a genuine second rotation
        GAP_MIN = round(as.numeric(difftime(
          as.POSIXct(.data[[date_col]], tz = "UTC"),
          dplyr::lag(as.POSIXct(.data[[date_col]], tz = "UTC")),
          units = "mins")), 1)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-KEY)

    message("Examples from ", basename(path), " (level '", level, "')")
    return(d)
  }

  message("No duplicates found at level '", level, "'.")
  tibble::tibble()
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(arrow); library(data.table); library(here)
  })
  print(as.data.frame(check_totalbr_duplicates()))
}
