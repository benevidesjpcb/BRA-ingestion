#!/usr/bin/env Rscript
# =============================================================================
# prepare_totalbr.R
#
# The whole TOTALBR cycle in one call: read what was downloaded, collapse the
# rows that share a pk, merge the records of each flight, and hand back a dataset
# ready to work on.
#
#   source(here::here("TOTALBR", "prepare_totalbr.R"))
#   flights <- totalbr_prepare(2026)                       # one year
#   flights <- totalbr_prepare(2026, month = 1)            # January only
#   flights <- totalbr_prepare(2026, out_file = "flights-2026.csv")
#
# THE STEPS, AND WHY EACH IS THERE
#
#   1. READ          the year from the CSVs downloaded from the API (or the
#                    parquet archive with source = "parquet").
#   2. NORMALISE     times to POSIXct in one zone, pk to one case. Without this
#                    the same row from two sources compares as two rows.
#   3. SAME pk       rows sharing a pk describe one movement -- every identity
#                    column agrees -- but carry different payloads, so they are
#                    COLLAPSED, not deleted: li_* unioned, first filled value
#                    elsewhere. Deleting would drop filled fields at random.
#   4. MERGE FLIGHTS the records of one flight — one per unit that saw it —
#                    become one row spanning from the earliest capture to the
#                    latest, listing every unit.
#
# What comes back is a tibble with the original columns plus N_MERGED, SPAN_MIN,
# MERGED_PK and ZERO_SPAN_ONLY. Renaming columns, filtering to the study
# airports, deriving metrics — all of that happens after this, on the result.
#
# NOTHING IS WRITTEN unless out_file is given, and out_file is always a NEW file:
# a download is a record of what the API served and is never edited in place.
# =============================================================================

source(here::here("TOTALBR", "totalbr_sources.R"))
source(here::here("TOTALBR", "check_totalbr_duplicates.R"))
source(here::here("TOTALBR", "merge_totalbr_duplicates.R"))

# =============================================================================
# totalbr_prepare(years, month, gap_min, source, raw_dir, out_file, quiet)
#
#   years    : one or more years, e.g. 2026 or 2024:2026
#   month    : optional month number(s) to keep, for working on a slice
#   gap_min  : how far apart two records of the same flight may be (minutes).
#              See totalbr_cluster_profile() before changing it.
#   source   : "csv" (the API download) or "parquet" (the archive)
#   out_file : optional path to write the result to; never an existing raw file
#
# Returns the prepared tibble, with the step counts attached as attr "steps".
# =============================================================================
totalbr_prepare <- function(years    = NULL,
                            month    = NULL,
                            gap_min  = TOTALBR_GAP_MIN,
                            source   = c("csv", "parquet"),
                            raw_dir  = here::here("data-raw", "totalbr"),
                            date_col = "dt_dia",
                            out_file = NULL,
                            quiet    = FALSE) {

  source <- match.arg(source)
  if (is.null(years)) years <- if (exists("totalbr_data_years", inherits = TRUE))
    get("totalbr_data_years", inherits = TRUE) else
      as.integer(format(Sys.Date(), "%Y"))

  say <- function(...) if (!quiet) message(...)

  # ---- 1. read -------------------------------------------------------------
  say("1/4 reading ", source, ": ", paste(years, collapse = ", "))
  # month goes DOWN to the reader: filtering after a year is in memory is the very
  # cost this pipeline is meant to avoid. The filter below still runs, on rows
  # that are already the right month, and stays as the guard for a date_col the
  # reader could not interpret.
  parts <- lapply(years, function(y)
    totalbr_read_year(y, month = month, source = source, raw_dir = raw_dir,
                      date_col = date_col))
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0, parts)
  if (length(parts) == 0) {
    message("Nothing to prepare: no ", source, " data for ",
            paste(years, collapse = ", "), " in ", raw_dir)
    return(tibble::tibble())
  }
  d <- dplyr::bind_rows(parts)
  n_read <- nrow(d)

  # ---- 2. normalise --------------------------------------------------------
  # totalbr_normalise() parses dh_inicio, dh_fim, dh_eobt and dh_eet - but NOT
  # dt_dia, which therefore arrives as text from a CSV and as POSIXct from the
  # parquet. Parsed here so the prepared dataset has the same types whichever
  # source it came from, and so date arithmetic below is safe: on a character
  # vector, format(x, "%m") reads "%m" as format()'s `trim` argument and fails
  # with "invalid 'trim' argument" instead of extracting a month.
  say("2/4 normalising times and pk")
  d <- totalbr_normalise(d)
  if (date_col %in% names(d) && !inherits(d[[date_col]], "POSIXt"))
    d[[date_col]] <- totalbr_parse_time(d[[date_col]])

  if (!is.null(month)) {
    # read the month off whatever the column is. format(x, "%m") only means
    # "the month" for a POSIXct/Date; on a character vector "%m" lands in
    # format()'s `trim` argument and errors. The stamps are ISO, so on text the
    # month is simply characters 6-7 — no parsing, nothing to go wrong.
    mth <- if (inherits(d[[date_col]], c("POSIXt", "Date")))
             as.integer(format(d[[date_col]], "%m"))
           else
             suppressWarnings(as.integer(substr(as.character(d[[date_col]]), 6, 7)))
    keep <- !is.na(mth) & mth %in% as.integer(month)
    d <- d[keep, , drop = FALSE]
    say("    month filter: ", nrow(d), " row(s)")
  }

  n_kept <- nrow(d)

  # ---- 3. collapse the rows that share a pk --------------------------------
  # Not a deletion. The copies are NOT interchangeable: on the parquet archive all
  # 910 rows behind 455 repeated pk stay distinct when every column is compared,
  # differing on li_orgaos and dh_eet while agreeing on every identity column.
  # Keeping "the first" would discard 162 filled dh_eet values at random.
  # canonical li_* lists BEFORE anything is grouped: a unit listed twice in one
  # row makes every later count of units wrong, and the two sources do not even
  # agree on the separator. Several DIFFERENT units in one row are normal -- two
  # thirds of raw rows carry two to six -- and are all kept.
  d <- totalbr_clean_units(d, quiet = quiet)

  say("3/4 collapsing rows that share a pk")
  d <- totalbr_merge_duplicate_pk(d, quiet = quiet)
  n_after_pk <- nrow(d)

  # ---- 4. merge the records of each flight ---------------------------------
  say("4/4 merging the records of each flight (gap_min = ", gap_min, ")")
  out <- totalbr_merge_flights(d, gap_min = gap_min)

  steps <- tibble::tibble(
    STEP  = c("read", "kept after month filter", "same pk collapsed",
              "records merged", "flights out"),
    ROWS  = c(n_read, n_kept, n_kept - n_after_pk,
              n_after_pk - nrow(out), nrow(out))
  )
  if (!quiet) {
    message("")
    print(as.data.frame(steps), row.names = FALSE)
  }

  if (!is.null(out_file)) {
    if (grepl("^totalbr_[0-9]{4}\\.csv$", basename(out_file)))
      stop("That is a raw download file name. Write the prepared data to a new ",
           "name, e.g. totalbr_2026_flights.csv.")
    data.table::fwrite(out, out_file, sep = ";", na = "", quote = TRUE)
    message("Written to ", out_file)
  }

  attr(out, "steps") <- steps
  out
}


# =============================================================================
# totalbr_prepare_months(years, months, out_dir, force)
#
# Runs totalbr_prepare() month by month and writes one file per month:
#
#   <out_dir>/totalbr-<year>-<month>-flights.csv
#
#   totalbr_prepare_months(2026)                  # every CLOSED month of 2026
#   totalbr_prepare_months(2026, months = 2)      # just February
#
# CLOSED MONTHS ONLY, by default. The current month is still receiving rows, so a
# file written from it is a snapshot that will disagree with the next one. Pass
# the month explicitly to force it.
#
# ALREADY-WRITTEN MONTHS ARE SKIPPED, so this is re-runnable: it costs nothing to
# call it again after a new month closes, and it will do only that month.
#
# Returns, invisibly, the paths written or already present.
# =============================================================================
totalbr_prepare_months <- function(years    = NULL,
                                   months   = NULL,
                                   out_dir  = here::here("outputs"),
                                   raw_dir  = here::here("data-raw", "totalbr"),
                                   source   = c("csv", "parquet"),
                                   gap_min  = TOTALBR_GAP_MIN,
                                   force    = FALSE,
                                   quiet    = FALSE) {

  source <- match.arg(source)
  if (is.null(years)) years <- if (exists("totalbr_data_years", inherits = TRUE))
    get("totalbr_data_years", inherits = TRUE) else
      as.integer(format(Sys.Date(), "%Y"))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  today     <- Sys.Date()
  this_year <- as.integer(format(today, "%Y"))
  this_mon  <- as.integer(format(today, "%m"))
  written   <- character(0)

  for (yr in years) {
    want <- if (!is.null(months)) as.integer(months) else {
      # only months that have finished: the current one is still filling up
      if (yr > this_year) integer(0)
      else if (yr == this_year) seq_len(max(this_mon - 1L, 0L))
      else 1:12
    }
    if (length(want) == 0) {
      message("Year ", yr, ": no closed month to prepare yet."); next
    }

    for (mo in want) {
      f <- file.path(out_dir, sprintf("totalbr-%d-%02d-flights.csv", yr, mo))
      if (file.exists(f) && !force) {
        message(sprintf("%d-%02d  skip (already written: %s)", yr, mo, basename(f)))
        written <- c(written, f); next
      }
      message(sprintf("%d-%02d  preparing ...", yr, mo))
      out <- totalbr_prepare(yr, month = mo, gap_min = gap_min, source = source,
                             raw_dir = raw_dir, out_file = f, quiet = quiet)
      if (nrow(out) > 0) written <- c(written, f)
    }
  }
  invisible(written)
}

# =============================================================================
# totalbr_bind_months(files, remerge)
#
# The monthly files read back as ONE dataset.
#
# WHY THIS IS NOT JUST rbind
# A flight that starts on 31 January at 23:40 and lands on 1 February has its
# records split across two months, so each month merged its own half and the
# flight appears twice, each time incomplete. Concatenating the files preserves
# that split. `remerge = TRUE` (the default) runs the flight merge once more over
# the joined data, which joins exactly those straddling records and leaves every
# other row alone -- the rule is the same, so a flight already whole cannot merge
# with anything.
#
# N_MERGED is recomputed from MERGED_PK, which is unioned across the pass, so it
# keeps meaning "how many SOURCE rows are behind this flight" rather than "how
# many rows this last pass merged".
# =============================================================================
totalbr_bind_months <- function(files    = NULL,
                                out_dir  = here::here("outputs"),
                                pattern  = "^totalbr-\\d{4}-\\d{2}-flights\\.csv$",
                                remerge  = TRUE,
                                gap_min  = TOTALBR_GAP_MIN,
                                out_file = NULL,
                                quiet    = FALSE) {

  if (is.null(files)) files <- list.files(out_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    message("No monthly files in ", out_dir); return(tibble::tibble())
  }
  files <- sort(files)
  if (!quiet) message("Binding ", length(files), " month(s): ",
                      paste(basename(files), collapse = ", "))

  parts <- lapply(files, function(f)
    tibble::as_tibble(data.table::fread(file = f, sep = ";", na.strings = "",
                                        showProgress = FALSE, fill = Inf,
                                        header = TRUE)))
  d <- dplyr::bind_rows(parts)
  # written as text by fwrite; the merge needs real timestamps back
  for (nm in intersect(c("dt_dia", "dh_inicio", "dh_fim", "dh_eobt", "dh_eet"),
                       names(d)))
    if (!inherits(d[[nm]], "POSIXt")) d[[nm]] <- totalbr_parse_time(d[[nm]])

  n_in <- nrow(d)
  if (!remerge) {
    if (!quiet) message(n_in, " flight(s), months concatenated as they were.")
    return(d)
  }

  # MERGED_PK travels as a union so provenance survives a second pass
  out <- totalbr_merge_flights(d, gap_min = gap_min,
                               union_cols = c(TOTALBR_UNION_COLS, "MERGED_PK"))
  if ("MERGED_PK" %in% names(out))
    out$N_MERGED <- lengths(strsplit(as.character(out$MERGED_PK), "|", fixed = TRUE))

  if (!quiet)
    message(sprintf("%d flight(s) in, %d out: %d straddled a month boundary.",
                    n_in, nrow(out), n_in - nrow(out)))

  if (!is.null(out_file)) {
    data.table::fwrite(out, out_file, sep = ";", na = "", quote = TRUE)
    message("Written to ", out_file)
  }
  out
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(arrow); library(data.table); library(here)
  })
  args <- commandArgs(trailingOnly = TRUE)
  totalbr_prepare(if (length(args) == 0) NULL else as.integer(args))
}
