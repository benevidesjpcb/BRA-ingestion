#!/usr/bin/env Rscript
# =============================================================================
# prepare_totalbr.R
#
# The whole TOTALBR cycle in one call: read what was downloaded, remove the one
# kind of row that is genuinely a repeat, merge the records of each flight, and
# hand back a dataset ready to work on.
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
#   3. DROP SAME pk  the only deletion in the pipeline. `pk` is the row hash, so
#                    an identical pk is the same row stored twice.
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
                            gap_min  = 45,
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
  parts <- lapply(years, function(y)
    totalbr_read_year(y, source = source, raw_dir = raw_dir, date_col = date_col))
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0, parts)
  if (length(parts) == 0) {
    message("Nothing to prepare: no ", source, " data for ",
            paste(years, collapse = ", "), " in ", raw_dir)
    return(tibble::tibble())
  }
  d <- dplyr::bind_rows(parts)

  if (!is.null(month)) {
    keep <- as.integer(format(d[[date_col]], "%m")) %in% as.integer(month)
    d <- d[keep, , drop = FALSE]
    say("    month filter: ", nrow(d), " row(s)")
  }
  n_read <- nrow(d)

  # ---- 2. normalise --------------------------------------------------------
  say("2/4 normalising times and pk")
  d <- totalbr_normalise(d)

  # ---- 3. the only deletion ------------------------------------------------
  say("3/4 dropping rows whose pk was already seen")
  d <- totalbr_drop_duplicate_pk(d, quiet = quiet)
  n_after_pk <- nrow(d)

  # ---- 4. merge the records of each flight ---------------------------------
  say("4/4 merging the records of each flight (gap_min = ", gap_min, ")")
  out <- totalbr_merge_flights(d, gap_min = gap_min)

  steps <- tibble::tibble(
    STEP  = c("read", "same pk dropped", "records merged", "flights out"),
    ROWS  = c(n_read, n_read - n_after_pk,
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

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(arrow); library(data.table); library(here)
  })
  args <- commandArgs(trailingOnly = TRUE)
  totalbr_prepare(if (length(args) == 0) NULL else as.integer(args))
}
