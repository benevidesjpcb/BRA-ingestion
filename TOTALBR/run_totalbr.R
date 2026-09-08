#!/usr/bin/env Rscript
# =============================================================================
# run_totalbr.R
#
# The TOTALBR cycle end to end, up to a cut-off date, in one call:
#
#   download  ->  check duplicates  ->  prepare month by month
#             ->  re-check duplicate flights  ->  bind the months
#
#   source(here::here("TOTALBR", "run_totalbr.R"))
#   run_totalbr(through = "2026-06-30")          # everything up to 30 June 2026
#   run_totalbr(through = "2026-07-31")          # in August, adds only July
#
# From a terminal:
#   Rscript TOTALBR/run_totalbr.R 2026-06-30
#
# WHY A CUT-OFF INSTEAD OF A YEAR
# The study period ends inside a year. A year is the unit the DOWNLOAD works in
# (one file per year, months fetched as they close), but the unit of the PRODUCT
# is the month, and the period stops at a month boundary. `through` is therefore
# read as "every month that ENDS on or before this date": 2026-06-30 gives
# January..June 2026, and never a half-open June that would disagree with the
# next run.
#
# EVERY STEP IS RE-RUNNABLE. The download resumes, a month already prepared is
# skipped, and the bind simply reads whatever months exist. When July closes,
# the same call downloads July, prepares July, and re-binds — no argument change.
#
# WHAT IS WRITTEN
#   outputs/totalbr-<yyyy>-<mm>-flights.csv        one product per month
#   outputs/totalbr-flights-<from>-<through>.csv   the months bound together
#   outputs/totalbr-duplicates-<through>.csv       the duplication report
# =============================================================================

source(here::here("TOTALBR", "download_totalbr.R"))
source(here::here("TOTALBR", "prepare_totalbr.R"))

# =============================================================================
# totalbr_months_through(years, through)
#
# The closed months of `years` that end on or before `through`, as a list of
# month numbers per year. A month is included only when its LAST DAY is within
# the cut-off — a month still receiving rows would produce a file that the next
# run disagrees with.
# =============================================================================
totalbr_months_through <- function(years, through) {
  through <- as.Date(through)
  stats::setNames(lapply(years, function(y) {
    # first day of each month of the year, and the day the month ends
    starts <- as.Date(sprintf("%d-%02d-01", y, 1:12))
    ends   <- seq(starts[1], by = "month", length.out = 13)[-1] - 1
    which(ends <= through)
  }), as.character(years))
}

# =============================================================================
# run_totalbr(through, years, download, gap_min, ...)
#
#   through  : cut-off date, e.g. "2026-06-30". Months ending after it are left
#              for a later run.
#   years    : years to cover. Defaults to totalbr_data_years from
#              _chapter-setup.R, trimmed to the years the cut-off reaches.
#   download : FALSE skips the API and works on what is already in data-raw/.
#   check    : FALSE skips the duplication report (it re-reads every file).
#   force    : re-prepare months already written.
#
# Returns, invisibly, a list: the monthly paths, the bound tibble, and the
# duplication report.
# =============================================================================
run_totalbr <- function(through  = "2026-06-30",
                        years    = NULL,
                        download = TRUE,
                        check    = TRUE,
                        force    = FALSE,
                        gap_min  = TOTALBR_GAP_MIN,
                        raw_dir  = here::here("data-raw", "totalbr"),
                        out_dir  = here::here("outputs"),
                        quiet    = FALSE) {

  through <- as.Date(through)
  if (is.null(years)) years <- if (exists("totalbr_data_years", inherits = TRUE))
    get("totalbr_data_years", inherits = TRUE) else
      as.integer(format(through, "%Y"))
  # a year that starts after the cut-off has nothing to contribute
  years <- years[years <= as.integer(format(through, "%Y"))]
  if (length(years) == 0) stop("No year of the study period ends before ", through)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  want <- totalbr_months_through(years, through)
  message("TOTALBR through ", through, " — ",
          paste(sprintf("%s: %d month(s)", names(want), lengths(want)),
                collapse = ", "))

  # ---- 1. download ---------------------------------------------------------
  # The engine fetches whole years month by month and refetches a month that was
  # still open when it was last seen, so asking for the year is right even when
  # the period stops in June: the months after the cut-off simply are not
  # prepared below.
  if (download) {
    message("\n== 1/4 download ==")
    download_totalbr(years, out_dir = raw_dir)
  } else {
    message("\n== 1/4 download skipped (download = FALSE) ==")
  }

  # ---- 2. measure the duplication in what arrived --------------------------
  # BEFORE preparing, and on the API's own files ("csv"), because the archive and
  # the API are different producers with different faults. This step reports; it
  # never repairs — which copy to keep is DECEA's decision.
  dup <- NULL
  if (check) {
    message("\n== 2/4 duplication in the raw download ==")
    dup <- check_totalbr_duplicates(years = years, source = "csv",
                                    raw_dir = raw_dir)
    if (nrow(dup) > 0) {
      f <- file.path(out_dir, sprintf("totalbr-duplicates-%s.csv", through))
      data.table::fwrite(dup, f, sep = ";", na = "", quote = TRUE)
      message("Duplication report written to ", f)
      if (!quiet) print(as.data.frame(dup), row.names = FALSE)
    }
  } else {
    message("\n== 2/4 duplication check skipped (check = FALSE) ==")
  }

  # ---- 3. the monthly product ----------------------------------------------
  # One file per month. totalbr_prepare() collapses the rows sharing a pk and
  # merges the records of each flight, so the cleaning happens here, per month,
  # on the rows the month actually holds.
  message("\n== 3/4 preparing one file per month ==")
  monthly <- character(0)
  for (y in years) {
    mo <- want[[as.character(y)]]
    if (length(mo) == 0) { message("Year ", y, ": no month within the cut-off."); next }
    monthly <- c(monthly,
                 totalbr_prepare_months(y, months = mo, out_dir = out_dir,
                                        raw_dir = raw_dir, gap_min = gap_min,
                                        force = force, quiet = quiet))
  }
  if (length(monthly) == 0) {
    message("No monthly file produced — nothing downloaded for these months yet.")
    return(invisible(list(monthly = character(0), flights = tibble::tibble(),
                          duplicates = dup)))
  }

  # ---- 4. bind the months --------------------------------------------------
  # remerge = TRUE joins the flight whose records straddle a month boundary — it
  # was split in two incomplete halves by the per-month preparation, and only a
  # pass over the joined data can put it back together.
  message("\n== 4/4 binding the months ==")
  from <- sprintf("%d-01-01", min(years))
  out_file <- file.path(out_dir, sprintf("totalbr-flights-%s-%s.csv", from, through))
  flights <- totalbr_bind_months(files = monthly, gap_min = gap_min,
                                 remerge = TRUE, out_file = out_file,
                                 quiet = quiet)

  # A last look at the joined product: how many source rows sit behind each
  # flight. Everything at N_MERGED == 1 was seen by one unit only; the tail is
  # where a flight was reported by several, which is normal, and where a genuine
  # duplicate would still show.
  if (nrow(flights) > 0 && "N_MERGED" %in% names(flights)) {
    message("\nRecords behind each flight (N_MERGED):")
    print(as.data.frame(
      dplyr::count(flights, N_MERGED, name = "FLIGHTS")), row.names = FALSE)
  }

  invisible(list(monthly = monthly, flights = flights, duplicates = dup))
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(arrow); library(data.table); library(here)
  })
  args <- commandArgs(trailingOnly = TRUE)
  run_totalbr(through = if (length(args) == 0) "2026-06-30" else args[1])
}
