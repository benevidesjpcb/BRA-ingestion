#!/usr/bin/env Rscript
# =============================================================================
# download_taxi.R
#
# Downloads the taxi-time source from the ICEA/DECEA ODIN API into ONE FILE PER
# YEAR, fetching one month at a time and resuming whatever is already on disk.
#
# The download logic lives in ODIN/download_odin.R and is shared with KPI08 and
# TOTALBR; this file only supplies what is specific to taxi.
#
#   source(here::here("TAXI", "download_taxi.R"))
#   download_taxi(2024:2026)
#
# Output: <out_dir>/dsTaxi<year>.csv — the name the rest of the pipeline already
# reads. Nothing downstream changes: the inventory, the preparation, the
# validation and the dashboard all take their files from data-raw/ and do not
# care how they got there. Years already held as dsTaxiYYYY.csv are inspected and
# only the missing months are fetched.
#
# ---------------------------------------------------------------------------
# The table is `dstaxi` — the same ODIN API as KPI08 and TOTALBR, with the table
# name swapped. Confirmed by DECEA, so it is the default here.
#
# THE DATE COLUMN IS STILL NOT GUESSED. A wrong table announces itself with an
# HTTP error; a wrong date column returns the WRONG MONTHS with no error at all,
# and the mistake surfaces much later as a coverage hole nobody can explain. One
# request settles it:
#
#   odin_probe("dstaxi")                        # the columns, date-like flagged
#   Sys.setenv(TAXI_DATE_COL = "<column>")      # or put it in .Renviron
#
# Taxi time needs both ends of the movement, so the column that anchors a flight
# to a month has to be chosen deliberately — an arrival-only stamp would drop
# every departure from the window it belongs to.
# ---------------------------------------------------------------------------
# =============================================================================

# the shared engine; sourcing only defines download_odin(), odin_probe(), odin_tables()
source(here::here("ODIN", "download_odin.R"))

download_taxi <- function(years    = as.integer(format(Sys.Date(), "%Y")),
                          out_dir  = here::here("data-raw"),
                          # confirmed by DECEA: the same ODIN API as the other
                          # datasets, with `dstaxi` in place of `total_brasil`
                          table    = Sys.getenv("TAXI_TABLE", unset = "dstaxi"),
                          date_col = Sys.getenv("TAXI_DATE_COL", unset = ""),
                          id_col   = Sys.getenv("TAXI_ID_COL",   unset = "id"),
                          dedup_col = {
                            x <- Sys.getenv("TAXI_DEDUP_COL", unset = "")
                            if (nzchar(x)) x else NULL
                          },
                          force    = FALSE) {

  # ---- which date column? -------------------------------------------------
  if (!nzchar(date_col)) {
    message("TAXI_DATE_COL is not set. Columns in '", table, "':")
    odin_probe(table)
    stop("Pick the column the month windows should filter on and set it:\n",
         "  Sys.setenv(TAXI_DATE_COL = \"<column>\")\n",
         "  A wrong one returns the wrong months without any error.")
  }

  download_odin(
    table     = table,
    years     = years,
    out_dir   = out_dir,
    date_col  = date_col,
    id_col    = id_col,
    dedup_col = dedup_col,
    # dsTaxi2025.csv, not dsTaxi_2025.csv: the established name, so the existing
    # inventory regex and the preparation chunk keep working untouched
    prefix    = "dsTaxi",
    sep       = "",
    force     = force
  )
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args  <- commandArgs(trailingOnly = TRUE)
  years <- if (length(args) == 0) as.integer(format(Sys.Date(), "%Y")) else args
  download_taxi(years)
}
