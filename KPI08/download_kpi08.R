#!/usr/bin/env Rscript
# =============================================================================
# download_kpi08.R
#
# Downloads KPI08 (ASMA) data from the ICEA/DECEA ODIN API into ONE FILE PER
# YEAR, fetching one month at a time and resuming whatever is already on disk.
#
# The download logic lives in ODIN/download_odin.R and is shared with every
# other ODIN table; this file only supplies what is specific to KPI08.
#
#   1) From a terminal
#        Rscript KPI08/download_kpi08.R                 # current year
#        Rscript KPI08/download_kpi08.R 2026            # one year
#        Rscript KPI08/download_kpi08.R 2024 2025 2026  # several years
#
#   2) From R / a Quarto chunk
#        source(here::here("KPI08", "download_kpi08.R"))
#        download_kpi08(2024:2026, out_dir = here::here("data-raw", "kpi08"))
#
# Output: <out_dir>/kpi08_<year>.csv, with the per-month parts under parts/.
# =============================================================================

# the shared engine; sourcing only defines download_odin()
source(here::here("ODIN", "download_odin.R"))

download_kpi08 <- function(years   = as.integer(format(Sys.Date(), "%Y")),
                           out_dir = file.path("data-raw", "kpi08"),
                           force   = FALSE) {
  download_odin(
    table     = "kpi08",
    years     = years,
    out_dir   = out_dir,
    # the arrival landing time is what the year/month windows filter on
    date_col  = Sys.getenv("ODIN_DATE_COL", unset = "aldt"),
    id_col    = Sys.getenv("ODIN_ID_COL",   unset = "id"),
    # Each arrival appears once per ASMA ring under the SAME id, so the ring must
    # be part of the de-duplication key or one whole ring is silently deleted.
    dedup_col = Sys.getenv("ODIN_RING_COL", unset = "c"),
    force     = force
  )
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args  <- commandArgs(trailingOnly = TRUE)
  years <- if (length(args) == 0) as.integer(format(Sys.Date(), "%Y")) else args
  download_kpi08(years)
  message("Done. If the year filter looks wrong, confirm ODIN_DATE_COL.")
}
