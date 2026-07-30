#!/usr/bin/env Rscript
# =============================================================================
# download_totalbr.R
#
# Downloads the TOTALBR dataset from the ICEA/DECEA ODIN API into ONE FILE PER
# YEAR, fetching one month at a time and resuming whatever is already on disk.
#
# The download logic lives in ODIN/download_odin.R and is shared with every
# other ODIN table; this file only supplies what is specific to TOTALBR.
#
#   1) From a terminal
#        Rscript TOTALBR/download_totalbr.R                 # current year
#        Rscript TOTALBR/download_totalbr.R 2026            # one year
#        Rscript TOTALBR/download_totalbr.R 2024 2025 2026  # several years
#
#   2) From R / a Quarto chunk
#        source(here::here("TOTALBR", "download_totalbr.R"))
#        download_totalbr(2024:2026, out_dir = here::here("data-raw", "totalbr"))
#
# Output: <out_dir>/totalbr_<year>.csv, with the per-month parts under parts/.
#
# Earlier years already held as files can simply be dropped into <out_dir> with
# the same name; the downloader inspects what each file covers and only fetches
# the months that are missing or incomplete.
#
# ---------------------------------------------------------------------------
# Two things about this table that the defaults below encode:
#
#   The unique key is `pk`, NOT `id`. `id` is the callsign plus the airport pair
#   (e.g. PPBLVSBPASBSM), so it repeats on every flight of that route — the same
#   id appears on 2019-03-12 and again on 2019-05-19. De-duplicating on `id`
#   would delete almost every record.
#
#   Several columns are JSON arrays (li_orgaos, li_tipovoo, li_regravoo,
#   li_prnav, li_rvsm). The shared engine collapses them to pipe-separated
#   strings so they survive the CSV; split them again on read if needed.
# ---------------------------------------------------------------------------
# =============================================================================

# the shared engine; sourcing only defines download_odin()
source(here::here("ODIN", "download_odin.R"))

download_totalbr <- function(years   = as.integer(format(Sys.Date(), "%Y")),
                             out_dir = file.path("data-raw", "totalbr"),
                             force   = FALSE) {
  dedup <- Sys.getenv("TOTALBR_DEDUP_COL", unset = "")
  download_odin(
    table     = "totalbr",
    years     = years,
    out_dir   = out_dir,
    date_col  = Sys.getenv("TOTALBR_DATE_COL", unset = "dt_dia"),
    # `pk` is the row hash; `id` repeats across flights of the same route
    id_col    = Sys.getenv("TOTALBR_ID_COL",   unset = "pk"),
    # NULL means "the id alone identifies a row", which is the usual case
    dedup_col = if (nzchar(dedup)) dedup else NULL,
    force     = force
  )
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args  <- commandArgs(trailingOnly = TRUE)
  years <- if (length(args) == 0) as.integer(format(Sys.Date(), "%Y")) else args
  download_totalbr(years)
  message("Done. If a month came back empty, confirm TOTALBR_DATE_COL (dt_dia).")
}
