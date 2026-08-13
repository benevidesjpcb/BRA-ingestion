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
# Output: data-raw/dstaxi/dsTaxi<year>.csv, with the monthly downloads under
# data-raw/dstaxi/parts/ — the same layout as data-raw/kpi08 and
# data-raw/totalbr. Both folders are created if missing.
#
# The FILE NAME is unchanged, so the inventory, the preparation, the validation
# and the dashboard keep reading dsTaxiYYYY.csv; they find it through
# bra_taxi_file()/bra_taxi_files() in _chapter-setup.R, which look in
# data-raw/dstaxi/ first and still accept a file left directly in data-raw/.
# Years already held are inspected and only the missing months are fetched.
#
# ---------------------------------------------------------------------------
# The table is `dstaxi` — the same ODIN API as KPI08 and TOTALBR, with the table
# name swapped. Confirmed by DECEA. `odin_probe("dstaxi")` returns 16 columns:
#
#   mov, matricula, indicativo, adpartida, addestino, tipovoo, vratipolinha,
#   companhia, tipoaeronave, dhbimtra, dhvra, box, pista, matchvra, class,
#   referencia
#
# Three consequences, each handled below.
#
# 1. THE DATE COLUMN IS dhbimtra. The preparation chunk maps dh_bimtra to
#    MVT_TIME — the movement itself, landing for an arrival and take-off for a
#    departure — and dh_vra to BLOCK_TIME. Anchoring the month window on the
#    movement therefore places both phases in the month they are reported in.
#    (dhvra would put a flight blocked off at 23:50 on the 31st into the wrong
#    month, and the taxi time is computed as the gap between the two.)
#
# 2. THERE IS NO `id` AND NO `pk`. Offset pagination needs a total order, so the
#    order is built from the columns that identify a movement: the movement
#    stamp, the callsign, the phase and the block stamp. Nothing is de-duplicated
#    on merge (id_col = NULL) — with no unique key, any key we invented would
#    delete real movements, e.g. the two legs a callsign flies in one day.
#
# 3. THE API NAMES ARE NOT THE FILE NAMES. The header of the existing files is
#
#      mov, matricula, indicativo, adpartida, addestino, tipovoo,
#      vra_tipo_linha, companhia, tipoaeronave, dh_bimtra, dh_vra, box, pista,
#      match_vra                     (+ an unnamed 15th payload field, read as V15)
#
#    Four of those are spelled without the underscores by the API, and they are
#    renamed on arrival, so a downloaded year is read by exactly the same
#    preparation code as a year that came from the zip. The API's two extra
#    columns (class, referencia) simply travel along; the merge fills them with
#    NA for the years that predate them.
# ---------------------------------------------------------------------------
# =============================================================================

# the shared engine; sourcing only defines download_odin(), odin_probe(), odin_tables()
source(here::here("ODIN", "download_odin.R"))

# The study period comes from _chapter-setup.R (dsTaxi_years) when that has been
# sourced; only a session that never loaded it falls back to the current year.
taxi_default_years <- function() {
  if (exists("dsTaxi_years", inherits = TRUE)) get("dsTaxi_years", inherits = TRUE)
  else as.integer(format(Sys.Date(), "%Y"))
}

download_taxi <- function(years    = taxi_default_years(),
                          # its own folder, like data-raw/kpi08 and
                          # data-raw/totalbr; the months land in dstaxi/parts/
                          out_dir  = here::here("data-raw", "dstaxi"),
                          # confirmed by DECEA: the same ODIN API as the other
                          # datasets, with `dstaxi` in place of `total_brasil`
                          table    = Sys.getenv("TAXI_TABLE", unset = "dstaxi"),
                          # the movement stamp; see note 1 in the header
                          date_col = Sys.getenv("TAXI_DATE_COL", unset = "dhbimtra"),
                          # no unique id in this table; see note 2
                          id_col   = NULL,
                          # enough columns to break every tie in the pagination
                          order_cols = c("dhbimtra", "indicativo", "mov", "dhvra"),
                          # API name -> the name already used on disk; see note 3
                          rename_cols = c(dhbimtra     = "dh_bimtra",
                                          dhvra        = "dh_vra",
                                          matchvra     = "match_vra",
                                          vratipolinha = "vra_tipo_linha"),
                          force    = FALSE) {

  download_odin(
    table       = table,
    years       = years,
    out_dir     = out_dir,
    date_col    = date_col,
    id_col      = id_col,
    order_cols  = order_cols,
    rename_cols = rename_cols,
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
  years <- if (length(args) == 0) taxi_default_years() else args
  download_taxi(years)
}
