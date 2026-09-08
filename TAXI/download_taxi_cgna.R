#!/usr/bin/env Rscript
# =============================================================================
# download_taxi_cgna.R
#
# The taxi-time source AS THE CGNA REPORTS IT, written in the dstaxi shape and
# under a name that can never be mistaken for the ODIN one:
#
#   data-raw/dstaxi/dsTaxi<year>cgna.csv          the year
#   data-raw/dstaxi/parts/dsTaxi<year>cgna_<YYYY-MM>.csv   one file per MONTH
#
#   source(here::here("TAXI", "download_taxi_cgna.R"))
#   download_taxi_cgna(2025:2026)
#
# or as a script:
#
#   Rscript TAXI/download_taxi_cgna.R 2025
#
# ---------------------------------------------------------------------------
# WHY A WRAPPER AND NOT A DOWNLOADER. The CGNA fetching already exists, day by
# day and resumable, in API_TATIC/download_tatic.R. Writing a second one that
# hits the same endpoint would give two things to keep in step and two ways to
# be wrong about a missing day. So this file downloads through that one and then
# does the only part that is specific to taxi: turning a TATIC record into a
# dstaxi row, month by month.
#
# WHY THE dstaxi COLUMN NAMES. TAXI/reproduce_txxt.R, the comparison and the
# dashboard all read `mov`, `dh_bimtra`, `dh_vra`, `box`, `pista`, ... A CGNA
# year written in that header is read by the code that already exists, and the
# two sources can be compared row against row without a translation step in
# between.
#
# WHAT MAPS TO WHAT, and where the taxi time comes from
#   mov          <- PHASE            Dep / Arr
#   dh_bimtra    <- MOV_TIME         the runway event: take-off (Dep), landing (Arr)
#   dh_vra       <- BLOCK_TIME       off-blocks `cPush` (Dep), on-blocks `cPos` (Arr)
#   indicativo   <- Callsign         pista <- Runway     tipoaeronave <- AcftType
#   adpartida    <- Adep             addestino <- Ades
# so TXXT = dh_bimtra - dh_vra on a departure and the reverse on an arrival --
# exactly the ODIN convention, computed by the same txxt_fn().
#
# THE ONE COLUMN THE CGNA CANNOT FILL: `box` (the stand). It is not in the TATIC
# field list -- the slide sources stands from BI/SINTESE -- so it is written
# empty rather than guessed. That matters downstream: the PBWG reference key is
# (ICAO, PHASE, STND, RWY), so a CGNA year produces NO reference groups on its
# own. Use these files to CROSS-CHECK the ODIN taxi times; making them a
# productive txxt source needs a stand from somewhere, or a reference key
# without one, which is a different metric from the golden files.
# `matricula`, `tipovoo`, `vra_tipo_linha`, `companhia` and `match_vra` are
# empty for the same reason: the source does not carry them.
# =============================================================================

source(here::here("API_TATIC", "download_tatic.R"))
source(here::here("API_TATIC", "harmonise_tatic.R"))

# the dstaxi header, in the order the existing files use it
TAXI_CGNA_COLS <- c("mov", "matricula", "indicativo", "adpartida", "addestino",
                    "tipovoo", "vra_tipo_linha", "companhia", "tipoaeronave",
                    "dh_bimtra", "dh_vra", "box", "pista", "match_vra")

# stamps are written the way the dstaxi reader parses them ("ymd HMS"); a
# format that only looks right returns NA silently downstream
.taxi_cgna_stamp <- function(x) {
  out <- format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  out[is.na(x)] <- NA_character_
  out
}

# ---- one month of TATIC records -> dstaxi rows -------------------------------
taxi_cgna_shape <- function(d) {
  h <- harmonise_tatic(d, quiet = TRUE)
  if (nrow(h) == 0) return(NULL)
  # a row with no phase has no taxi time and no place in either file
  h <- h[!is.na(h$PHASE), , drop = FALSE]
  if (nrow(h) == 0) return(NULL)
  data.frame(
    mov            = ifelse(h$PHASE == "DEP", "Dep", "Arr"),
    matricula      = NA_character_,
    indicativo     = h$FLTID,
    adpartida      = h$ADEP,
    addestino      = h$ADES,
    tipovoo        = NA_character_,
    vra_tipo_linha = NA_character_,
    companhia      = NA_character_,
    tipoaeronave   = h$ARCTYP,
    dh_bimtra      = .taxi_cgna_stamp(h$MOV_TIME),
    dh_vra         = .taxi_cgna_stamp(h$BLOCK_TIME),
    box            = NA_character_,     # not in the CGNA feed -- see the header
    pista          = h$RWY,
    match_vra      = NA_character_,
    stringsAsFactors = FALSE
  )[, TAXI_CGNA_COLS]
}

# =============================================================================
# download_taxi_cgna(years, from, to, out_dir, force, download)
#
#   years    : years to build, e.g. 2025 or 2025:2026
#   from/to  : optional "YYYYMMDD" bounds inside those years
#   out_dir  : default data-raw/dstaxi -- alongside the ODIN files
#   force    : TRUE re-fetches every day and rebuilds every month
#   download : FALSE rebuilds the taxi files from the TATIC months already on
#              disk, without touching the API
#
# Returns, invisibly, the year files written.
# =============================================================================
download_taxi_cgna <- function(years    = tatic_default_years(),
                               from     = NULL,
                               to       = NULL,
                               out_dir  = here::here("data-raw", "dstaxi"),
                               tatic_dir = here::here("data-raw", "tatic"),
                               force    = FALSE,
                               download = TRUE) {

  years <- suppressWarnings(as.integer(years))
  if (length(years) == 0 || any(is.na(years)))
    stop("Years must be 4-digit numbers, e.g. 2025 or 2025:2026.")

  # ---- 1. the days, through the CGNA downloader ---------------------------
  # It fetches only what is missing, day by day, and reports the days the source
  # has nothing for. Nothing about that is taxi-specific, so nothing is repeated
  # here.
  if (download) download_tatic(years = years, from = from, to = to,
                               out_dir = tatic_dir, force = force)

  parts_dir <- file.path(out_dir, "parts")
  for (d in c(out_dir, parts_dir))
    if (!dir.exists(d)) { dir.create(d, recursive = TRUE); message("Created ", d) }

  tatic_parts <- file.path(tatic_dir, "parts")
  today   <- Sys.Date()
  written <- character(0)

  for (yr in years) {
    src <- list.files(tatic_parts,
                      pattern = sprintf("^tatic_%d-[0-9]{2}\\.csv$", yr),
                      full.names = TRUE)
    if (length(src) == 0) {
      message(sprintf("Year %d: no CGNA month on disk; nothing to build.", yr))
      next
    }

    # ---- 2. one taxi part per month --------------------------------------
    # A month is rebuilt when its TATIC part is newer than the taxi part it
    # produced, so a resumed download that added days to a month is picked up
    # and the other eleven months are not re-read.
    for (f in sort(src)) {
      ym  <- sub("^tatic_", "", tools::file_path_sans_ext(basename(f)))
      dst <- file.path(parts_dir, sprintf("dsTaxi%dcgna_%s.csv", yr, ym))
      fresh <- !force && file.exists(dst) &&
        file.info(dst)$mtime >= file.info(f)$mtime
      if (fresh) {
        message(sprintf("  %s  skip (up to date)", ym)); next
      }
      shaped <- taxi_cgna_shape(tatic_read_csv(f))
      if (is.null(shaped)) {
        message(sprintf("  %s  no usable movement", ym))
        if (file.exists(dst)) unlink(dst)
        next
      }
      shaped <- shaped[order(shaped$dh_bimtra, shaped$indicativo), , drop = FALSE]
      tatic_write_csv(shaped, dst)
      message(sprintf("  %s  %d movement(s)", ym, nrow(shaped)))
    }

    # ---- 3. the months merged into the year ------------------------------
    got <- list.files(parts_dir,
                      pattern = sprintf("^dsTaxi%dcgna_%d-[0-9]{2}\\.csv$", yr, yr),
                      full.names = TRUE)
    if (length(got) == 0) {
      message(sprintf("Year %d: no month produced a movement; nothing written.", yr))
      next
    }
    combined <- tatic_rbind_fill(lapply(sort(got), tatic_read_csv))
    combined <- combined[order(combined$dh_bimtra, combined$indicativo), , drop = FALSE]

    out_csv <- file.path(out_dir, sprintf("dsTaxi%dcgna.csv", yr))
    tatic_write_csv(combined, out_csv)
    written <- c(written, out_csv)
    message(sprintf("Year %d: merged %d month(s) -> %d movement(s) -> %s",
                    yr, length(got), nrow(combined), out_csv))

    # ---- 4. which months are missing, and how complete the rest are -------
    # Judged by the DAYS a month holds, never by a file existing: a month
    # downloaded while it was still the current one is a partial month, and a
    # file that is merely present would otherwise be skipped forever.
    last_month <- if (yr == as.integer(format(today, "%Y")))
      as.integer(format(today, "%m")) else 12L
    have  <- as.integer(substr(sub(".*_", "", basename(sort(got))), 6, 7))
    never <- setdiff(seq_len(last_month), have)
    if (length(never) > 0)
      message(sprintf("  NOTE: month(s) with no data at all: %s",
                      paste(sprintf("%02d", never), collapse = ", ")))

    days  <- as.Date(substr(combined$dh_bimtra, 1, 10))
    partial <- character(0)
    for (mo in have) {
      first <- as.Date(sprintf("%d-%02d-01", yr, mo))
      last  <- min(seq(first, by = "month", length.out = 2)[2] - 1, today - 1)
      n_exp <- as.integer(last - first) + 1L
      n_got <- length(unique(days[!is.na(days) & days >= first & days <= last]))
      if (n_got < n_exp - 1L)
        partial <- c(partial, sprintf("%02d (%d/%d days)", mo, n_got, n_exp))
    }
    if (length(partial) > 0)
      message(sprintf("  NOTE: incomplete month(s): %s",
                      paste(partial, collapse = ", ")))
  }

  invisible(written)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) download_taxi_cgna()
  else download_taxi_cgna(years = as.integer(args[1]),
                          from  = if (length(args) >= 2) args[2] else NULL,
                          to    = if (length(args) >= 3) args[3] else NULL)
}
