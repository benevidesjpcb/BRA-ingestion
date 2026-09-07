#!/usr/bin/env Rscript
# =============================================================================
# export_asma_pbwg.R
#
# The PBWG ASMA deliverable: one file per year, in the columns PBWG asks for.
#
#   source(here::here("KPI08", "export_asma_pbwg.R"))
#   export_asma_pbwg()                      # every year present
#   export_asma_pbwg(years = 2026)          # just one
#
# Output: outputs/asma/BRA-airport-xsma-<year>.csv, named the way PBWG names the
# other members' files (SIN-airport-xsma-2026.csv).
#
#   ICAO, DATE, PHASE, RWY, RANGE, N_VALID, TOTAL_TIME, TOTAL_REF_TIME,
#   TOTAL_ADD_TIME
#
# THE NUMBERS ARE OURS, NOT THE SUPPLIED ONES.
# The source carries Brazil's own `desimp` and `kpi08`, and they are kept in the
# analytic table as TOT_BRA_REF / TOT_BRA_KPI08 for validation. Nothing of theirs
# reaches this file: TOTAL_REF_TIME is the reference recomputed in this
# repository (GANP p20 of the reference year, per asma_ref_key) and
# TOTAL_ADD_TIME is TOTAL_TIME minus it. The two definitions differ -- ours keys
# the reference on CLASS, theirs does not -- so mixing them in one column would
# produce a file nobody could reconcile.
#
# ONE FILE PER YEAR, WITH RANGE AS A COLUMN.
# C40 and C100 measure different distances -- about ten minutes apart in this
# source -- so a row summing both would describe the ring mix rather than the
# operation, and a year whose traffic shifted between rings would look like a
# year whose performance changed. RANGE therefore stays in the grouping: the two
# rings share a file but never a row, and any reader aggregating this file must
# keep them apart.
#
# WHAT IS AGGREGATED AWAY
# The analytic table is one row per ICAO/PHASE/DATE/RANGE/CLASS/RWY/SECTOR_GROUP.
# CLASS and SECTOR_GROUP are summed out here, because PBWG asks for the day and
# the runway. The reference was BUILT on the finer key, so summing after the fact
# is right: each movement kept the reference of its own class and sector.
# =============================================================================

suppressPackageStartupMessages({library(dplyr); library(readr)})

export_asma_pbwg <- function(years    = NULL,
                             data_dir = here::here("data"),
                             out_dir  = here::here("outputs", "asma"),
                             ref_year = 2024,
                             variant  = "icao_ganp_p20",
                             quiet    = FALSE) {

  pat <- sprintf("^PBWG-BRA-asma-analytic-[0-9]{4}-[0-9]{4}-ref%d-%s\\.csv$",
                 ref_year, variant)
  files <- list.files(data_dir, pattern = pat, full.names = TRUE)
  if (length(files) == 0)
    stop("No ASMA analytic CSV in ", data_dir, " matching ", pat,
         "\n  -> run the `prepare-bra-asma-data` chunk first.")
  if (length(files) > 1)
    files <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]

  raw <- readr::read_csv(files, show_col_types = FALSE,
                         col_types = readr::cols(DATE = readr::col_character(),
                                                 .default = readr::col_guess()))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  d <- raw |>
    mutate(YEAR = substr(DATE, 1, 4)) |>
    # A row with no valid movement contributes nothing but would arrive as a line
    # of zeros; the deliverable carries measured days, not placeholders.
    filter(MVTS_VALID > 0)

  if (!is.null(years)) d <- filter(d, YEAR %in% as.character(years))
  if (nrow(d) == 0) { message("Nothing to export for ", paste(years, collapse = ", ")); return(invisible(character(0))) }

  written <- character(0)
  for (yr in sort(unique(d$YEAR))) {
    out <- d |>
      filter(YEAR == yr) |>
      group_by(ICAO, DATE, PHASE, RWY, RANGE) |>
      summarise(N_VALID        = sum(MVTS_VALID),
                TOTAL_TIME     = round(sum(TOT_ASMA), 4),
                TOTAL_REF_TIME = round(sum(TOT_REF), 4),
                TOTAL_ADD_TIME = round(sum(TOT_ADD_TIME), 4),
                .groups = "drop") |>
      arrange(ICAO, DATE, RWY, RANGE)

    f <- file.path(out_dir, sprintf("BRA-airport-xsma-%s.csv", yr))
    readr::write_csv(out, f, na = "")
    written <- c(written, f)
    if (!quiet) {
      # per ring, because a single figure over both would be the very average
      # this file is shaped to prevent
      per <- out |> group_by(RANGE) |>
        summarise(mv = sum(N_VALID),
                  add = sum(TOTAL_ADD_TIME) / sum(N_VALID), .groups = "drop")
      message(sprintf("%s: %s row(s)", basename(f), format(nrow(out), big.mark = ",")))
      for (i in seq_len(nrow(per)))
        message(sprintf("    %-5s %s movement(s), %.3f min/movement",
                        per$RANGE[i], format(per$mv[i], big.mark = ","), per$add[i]))
    }
  }
  invisible(written)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  export_asma_pbwg(if (length(args) == 0) NULL else as.integer(args))
}
