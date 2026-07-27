#!/usr/bin/env Rscript
# =============================================================================
# validate_asma_golden.R
#
# Compares the ASMA (KPI08) analytic output produced by this repository against
# the reference file used in the official report, stored in golden/.
#
#   Rscript KPI08/validate_asma_golden.R
#
# or from R / a Quarto chunk:
#   source(here::here("KPI08", "validate_asma_golden.R"))
#   validate_asma_golden()
#
# Both files share the same schema:
#   ICAO, PHASE, DATE, RANGE, CLASS, RWY, SECTOR_GROUP,
#   MVTS_VALID, MVTS_NA, TOT_ASMA, TOT_REF, TOT_ADD_TIME, TOT_BRA_REF, TOT_BRA_KPI08
#
# The comparison is done on the shared reference key, restricted to the years the
# golden file covers, and reports per column: matching groups, differing groups,
# the largest single difference and the totals on each side.
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

# =============================================================================
# validate_asma_golden(ours, golden, tol)
#
#   ours   : our analytic CSV (default: newest ASMA analytic in data/)
#   golden : the reference CSV in golden/
#   tol    : absolute tolerance below which values count as equal (minutes)
#
# Returns, invisibly, a list with the per-column summary and the worst offenders.
# =============================================================================
validate_asma_golden <- function(
  ours   = NULL,
  golden = here::here("golden", "PBWG-BRA-asma-analytic-2023-2025-ref2024-icao_ganp_p20.csv"),
  tol    = 1e-6
) {
  if (is.null(ours)) {
    cand <- list.files(here::here("data"),
                       pattern = "^PBWG-BRA-asma-analytic-.*\\.csv$", full.names = TRUE)
    if (length(cand) == 0)
      stop("No ASMA analytic CSV in data/. Run the `prepare-bra-asma-data` chunk first.")
    ours <- cand[order(file.info(cand)$mtime, decreasing = TRUE)][1]
  }
  if (!file.exists(golden)) stop("Golden file not found: ", golden)

  message("ours   : ", basename(ours))
  message("golden : ", basename(golden))

  rd <- function(f) readr::read_csv(
    f, show_col_types = FALSE,
    col_types = readr::cols(DATE = readr::col_character(), .default = readr::col_guess())
  )
  g <- rd(golden)
  o <- rd(ours)

  key  <- c("ICAO", "PHASE", "DATE", "RANGE", "CLASS", "RWY", "SECTOR_GROUP")
  vals <- c("MVTS_VALID", "MVTS_NA", "TOT_ASMA", "TOT_REF", "TOT_ADD_TIME",
            "TOT_BRA_REF", "TOT_BRA_KPI08")

  missing_key <- setdiff(key, intersect(names(g), names(o)))
  if (length(missing_key) > 0)
    stop("The two files do not share the reference key; missing: ",
         paste(missing_key, collapse = ", "),
         "\n  -> re-run `prepare-bra-asma-data`; the analytic output must keep ",
         "CLASS, RWY and SECTOR_GROUP.")
  vals <- intersect(vals, intersect(names(g), names(o)))

  # Compare only the years present on BOTH sides. The study period and the
  # reference file need not cover the same span, and listing a year that only one
  # of them has would report every one of its groups as missing.
  years <- intersect(sort(unique(substr(g$DATE, 1, 4))),
                     sort(unique(substr(o$DATE, 1, 4))))
  if (length(years) == 0)
    stop("The two files share no year: golden has ",
         paste(sort(unique(substr(g$DATE, 1, 4))), collapse = ", "),
         "; ours has ", paste(sort(unique(substr(o$DATE, 1, 4))), collapse = ", "), ".")
  g <- dplyr::filter(g, substr(DATE, 1, 4) %in% years)
  o <- dplyr::filter(o, substr(DATE, 1, 4) %in% years)
  message("years  : ", paste(years, collapse = ", "), " (present in both)")

  # ---- key coverage ---------------------------------------------------------
  gk <- dplyr::select(g, all_of(key)) |> dplyr::distinct()
  ok <- dplyr::select(o, all_of(key)) |> dplyr::distinct()
  only_g <- dplyr::anti_join(gk, ok, by = key)
  only_o <- dplyr::anti_join(ok, gk, by = key)

  message(sprintf("\ngroups: golden %s | ours %s | only in golden %s | only in ours %s",
                  format(nrow(gk), big.mark = ","), format(nrow(ok), big.mark = ","),
                  format(nrow(only_g), big.mark = ","),
                  format(nrow(only_o), big.mark = ",")))

  # ---- value comparison on the shared groups -------------------------------
  joined <- dplyr::inner_join(
    dplyr::select(g, all_of(c(key, vals))),
    dplyr::select(o, all_of(c(key, vals))),
    by = key, suffix = c("_g", "_o")
  )

  # Per-year first: a single pooled table hides a year whose reference file was
  # produced differently from the others.
  by_year <- joined |>
    dplyr::mutate(YEAR = substr(DATE, 1, 4)) |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(
      GROUPS      = dplyr::n(),
      VALID_GOLD  = sum(MVTS_VALID_g), VALID_OURS = sum(MVTS_VALID_o),
      NA_GOLD     = sum(MVTS_NA_g),    NA_OURS    = sum(MVTS_NA_o),
      ADD_GOLD    = round(sum(TOT_ADD_TIME_g), 1),
      ADD_OURS    = round(sum(TOT_ADD_TIME_o), 1),
      .groups = "drop"
    ) |>
    dplyr::mutate(ADD_PCT = round(100 * (ADD_OURS - ADD_GOLD) / ADD_GOLD, 3))
  message("\nper year (shared groups):")
  print(as.data.frame(by_year), row.names = FALSE)

  # groups only on one side, per year — this is where a differently-built year shows
  oy <- only_o |> dplyr::mutate(YEAR = substr(DATE, 1, 4)) |> dplyr::count(YEAR, name = "ONLY_OURS")
  gy <- only_g   |> dplyr::mutate(YEAR = substr(DATE, 1, 4)) |> dplyr::count(YEAR, name = "ONLY_GOLDEN")
  if (nrow(oy) > 0 || nrow(gy) > 0) {
    message("\ngroups present on one side only:")
    print(as.data.frame(dplyr::full_join(gy, oy, by = "YEAR")), row.names = FALSE)
  }

  message("\nall years pooled:")
  summary <- purrr::map(vals, function(v) {
    a <- joined[[paste0(v, "_g")]]; b <- joined[[paste0(v, "_o")]]
    d <- abs(a - b)
    tibble::tibble(
      COLUMN     = v,
      EQUAL      = sum(d <= tol, na.rm = TRUE),
      DIFFERENT  = sum(d > tol, na.rm = TRUE),
      MAX_DIFF   = round(max(d, na.rm = TRUE), 4),
      SUM_GOLDEN = round(sum(a, na.rm = TRUE), 1),
      SUM_OURS   = round(sum(b, na.rm = TRUE), 1)
    ) |>
      dplyr::mutate(PCT_DIFF = round(100 * (SUM_OURS - SUM_GOLDEN) / SUM_GOLDEN, 3))
  }) |> purrr::list_rbind()

  print(as.data.frame(summary), row.names = FALSE)

  all_equal <- all(summary$DIFFERENT == 0) && nrow(only_g) == 0 && nrow(only_o) == 0
  if (all_equal) {
    message("\nPASS — the reproduction matches golden/ exactly.")
  } else {
    message("\nDIFFERENCES REMAIN — largest offender per column:")
    worst <- purrr::map(vals, function(v) {
      a <- joined[[paste0(v, "_g")]]; b <- joined[[paste0(v, "_o")]]
      i <- which.max(abs(a - b))
      if (length(i) == 0) return(NULL)
      dplyr::bind_cols(tibble::tibble(COLUMN = v),
                       joined[i, key],
                       tibble::tibble(GOLDEN = a[i], OURS = b[i]))
    }) |> purrr::list_rbind()
    print(as.data.frame(worst), row.names = FALSE)
  }

  invisible(list(summary = summary, only_golden = only_g, only_ours = only_o))
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) validate_asma_golden()
