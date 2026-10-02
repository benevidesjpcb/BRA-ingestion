#!/usr/bin/env Rscript
# =============================================================================
# check_taxi_runways.R
#
# Is every runway the taxi table reports a runway the ICA registers for that
# aerodrome? `pista` in dstaxi against data-raw/aisweb/aisweb_runways.csv.
#
#   source(here::here("AISWEB", "check_taxi_runways.R"))
#   taxi_runway_check(2026)              # one row per aerodrome
#   taxi_runway_unknown(2026)            # the pista values the register lacks
#
# ONLY THE AERODROMES IN THE RUNWAY FILE ARE JUDGED. That file holds the
# aerodromes download_aisweb_aerodromes() was asked for -- twelve, by default --
# and a movement at any other aerodrome has nothing to be checked against. It is
# left out of the count rather than reported as unknown.
#
# THE AERODROME OF A MOVEMENT is where the runway is: the destination of an
# arrival, the origin of a departure.
#
# WHAT COUNTS AS THE SAME RUNWAY. The register writes a runway as its two
# thresholds, "17R/35L"; a movement uses one of them, "17R". Both sides are
# upper-cased and a single-digit number is padded ("9L" is "09L"), because that
# is formatting. Nothing else is folded: "33H" is not "33", and a value that
# differs by a letter is reported, not forgiven -- a helipad, a taxiway used as
# a runway and a typo all look like that, and telling them apart is the point.
# =============================================================================

source(here::here("TAXI", "compare_taxi_sources.R"))   # taxi_read(), taxi_norm()
source(here::here("CGNA", "cgna_common.R"))            # cgna_read_csv()

.rwy_norm <- function(x) {
  x <- taxi_norm(x)
  one <- !is.na(x) & grepl("^[0-9][A-Z]*$", x)
  x[one] <- paste0("0", x[one])
  x
}

# the register: one row per aerodrome x threshold designator
aisweb_thresholds <- function(path = here::here("data-raw", "aisweb", "aisweb_runways.csv")) {
  if (!file.exists(path))
    stop("Not found: ", path, "\nRun download_aisweb_aerodromes() first.", call. = FALSE)
  r <- cgna_read_csv(path)
  # the thresholds when the answer listed them, the two halves of the ident otherwise
  des <- ifelse(!is.na(r$thr) & nzchar(r$thr), gsub("|", "/", r$thr, fixed = TRUE), r$ident)
  out <- data.table::rbindlist(lapply(seq_len(nrow(r)), function(i)
    data.table::data.table(AIRPORT = taxi_norm(r$AeroCode[i]),
                           RWY = .rwy_norm(strsplit(des[i], "/", fixed = TRUE)[[1]]))))
  unique(out[!is.na(RWY)])
}

# movements per aerodrome x pista, for the aerodromes the register holds
.taxi_runway_counts <- function(year, taxi_path, runways_path) {
  thr <- aisweb_thresholds(runways_path)
  d <- taxi_read(taxi_path, cols = c("mov", "adpartida", "addestino", "pista"))
  d[, AIRPORT := taxi_norm(ifelse(taxi_norm(mov) == "ARR", addestino, adpartida))]
  d <- d[AIRPORT %in% thr$AIRPORT]
  d[, RWY := .rwy_norm(pista)]
  n <- d[, .(N = .N), by = .(AIRPORT, RWY)]
  n[, KNOWN := paste(AIRPORT, RWY) %in% paste(thr$AIRPORT, thr$RWY)]
  list(n = n, thr = thr)
}

# =============================================================================
# taxi_runway_check(year) -- one row per aerodrome
#
#   MOVEMENTS   movements at the aerodrome
#   NO_RWY      of those, with no pista at all
#   MATCHED     with a pista the register holds
#   UNKNOWN     with a pista the register does not hold
#   PCT_MATCHED MATCHED over the movements that HAVE a pista
#   REGISTER    the thresholds the ICA registers
#   NOT_IN_REGISTER   the pista values behind UNKNOWN
# =============================================================================
taxi_runway_check <- function(year = 2026,
    taxi_path = here::here("data-raw", "dstaxi", sprintf("dsTaxi%d.csv", year)),
    runways_path = here::here("data-raw", "aisweb", "aisweb_runways.csv")) {
  x <- .taxi_runway_counts(year, taxi_path, runways_path)
  n <- x$n
  out <- n[, .(MOVEMENTS = sum(N),
               NO_RWY    = sum(N[is.na(RWY)]),
               MATCHED   = sum(N[KNOWN]),
               UNKNOWN   = sum(N[!KNOWN & !is.na(RWY)]),
               NOT_IN_REGISTER = paste(sort(RWY[!KNOWN & !is.na(RWY)]), collapse = " ")),
           by = AIRPORT]
  out[, PCT_MATCHED := round(100 * MATCHED / pmax(MOVEMENTS - NO_RWY, 1), 2)]
  reg <- x$thr[, .(REGISTER = paste(sort(RWY), collapse = " ")), by = AIRPORT]
  out <- merge(reg, out, by = "AIRPORT", all.x = TRUE)
  data.table::setcolorder(out, c("AIRPORT", "MOVEMENTS", "NO_RWY", "MATCHED", "UNKNOWN",
                                 "PCT_MATCHED", "REGISTER", "NOT_IN_REGISTER"))
  out[order(AIRPORT)][]
}

# the pista values the register lacks, largest first
taxi_runway_unknown <- function(year = 2026,
    taxi_path = here::here("data-raw", "dstaxi", sprintf("dsTaxi%d.csv", year)),
    runways_path = here::here("data-raw", "aisweb", "aisweb_runways.csv")) {
  n <- .taxi_runway_counts(year, taxi_path, runways_path)$n
  n[!KNOWN & !is.na(RWY), .(AIRPORT, PISTA = RWY, MOVEMENTS = N)][order(-MOVEMENTS)][]
}
