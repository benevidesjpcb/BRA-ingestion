#!/usr/bin/env Rscript
# =============================================================================
# panel_totalbr.R -- the Brazilian side of the BRAZIL vs EUROPE panel
#
#   source(here::here("TOTALBR", "panel_totalbr.R"))
#   p <- totalbr_panel(2026, 1:6)      # every block, from the month parts
#   totalbr_panel_write(p)             # -> outputs/totalbr/panel-*.csv
#
# Five blocks, each answering one card of the panel:
#
#   $total      flights in the period, and per month
#   $daio       the traffic distribution -- Regional / Departures / Arrivals /
#               Overflights, which is exactly DAIO's I / D / A / O
#   $regions    share of external departures by world region
#   $countries  the country-level connections, ranked
#   $citypairs  the busiest aerodrome pairs against a chosen region
#
# WHAT "EXTERNAL" MEANS HERE, because the panel's own wording does not say and
# two readings give different numbers. A flight is external when it is NOT
# classified I: it has one end outside Brazil (A, D) or neither end in it (O).
# The region and country blocks describe THE OTHER END -- the foreign one -- so
# an A and a D to the same country count together as traffic with that country.
# Overflights are counted separately rather than folded in, because neither of
# their ends is Brazilian and calling one of them "the other end" would be
# arbitrary; $regions carries them in their own column.
#
# THE SOURCE IS THE CGNA FEED, and the result says so. The two feeds do not
# agree on the international traffic -- measured on 2026-01, the ODIN reports
# 8-11% more A, D and O while the internal traffic differs by 0.4% -- so a panel
# built from one and compared against a figure from the other is not comparing
# like with like. compare_totalbr_cgna.R is where that was measured.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})
source(here::here("TOTALBR", "classify_totalbr_daio.R"))

# ---- the world regions ------------------------------------------------------
# THE PANEL'S REGIONS ARE NOT CONTINENTS, so they cannot be taken from the
# aerodrome database's `continent` column. It splits the Americas three ways
# (South America / North America / Latin America & Caribbean) and lifts the
# Middle East out of Asia. Those are the panel's categories and they are
# political, so they are written out here as data rather than derived -- a
# derivation would have to encode the same choices anyway, less visibly.
#
# Central America and the Caribbean are ONE region ("Lat. Am. & Carib.") because
# the panel draws them that way; Mexico sits there rather than in North America
# for the same reason. Anything not listed falls to Africa or Asia/Pacific by
# the database's continent, and anything that reaches neither is reported as
# "unmapped" instead of being dropped -- see totalbr_panel_regions().
TOTALBR_REGIONS <- list(
  "South America" = c("AR", "BO", "CL", "CO", "EC", "FK", "GF", "GY", "PE",
                      "PY", "SR", "UY", "VE"),
  "North America" = c("US", "CA", "BM", "GL", "PM"),
  "Lat. Am. & Carib." = c(
    "MX", "GT", "BZ", "SV", "HN", "NI", "CR", "PA",              # Central Am.
    "CU", "DO", "HT", "JM", "PR", "BS", "TT", "BB", "AG", "AI",  # Caribbean
    "AW", "BQ", "CW", "SX", "GD", "KN", "KY", "LC", "MQ", "GP",
    "MS", "TC", "VC", "VG", "VI", "DM"),
  "Europe" = c("AT", "BE", "BY", "CH", "CY", "CZ", "DE", "DK", "ES", "FI",
               "FR", "GB", "GR", "HR", "HU", "IE", "IS", "IT", "LU", "LT",
               "LV", "MT", "NL", "NO", "PL", "PT", "RO", "RS", "RU", "SE",
               "SI", "SK", "UA", "AL", "BA", "BG", "EE", "MC", "MD", "ME",
               "MK", "XK"),
  "Middle East" = c("AE", "SA", "QA", "IL", "IR", "IQ", "JO", "KW", "LB",
                    "OM", "BH", "YE", "SY", "TR"),
  "Asia/Pacific" = c("AU", "NZ", "IN", "CN", "JP", "KR", "SG", "TH", "MY",
                     "ID", "PH", "VN", "HK", "TW", "PF", "NC", "FJ", "PG",
                     "PK", "BD", "LK", "NP", "KZ", "UZ", "MV")
)

# Africa is the remainder rather than a list: it is the region this feed touches
# most thinly and most variously (35 of the 106 countries in 2026-01/06), and a
# hand-kept list of African ISO codes would be the thing that silently drops a
# country the month after it is written. Everything not named above and known to
# the database as continent "AF" lands here; everything else is reported.
TOTALBR_REGION_FALLBACK <- c(AF = "Africa", AS = "Asia/Pacific",
                             OC = "Asia/Pacific", EU = "Europe",
                             SA = "South America", NA_ = "Lat. Am. & Carib.")

# ISO2 -> region, with the continent as the fallback and NA where neither knows
totalbr_region_of <- function(iso, cont_lookup = totalbr_continent_lookup()) {
  out <- rep(NA_character_, length(iso))
  for (rg in names(TOTALBR_REGIONS))
    out[iso %in% TOTALBR_REGIONS[[rg]]] <- rg
  miss <- is.na(out) & !is.na(iso)
  if (any(miss)) {
    cont <- cont_lookup[iso[miss]]
    cont[cont == "NA"] <- "NA_"            # North America, not a missing value
    out[miss] <- unname(TOTALBR_REGION_FALLBACK[cont])
  }
  out
}

# ISO2 -> continent, read off the same OurAirports file the classification uses.
# read.csv, not fread, and colClasses character throughout: this file's
# iso_country is where "NA" means NAMIBIA, and any reader that types the column
# turns Namibia into a missing value (the project has been here before -- see
# the totalbr_country_lookup() notes).
totalbr_continent_lookup <- function(file = NULL) {
  if (is.null(file)) file <- here::here("data-raw", "airports.csv")
  if (!file.exists(file))
    stop("Aerodrome database not found: ", file,
         "\nIt is what maps a country to a continent for the region fallback.")
  a <- data.table::fread(file = file, colClasses = "character",
                         select = c("iso_country", "continent"),
                         na.strings = NULL, showProgress = FALSE)
  a <- a[nzchar(iso_country) & nzchar(continent)]
  stats::setNames(a$continent[!duplicated(a$iso_country)],
                  a$iso_country[!duplicated(a$iso_country)])
}

# ---- loading the period -----------------------------------------------------
# The month parts, classified and stacked. A month missing from disk STOPS the
# run rather than narrowing it: a panel that says "Jan-Jun" while holding five
# months is a wrong number, not a smaller one.
totalbr_panel_load <- function(year, months = 1:6, feed = "cgna", quiet = TRUE) {
  parts <- lapply(months, function(m) {
    p <- totalbr_daio_part(year, m, feed = feed)
    if (!file.exists(p))
      stop(sprintf("The %s part for %d-%02d is not on disk: %s", toupper(feed),
                   year, m, p))
    if (!quiet) message("  ", basename(p))
    totalbr_daio_month(year, m, feed = feed, quiet = TRUE)
  })
  data.table::rbindlist(parts)
}

# The foreign end of a flight: the end that is not Brazil. NA for an overflight,
# which has two foreign ends and no basis for picking one, and NA for an
# internal flight, which has none.
.tb_panel_other_end <- function(d, what = c("cntry", "icao")) {
  what <- match.arg(what)
  cn_a <- if (what == "cntry") d$ADEP_CNTRY else d$ADEP
  cn_b <- if (what == "cntry") d$ADES_CNTRY else d$ADES
  data.table::fifelse(d$DAIO == "A", cn_a,
    data.table::fifelse(d$DAIO == "D", cn_b, NA_character_))
}

# =============================================================================
# totalbr_panel(year, months) -- every block at once
# =============================================================================
totalbr_panel <- function(year = 2026, months = 1:6, feed = "cgna",
                          d = NULL, quiet = FALSE) {
  if (is.null(d)) {
    if (!quiet) message("Reading ", toupper(feed), " parts ...")
    d <- totalbr_panel_load(year, months, feed, quiet)
  }
  d <- data.table::as.data.table(d)
  period <- sprintf("%d-%02d..%d-%02d", year, min(months), year, max(months))
  if (!quiet)
    message(sprintf("%s flight(s), %s, feed %s",
                    format(nrow(d), big.mark = ","), period, toupper(feed)))

  list(period    = period,
       feed      = feed,
       total     = totalbr_panel_total(d),
       daio      = totalbr_panel_daio(d),
       regions   = totalbr_panel_regions(d),
       countries = totalbr_panel_countries(d),
       citypairs = totalbr_panel_citypairs(d))
}

# ---- block 1: the totals ----------------------------------------------------
totalbr_panel_total <- function(d) {
  m <- format(d$DATE, "%Y-%m")
  out <- data.table::data.table(MONTH = sort(unique(m)))
  out[, FLIGHTS := as.integer(table(m)[MONTH])]
  rbind(out, data.table::data.table(MONTH = "TOTAL", FLIGHTS = nrow(d)))[]
}

# ---- block 2: the traffic distribution --------------------------------------
# The panel's four labels ARE the DAIO classes. Naming them here rather than
# leaving the letters is the whole translation: "Regional" is I.
TOTALBR_PANEL_DAIO_LABEL <- c(I = "Regional", D = "Departures",
                              A = "Arrivals", O = "Overflights")

totalbr_panel_daio <- function(d) {
  cls <- ifelse(is.na(d$DAIO), "unclassified", d$DAIO)
  n   <- table(factor(cls, levels = c(names(TOTALBR_PANEL_DAIO_LABEL),
                                      "unclassified")))
  out <- data.table::data.table(
    CLASS = c(unname(TOTALBR_PANEL_DAIO_LABEL), "unclassified"),
    DAIO  = c(names(TOTALBR_PANEL_DAIO_LABEL), NA_character_),
    FLIGHTS = as.integer(n))
  out[, PCT := round(100 * FLIGHTS / sum(FLIGHTS), 1)]
  out[]
}

# ---- block 3: external departures by region ---------------------------------
# "Share of mapped external departures": the foreign end of every flight that
# leaves or enters Brazil, grouped by world region and shown as a share of the
# flights that could be placed in one.
#
# UNMAPPED IS A ROW, NOT A SILENCE. A country the region table does not know
# and the database has no continent for would otherwise shrink every percentage
# below it while looking like nothing happened.
totalbr_panel_regions <- function(d) {
  iso <- .tb_panel_other_end(d, "cntry")
  ad  <- d$DAIO %in% c("A", "D") & !is.na(iso)
  rg  <- totalbr_region_of(iso[ad])
  rg[is.na(rg)] <- "unmapped"

  out <- data.table::data.table(REGION = names(sort(table(rg), decreasing = TRUE)),
                                FLIGHTS = as.integer(sort(table(rg), decreasing = TRUE)))
  out[, PCT := round(100 * FLIGHTS / sum(FLIGHTS), 1)]

  # overflights, kept apart: both of their ends are foreign
  ov <- d[DAIO == "O"]
  if (nrow(ov) > 0) {
    rg_o <- totalbr_region_of(c(ov$ADEP_CNTRY, ov$ADES_CNTRY))
    rg_o <- rg_o[!is.na(rg_o)]
    ot <- table(rg_o)
    out[, OVERFLIGHT_ENDS := as.integer(ot[REGION])]
    out[is.na(OVERFLIGHT_ENDS), OVERFLIGHT_ENDS := 0L]
  }
  out[]
}

# ---- block 4: the country-level connections ---------------------------------
# One row per foreign country, split into the direction it was flown, because
# "connections with Argentina" is a different number from "flights to
# Argentina" and the panel's bars do not say which they are.
totalbr_panel_countries <- function(d, region = NULL, n = NULL) {
  iso <- .tb_panel_other_end(d, "cntry")
  ok  <- d$DAIO %in% c("A", "D") & !is.na(iso)
  t   <- data.table::data.table(ISO = iso[ok], DAIO = d$DAIO[ok])
  out <- t[, .(FLIGHTS = .N,
               ARRIVALS   = sum(DAIO == "A"),
               DEPARTURES = sum(DAIO == "D")), by = ISO]
  out[, REGION := totalbr_region_of(ISO)]
  if (!is.null(region)) out <- out[REGION %in% region]
  out <- out[order(-FLIGHTS)]
  out[, PCT := round(100 * FLIGHTS / sum(FLIGHTS), 1)]
  if (!is.null(n)) out <- utils::head(out, n)
  out[]
}

# ---- block 5: the busiest aerodrome pairs -----------------------------------
# The panel's "city-pair" is an AERODROME pair: SBGR is Guarulhos, not Sao
# Paulo, and SBSP is the other Sao Paulo airport. Calling it a city pair would
# merge two aerodromes the data keeps apart, so the name says aerodrome and the
# reader can merge them if that is what they want.
totalbr_panel_citypairs <- function(d, region = "Europe", n = 15) {
  iso <- .tb_panel_other_end(d, "cntry")
  far <- .tb_panel_other_end(d, "icao")
  br  <- data.table::fifelse(d$DAIO == "A", d$ADES,
         data.table::fifelse(d$DAIO == "D", d$ADEP, NA_character_))
  ok  <- d$DAIO %in% c("A", "D") & !is.na(iso) & !is.na(far) & !is.na(br) &
         totalbr_region_of(iso) %in% region
  if (!any(ok)) return(data.table::data.table())
  t <- data.table::data.table(BR = br[ok], FAR = far[ok], ISO = iso[ok],
                              DAIO = d$DAIO[ok])
  out <- t[, .(FLIGHTS = .N,
               ARRIVALS   = sum(DAIO == "A"),
               DEPARTURES = sum(DAIO == "D")), by = .(BR, FAR, ISO)]
  utils::head(out[order(-FLIGHTS)], n)[]
}

# ---- writing ----------------------------------------------------------------
totalbr_panel_write <- function(p, out_dir = TOTALBR_OUT_DIR) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  tag <- sprintf("panel-%s-%s", p$feed, gsub("\\.\\.", "-", p$period))
  paths <- character(0)
  for (blk in c("total", "daio", "regions", "countries", "citypairs")) {
    if (is.null(p[[blk]]) || nrow(p[[blk]]) == 0) next
    f <- file.path(out_dir, sprintf("%s-%s.csv", tag, blk))
    data.table::fwrite(p[[blk]], f, sep = ";", na = "", quote = TRUE)
    paths <- c(paths, f)
  }
  message("Wrote ", length(paths), " file(s) -> ", out_dir)
  invisible(paths)
}
