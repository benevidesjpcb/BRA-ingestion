#!/usr/bin/env Rscript
# =============================================================================
# classify_totalbr_daio.R
#
# TOTALBR reduced to the six fields a traffic profile needs, with each flight
# classified by how it touches Brazil:
#
#   I  internal    both ends in Brazil
#   D  departing   from Brazil to abroad
#   A  arriving    from abroad into Brazil
#   O  overflight  neither end in Brazil -- it crossed Brazilian airspace, which
#                  is why it is in the national table at all
#
#   source(here::here("TOTALBR", "classify_totalbr_daio.R"))
#   d <- totalbr_daio_month(2026, 1)          # ONE MONTH -- start here
#   d <- totalbr_daio()                       # the whole parquet
#   d <- totalbr_daio(years = 2024:2026)      # a slice of it
#   totalbr_daio_summary(d)                   # counts by class and year
#   totalbr_daio_unresolved(d)                # the codes still unclassified
#   totalbr_lookup_coverage(d = d)            # which database covers YOUR data
#   totalbr_daio_write(d)                     # -> outputs/
#
# HOW A COUNTRY IS DECIDED, in order. Each step is separately visible in the
# result, because a classification nobody can audit is a number nobody should
# quote:
#
#   1. an aerodrome database -- data-raw/world-airports.csv, or an OurAirports
#      extract in data/. The schema is detected, not assumed; see
#      totalbr_country_lookup()
#   2. data/oa-patch-bra.csv, for what that database lacks or gets wrong
#   3. a PREFIX RULE for codes neither knows, kept deliberately narrow -- see
#      TOTALBR_BR_PREFIX below
#   4. anything still unknown stays NA, and DAIO stays NA with it
#
# Step 4 is the point of steps 1-3 being auditable: an unresolved code is a row
# that cannot be counted, so totalbr_daio_unresolved() lists them by how much
# traffic each one costs, worst first. That list is the to-do for the patch file.
# =============================================================================

suppressPackageStartupMessages({
  for (p in c("arrow", "dplyr", "readr", "tibble"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' is required. install.packages('", p, "')")
})

# ---------------------------------------------------------------------------
# THE PREFIX RULE, and why it is narrower than it looks like it should be.
#
# The draft this comes from used
#   grepl("^S[BDNSWISJ]|9|^Z|AFIL|NI", ADEP)
# whose alternation binds loosely: it reads as "starts with SB/SD/SN/..." OR
# "contains a 9 anywhere" OR "starts with Z" OR "contains AFIL" OR "contains NI".
# The last two are almost certainly not what was meant -- SANI, LFNI and CYNI
# would all be called Brazilian on the strength of two letters in the middle --
# and "^Z" is the ICAO prefix for CHINA, so it only fails to do damage because
# Chinese aerodromes are in the OurAirports extract and never reach this rule.
#
# So the patterns are anchored, and split by what they mean:
#
#   TOTALBR_BR_PREFIX     real Brazilian ICAO prefixes (SB, SD, SI, SJ, SN, SS,
#                         SW), plus SBxx-style four-character codes
#   TOTALBR_UNKNOWN_ADEP  NOT aerodromes: ZZZZ is "aerodrome unknown" and AFIL
#                         is "flight plan filed in the air". Treating them as
#                         Brazil is a MODELLING DECISION, not a lookup -- these
#                         are flights whose other end is known and whose missing
#                         end is, in a Brazilian feed, most often Brazilian.
#                         It is separate from the prefix rule so its cost can be
#                         measured: totalbr_daio_summary() counts it.
#
# Set totalbr_daio(assume_unknown_is_br = FALSE) to leave them NA instead, and
# compare the two runs before deciding which the study should use.
# ---------------------------------------------------------------------------
TOTALBR_BR_PREFIX    <- "^S[BDIJNSW]"
TOTALBR_UNKNOWN_ADEP <- "^(ZZZZ|AFIL|[0-9])"

# =============================================================================
# totalbr_country_lookup() -- ICAO -> ISO2 country, from whatever file you have
#
#   totalbr_country_lookup()                                   # auto-detect
#   totalbr_country_lookup("data-raw/world-airports.csv")
#
# WHERE THE FILES COME FROM -- written down because the next person to refresh
# one will not remember, and the two are easy to confuse by their filenames:
#
#   data-raw/airports.csv        https://ourairports.com/data/
#                                the full OurAirports dump. Columns ident, type,
#                                icao_code, iata_code, gps_code, iso_country,
#                                iso_region, ... Big (86k rows) because it counts
#                                every heliport and closed strip; the part that
#                                matters is the rows carrying an ICAO code.
#
#   data-raw/world-airports.csv  https://world-airport-database.com/download/
#                                ~9k rows. Columns icao, iata, country, ...
#                                Its iso_country column is EMPTY -- see below.
#
# THE SCHEMA IS DETECTED, NOT ASSUMED, because those two disagree on both the
# column names and what is in them:
#
#   OurAirports          icao_code, iso_country ("BR")
#   world-airport-db     icao, country ("Brazil"), iso_country ENTIRELY EMPTY
#
# That last one is the trap worth naming. readr types a column of nothing as
# `lgl`, so world-airports.csv reads with `iso_country` as logical NA -- and a
# lookup built on it joins successfully, returns NA for every aerodrome, and
# leaves every flight unclassified without one error. So a column is used only
# if it actually holds values, and what was chosen is printed.
#
# A country given as a NAME rather than a code is translated through
# data/country-icao-iso-etc.csv (country.name.en -> iso2c). Names that file does
# not know are reported rather than dropped: they are aerodromes that will go
# unclassified, and that is a number worth seeing before trusting the output.
# =============================================================================
TOTALBR_ICAO_COLS    <- c("icao", "icao_code", "ident", "gps_code")

# A SECOND KEY, WHERE THE FILE HAS ONE. In the OurAirports dump `icao_code` is
# filled for a subset, while `ident` is the primary key and IS the ICAO code
# wherever the aerodrome has one -- so keying on icao_code alone throws away
# aerodromes the file knows perfectly well. `ident` is only trusted when it looks
# like an ICAO code: four letters, nothing else. That excludes the local
# identifiers the same column carries for small fields ("00A", "3B7"), which are
# not ICAO codes and would collide with nothing but noise.
TOTALBR_ALT_ICAO_COLS <- c("ident", "gps_code")
TOTALBR_ICAO_RE       <- "^[A-Z]{4}$"
TOTALBR_ISO_COLS     <- c("iso_country", "iso2c", "country_iso", "cntry_iso")
TOTALBR_CNTRY_COLS   <- c("country", "country_name", "iso_country")

# a column that exists AND holds at least one value
.tb_usable <- function(df, candidates) {
  for (nm in candidates) {
    hit <- which(tolower(names(df)) == nm)
    if (length(hit) && any(!is.na(df[[hit[1]]])) &&
        any(nzchar(trimws(as.character(df[[hit[1]]]))), na.rm = TRUE))
      return(names(df)[hit[1]])
  }
  NULL
}

# ISO2 codes are two letters; a country name is not. Deciding by the CONTENT
# rather than the column name is what lets one function read both databases.
.tb_looks_iso2 <- function(x) {
  v <- toupper(trimws(as.character(x)))
  v <- v[!is.na(v) & nzchar(v)]
  length(v) > 0 && mean(grepl("^[A-Z]{2}$", v)) > 0.9
}

# country name -> ISO2, from the reference table the project already carries
totalbr_iso_from_name <- function(
    file = here::here("data", "country-icao-iso-etc.csv")) {
  if (!file.exists(file)) return(NULL)
  d <- readr::read_csv(file, show_col_types = FALSE, progress = FALSE)
  nm <- .tb_usable(d, c("country.name.en", "country_name_en", "cntry_name", "country"))
  is <- .tb_usable(d, c("iso2c", "cntry_iso", "iso_country"))
  if (is.null(nm) || is.null(is)) return(NULL)
  stats::setNames(toupper(trimws(d[[is]])), toupper(trimws(d[[nm]])))
}

totalbr_country_lookup <- function(
    file       = totalbr_lookup_file(),
    patch_file = here::here("data", "oa-patch-bra.csv"),
    quiet      = FALSE) {

  if (!file.exists(file))
    stop("Aerodrome database not found: ", file,
         "\nDownload one and put it in data-raw/:",
         "\n  https://ourairports.com/data/            -> airports.csv (preferred)",
         "\n  https://world-airport-database.com/download/ -> world-airports.csv",
         "\nor pass file =, or set BRA_AIRPORT_DB.")

  raw <- readr::read_csv(file, show_col_types = FALSE, progress = FALSE)

  icao_col <- .tb_usable(raw, TOTALBR_ICAO_COLS)
  if (is.null(icao_col))
    stop(basename(file), " has no usable ICAO column. Looked for: ",
         paste(TOTALBR_ICAO_COLS, collapse = ", "), ".\nIt has: ",
         paste(names(raw), collapse = ", "))

  # A code column first; a name column only if no code column holds values.
  iso_col  <- .tb_usable(raw, TOTALBR_ISO_COLS)
  if (!is.null(iso_col) && !.tb_looks_iso2(raw[[iso_col]])) iso_col <- NULL
  name_col <- if (is.null(iso_col)) .tb_usable(raw, TOTALBR_CNTRY_COLS) else NULL
  # A column called `country` may hold codes rather than names -- databases
  # differ, and the name of the column is not evidence. Decide by the content:
  # two letters is a code, whatever the header says.
  if (!is.null(name_col) && .tb_looks_iso2(raw[[name_col]])) {
    iso_col <- name_col; name_col <- NULL
  }
  if (is.null(iso_col) && is.null(name_col))
    stop(basename(file), " has no usable country column: the ones it has are ",
         "empty or unrecognised. It has: ", paste(names(raw), collapse = ", "))

  icao <- toupper(trimws(as.character(raw[[icao_col]])))

  if (!is.null(iso_col)) {
    iso <- toupper(trimws(as.character(raw[[iso_col]])))
    if (!quiet) message(sprintf("Lookup: %s -> %s (ISO2 codes) from %s",
                                icao_col, iso_col, basename(file)))
  } else {
    nm  <- toupper(trimws(as.character(raw[[name_col]])))
    map <- totalbr_iso_from_name()
    if (is.null(map))
      stop(basename(file), " gives the country as a NAME (", name_col, "), and ",
           "data/country-icao-iso-etc.csv is missing or unreadable, so it ",
           "cannot be turned into a code. Add that file, or use a database ",
           "that carries iso_country.")
    iso <- unname(map[nm])
    if (!quiet) {
      message(sprintf("Lookup: %s -> %s (country NAMES) from %s, via %s",
                      icao_col, name_col, basename(file),
                      "data/country-icao-iso-etc.csv"))
      lost <- sort(unique(nm[!is.na(nm) & nzchar(nm) & is.na(iso)]))
      if (length(lost) > 0)
        message(sprintf("  %d country name(s) not in the reference table: %s",
                        length(lost), paste(utils::head(lost, 12), collapse = ", ")))
    }
  }

  base <- tibble::tibble(ICAO = icao, CNTRY_ISO = iso, SOURCE = "db") |>
    dplyr::filter(!is.na(.data$ICAO), nzchar(.data$ICAO), !is.na(.data$CNTRY_ISO))

  # Rows the primary key missed, recovered from a secondary one. Bound AFTER the
  # primary, so the de-duplication below keeps the primary's answer wherever
  # both have the code and this can only ever add aerodromes, never change one.
  for (alt in setdiff(TOTALBR_ALT_ICAO_COLS, icao_col)) {
    hit <- which(tolower(names(raw)) == alt)
    if (length(hit) == 0) next
    v <- toupper(trimws(as.character(raw[[hit[1]]])))
    ok <- !is.na(v) & grepl(TOTALBR_ICAO_RE, v) & !is.na(iso) & !(v %in% base$ICAO)
    if (!any(ok)) next
    if (!quiet)
      message(sprintf("  +%d aerodrome(s) keyed on %s where %s was empty",
                      sum(ok), alt, icao_col))
    base <- dplyr::bind_rows(
      base, tibble::tibble(ICAO = v[ok], CNTRY_ISO = iso[ok], SOURCE = "db"))
  }

  patch <- if (file.exists(patch_file)) {
    readr::read_csv(patch_file, comment = "#", show_col_types = FALSE,
                    progress = FALSE) |>
      dplyr::filter(!is.na(.data$ICAO), !is.na(.data$CNTRY_ISO)) |>
      dplyr::transmute(ICAO = toupper(trimws(.data$ICAO)),
                       CNTRY_ISO = toupper(trimws(.data$CNTRY_ISO)),
                       SOURCE = "patch")
  } else {
    if (!quiet) message("No patch file at ", patch_file, " -- using the database alone.")
    NULL
  }

  # THE JOIN MUST NOT MULTIPLY ROWS. A lookup holding an ICAO twice duplicates
  # every flight through that aerodrome, and the result still looks plausible --
  # the row count is simply wrong, in a direction nobody checks. The patch is
  # bound first, so it wins where both have the code: it exists to correct the
  # database, and letting the database win would make it a no-op on its main job.
  lk  <- dplyr::bind_rows(patch, base)
  dup <- lk$ICAO[duplicated(lk$ICAO)]
  lk  <- lk[!duplicated(lk$ICAO), ]

  if (!quiet) {
    message(sprintf("  %d aerodrome(s): %d from the patch, %d from the database",
                    nrow(lk), sum(lk$SOURCE == "patch"), sum(lk$SOURCE == "db")))
    inner <- unique(dup[!(dup %in% patch$ICAO)])
    if (length(inner) > 0)
      message(sprintf("  %d code(s) repeated WITHIN the database, first kept: %s",
                      length(inner), paste(utils::head(inner, 10), collapse = ", ")))
  }
  lk
}

# Kept under the old name: it is what the earlier draft called, and renaming a
# function is not a reason to break a script someone already has open.
totalbr_oa_lookup <- totalbr_country_lookup

# The aerodrome database, wherever it is. Two of them have been used here and
# their coverage is not the same; how much each one actually resolves is printed
# by totalbr_country_lookup() and measured by totalbr_daio_unresolved(). Neither
# is assumed to be better -- compare them with totalbr_lookup_coverage() rather
# than by the size of the file, which counts heliports and codeless fields that
# no flight will ever be matched against.
totalbr_lookup_file <- function() {
  env <- Sys.getenv("BRA_AIRPORT_DB", unset = "")
  if (nzchar(env)) return(env)
  # The OurAirports dump first: it carries both an ICAO column and a country
  # code, so it needs no name translation and resolves the most codes.
  cand <- c(here::here("data-raw", "airports.csv"),
            here::here("data", "airports.csv"),
            here::here("data-raw", "world-airports.csv"),
            here::here("data", "world-airports.csv"),
            sort(list.files(here::here("data"), pattern = "^oa-[0-9]{6}\\.csv$",
                            full.names = TRUE), decreasing = TRUE))
  hit <- cand[file.exists(cand)]
  if (length(hit) > 0) return(hit[1])
  here::here("data-raw", "airports.csv")   # named, so the error says what to add
}

# =============================================================================
# totalbr_lookup_coverage(files) -- which database resolves more of YOUR flights
#
#   totalbr_lookup_coverage()                       # every candidate on disk
#   totalbr_lookup_coverage(d = totalbr_daio_month(2026, 1))
#
# Two numbers per file, and only the second one matters:
#
#   AERODROMES  how many ICAO codes it resolves to a country. A file can be
#               enormous and score badly here: a dump full of heliports and
#               fields with no ICAO code at all is large, not useful.
#   RESOLVED    the share of the aerodrome codes IN YOUR DATA it covers, when a
#               classified month is passed as `d`. This is the question. A
#               database that knows 40,000 aerodromes none of which appear in
#               TOTALBR is worth less than one that knows 2,000 that do.
#
# Pass the month you are actually working on. Comparing files in the abstract is
# how a smaller database gets rejected for being smaller when it happens to
# cover Brazilian traffic better.
# =============================================================================
totalbr_lookup_coverage <- function(files = NULL, d = NULL) {
  if (is.null(files)) {
    cand <- c(here::here("data-raw", "airports.csv"),
              here::here("data", "airports.csv"),
              here::here("data-raw", "world-airports.csv"),
              here::here("data", "world-airports.csv"),
              list.files(here::here("data"), pattern = "^oa-[0-9]{6}\\.csv$",
                         full.names = TRUE))
    files <- cand[file.exists(cand)]
  }
  if (length(files) == 0) stop("No aerodrome database found to compare.")

  codes <- if (!is.null(d)) {
    v <- c(d$ADEP, d$ADES)
    unique(v[!is.na(v)])
  } else NULL

  out <- lapply(files, function(f) {
    lk <- tryCatch(totalbr_country_lookup(file = f, quiet = TRUE),
                   error = function(e) NULL)
    if (is.null(lk))
      return(tibble::tibble(FILE = basename(f), AERODROMES = NA_integer_,
                            IN_DATA = NA_integer_, RESOLVED_PCT = NA_real_))
    tibble::tibble(
      FILE       = basename(f),
      AERODROMES = nrow(lk),
      IN_DATA    = if (is.null(codes)) NA_integer_ else sum(codes %in% lk$ICAO),
      RESOLVED_PCT = if (is.null(codes)) NA_real_
                     else round(100 * mean(codes %in% lk$ICAO), 1))
  })
  out <- dplyr::bind_rows(out)
  if (is.null(codes))
    message("No data passed: RESOLVED_PCT needs `d`, e.g. ",
            "totalbr_lookup_coverage(d = totalbr_daio_month(2026, 1)).")
  out[order(-out$RESOLVED_PCT, -out$AERODROMES), ]
}

# =============================================================================
# totalbr_daio(...) -- the classified table
#
#   src      : the parquet, or a data frame already in memory
#   years    : optional filter on the year of dt_dia
#
# Only six columns are read from the parquet. That is not tidiness: the file is
# around a gigabyte and holds five list columns, and reading it whole to keep a
# quarter of it is the difference between a minute and a session that swaps.
# =============================================================================
totalbr_daio <- function(src   = totalbr_daio_source(),
                         years = NULL,
                         lookup = totalbr_country_lookup(),
                         assume_unknown_is_br = TRUE,
                         quiet = FALSE) {

  ndf <- if (is.data.frame(src)) {
    dplyr::transmute(src,
                     FLTID = .data$co_indicativo, ADEP = .data$co_addep,
                     ADES  = .data$co_addes,      TYPE = .data$co_modelo,
                     DATE  = .data$dt_dia,        SVC  = .data$li_tipovoo)
  } else {
    if (!file.exists(src))
      stop("TOTALBR source not found: ", src,
           "\nPass src = (a .csv month part or a .parquet), or set ",
           "BRA_TOTALBR_PARQUET.")
    if (!quiet) message("Reading ", src)
    want <- c("co_indicativo", "co_addep", "co_addes", "co_modelo",
              "dt_dia", "li_tipovoo")

    if (grepl("\\.csv$", src, ignore.case = TRUE)) {
      # A raw download, semicolon-separated and quoted, read as text. Only the
      # six wanted columns are selected, so a month part costs its own six
      # columns and not its forty.
      head1 <- data.table::fread(file = src, sep = ";", nrows = 0,
                                 showProgress = FALSE)
      missing <- setdiff(want, names(head1))
      if (length(missing) > 0)
        stop(basename(src), " lacks: ", paste(missing, collapse = ", "))
      d <- data.table::fread(file = src, sep = ";", select = want,
                             colClasses = "character", na.strings = "",
                             showProgress = FALSE, fill = Inf, header = TRUE)
      # fwrite wrote the stamps as text; they are UTC, whatever a parquet
      # column's label may claim elsewhere (see totalbr_sources.R on that trap).
      d[, dt_dia := as.POSIXct(dt_dia, tz = "UTC")]
      d <- as.data.frame(d)
    } else {
      ds <- arrow::open_dataset(src)
      missing <- setdiff(want, names(ds))
      if (length(missing) > 0)
        stop("The parquet lacks: ", paste(missing, collapse = ", "))
      d <- ds |> dplyr::select(dplyr::all_of(want)) |> dplyr::collect()
    }
    dplyr::rename(d, FLTID = "co_indicativo", ADEP = "co_addep",
                  ADES  = "co_addes",      TYPE = "co_modelo",
                  DATE  = "dt_dia",        SVC  = "li_tipovoo")
  }

  if (!is.null(years)) {
    # format() on the object's own clock, NOT as.Date(), which reads a POSIXct in
    # UTC and shifts a stamp whose column is labelled with another zone -- the
    # trap totalbr_sources.R documents, where a row written 00:59 landed in the
    # previous year.
    yr  <- as.integer(format(ndf$DATE, "%Y"))
    ndf <- ndf[!is.na(yr) & yr %in% as.integer(years), ]
  }
  if (!quiet) message(sprintf("Flights: %s", format(nrow(ndf), big.mark = ",")))

  # The aerodrome codes are matched as written: upper case, no blanks. A code
  # that differs only in case would otherwise be a miss, and a miss becomes an
  # NA country and a dropped flight.
  norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x[!nzchar(x)] <- NA_character_
    x
  }
  ndf$ADEP <- norm(ndf$ADEP)
  ndf$ADES <- norm(ndf$ADES)

  cn <- stats::setNames(lookup$CNTRY_ISO, lookup$ICAO)
  ndf$ADEP_CNTRY <- unname(cn[ndf$ADEP])
  ndf$ADES_CNTRY <- unname(cn[ndf$ADES])

  # How each end was decided, kept in the table. Without it, "why is this flight
  # internal" can only be answered by re-running the rules by hand.
  src_of <- function(code, cntry) {
    ifelse(!is.na(cntry), "lookup",
    ifelse(is.na(code), "no code",
    ifelse(grepl(TOTALBR_BR_PREFIX, code), "prefix",
    ifelse(grepl(TOTALBR_UNKNOWN_ADEP, code),
           if (assume_unknown_is_br) "assumed" else "unresolved",
           "unresolved"))))
  }
  ndf$ADEP_SRC <- src_of(ndf$ADEP, ndf$ADEP_CNTRY)
  ndf$ADES_SRC <- src_of(ndf$ADES, ndf$ADES_CNTRY)

  fill <- function(cntry, srcs) ifelse(is.na(cntry) & srcs %in% c("prefix", "assumed"),
                                       "BR", cntry)
  ndf$ADEP_CNTRY <- fill(ndf$ADEP_CNTRY, ndf$ADEP_SRC)
  ndf$ADES_CNTRY <- fill(ndf$ADES_CNTRY, ndf$ADES_SRC)

  a <- ndf$ADEP_CNTRY; b <- ndf$ADES_CNTRY
  ndf$DAIO <- ifelse(is.na(a) | is.na(b), NA_character_,
              ifelse(a == "BR" & b == "BR", "I",
              ifelse(a == "BR",             "D",
              ifelse(b == "BR",             "A", "O"))))

  if (!quiet) {
    n_na <- sum(is.na(ndf$DAIO))
    if (n_na > 0)
      message(sprintf("  %s flight(s) (%.2f%%) unclassified -- totalbr_daio_unresolved()",
                      format(n_na, big.mark = ","), 100 * n_na / nrow(ndf)))
  }
  tibble::as_tibble(ndf)
}

# where the parquet is: the env var, then the project's own raw folder
totalbr_daio_source <- function() {
  env <- Sys.getenv("BRA_TOTALBR_PARQUET", unset = "")
  if (nzchar(env)) return(env)
  for (d in c(here::here("data-src"), here::here("data-raw", "totalbr"))) {
    f <- list.files(d, pattern = "\\.parquet$", full.names = TRUE)
    if (length(f) > 0) return(sort(f, decreasing = TRUE)[1])
  }
  here::here("data-src", "totalbr.parquet")   # named, so the error says what to add
}

# =============================================================================
# totalbr_daio_month(year, month) -- ONE MONTH, from the raw download
#
#   totalbr_daio_month(2026, 1)
#
# The month parts under data-raw/totalbr/parts/ are the cheapest way to work:
# one month is a few hundred megabytes of CSV against a gigabyte of parquet, it
# is already on disk, and it is the same rows the parquet holds for that month.
# Start here, and only reach for the whole archive once the rules and the patch
# file are settled on a month you have actually looked at.
#
# The part is the RAW download, before the duplicate handling in
# prepare_totalbr.R. That is deliberate for a traffic profile -- every record
# the source served, classified as it stands -- but it means a flight reported
# twice is counted twice. Run totalbr_prepare(year, month) first and pass its
# result as `src` when the count itself has to be right.
# =============================================================================
totalbr_daio_month <- function(year, month, raw_dir = here::here("data-raw", "totalbr"),
                               ...) {
  mm   <- sprintf("%02d", as.integer(month))
  part <- file.path(raw_dir, "parts", sprintf("totalbr_%d-%s.csv", year, mm))
  if (!file.exists(part)) {
    have <- list.files(file.path(raw_dir, "parts"),
                       pattern = "^totalbr_[0-9]{4}-[0-9]{2}\\.csv$")
    stop("Not found: ", part,
         if (length(have)) paste0("\nMonths on disk: ",
                                  paste(sub("^totalbr_|\\.csv$", "", have),
                                        collapse = ", "))
         else "\nNo month parts in that folder at all.")
  }
  totalbr_daio(src = part, ...)
}

# =============================================================================
# totalbr_daio_summary(d) -- flights per class per year, and what decided them
# =============================================================================
totalbr_daio_summary <- function(d) {
  yr   <- format(d$DATE, "%Y")   # the written clock; see totalbr_daio()
  daio <- ifelse(is.na(d$DAIO), "unclassified", d$DAIO)
  # table() -> matrix -> data.frame, rather than reshape(): the column names are
  # then exactly the class letters, in a known order, whichever classes the data
  # happens to contain.
  m   <- table(YEAR = yr, DAIO = daio)
  out <- as.data.frame.matrix(m)
  cols <- intersect(c("I", "D", "A", "O", "unclassified"), names(out))
  out <- out[, cols, drop = FALSE]
  out$TOTAL <- rowSums(out)
  tibble::as_tibble(cbind(YEAR = rownames(m), out))
}

# how each end of each flight was decided -- the audit of the rules themselves
totalbr_daio_provenance <- function(d) {
  tibble::as_tibble(as.data.frame(
    table(ADEP = d$ADEP_SRC, ADES = d$ADES_SRC), stringsAsFactors = FALSE)) |>
    dplyr::filter(.data$Freq > 0) |>
    dplyr::arrange(dplyr::desc(.data$Freq))
}

# =============================================================================
# totalbr_daio_unresolved(d) -- the codes that cost the most flights
#
# The to-do list for data/oa-patch-bra.csv, worst first: every row here is
# flights that cannot be counted. A code appearing on thousands of flights is
# worth ten minutes with a chart; one appearing twice is not.
# =============================================================================
totalbr_daio_unresolved <- function(d, n = 40) {
  bad <- c(d$ADEP[d$ADEP_SRC == "unresolved"], d$ADES[d$ADES_SRC == "unresolved"])
  bad <- bad[!is.na(bad)]
  if (length(bad) == 0) {
    message("Every aerodrome code resolved to a country.")
    return(tibble::tibble(ICAO = character(0), FLIGHTS = integer(0)))
  }
  tb <- sort(table(bad), decreasing = TRUE)
  utils::head(tibble::tibble(ICAO = names(tb), FLIGHTS = as.integer(tb)), n)
}

# =============================================================================
# totalbr_daio_write(d) -- the product
#
# Parquet by default: 11.6 million rows is not a CSV anyone wants to re-read,
# and the timestamps survive as timestamps. Pass format = "csv" when something
# downstream needs text.
# =============================================================================
totalbr_daio_write <- function(d, out_dir = here::here("outputs"),
                               format = c("parquet", "csv"), file = NULL) {
  format <- match.arg(format)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  yrs <- range(format(d$DATE, "%Y"), na.rm = TRUE)
  tag <- if (yrs[1] == yrs[2]) yrs[1] else paste(yrs, collapse = "-")
  path <- if (!is.null(file)) file
          else file.path(out_dir, sprintf("totalbr-daio-%s.%s", tag, format))
  if (format == "parquet") arrow::write_parquet(d, path)
  else data.table::fwrite(d, path, sep = ";", na = "", quote = TRUE)
  message(sprintf("Wrote %s row(s) -> %s", format(nrow(d), big.mark = ","), path))
  invisible(path)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  d <- totalbr_daio(years = if (length(args)) as.integer(args) else NULL)
  print(totalbr_daio_summary(d))
  print(totalbr_daio_unresolved(d))
  totalbr_daio_write(d)
}
