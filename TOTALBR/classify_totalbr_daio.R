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
#   totalbr_daio_assumption_cost(d)           # what step 4 is worth, per class
#   totalbr_lookup_coverage(d = d)            # which database covers YOUR data
#   totalbr_daio_write(d)                     # -> outputs/totalbr/ (CSV)
#   totalbr_daio_write(d, format = "parquet") # ... parquet for archive-sized runs
#   totalbr_daio_read(path)                   # read one back, with its types
#
# HOW A COUNTRY IS DECIDED, in order. Each step is separately visible in the
# result, because a classification nobody can audit is a number nobody should
# quote:
#
#   1. an aerodrome database -- data-raw/airports.csv (OurAirports) or
#      the OurAirports dump. Its columns are found, not named; see
#      totalbr_country_lookup().                          _SRC = "lookup"
#   2. data/oa-patch-bra.csv, for what that database lacks or gets wrong. It is
#      as trusted as the database and separately reported, because it is 48
#      lines somebody maintains by hand.                  _SRC = "patch"
#   3. a PREFIX RULE, for a code NO database knows: the first two letters are a
#      Brazilian ICAO prefix (SB, SD, SI, SJ, SN, SS, SW), so the aerodrome is
#      in Brazil even though no file lists it. Deliberately narrow -- see
#      TOTALBR_BR_PREFIX below.                           _SRC = "prefix"
#   4. a Brazilian offshore platform (9P.., S9..): not an aerodrome, not in any
#      database, but its location is not in doubt.        _SRC = "platform"
#   5. codes that are not aerodromes at all (ZZZZ, XXXX, AFIL, AFIS, numeric),
#      ASSUMED Brazilian. The only step that invents an answer; measure it with
#      totalbr_daio_assumption_cost().                    _SRC = "assumed"
#   6. anything still unknown stays NA, and DAIO stays NA with it.
#                                                         _SRC = "unresolved"
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
# ZZZZ and XXXX are "aerodrome not stated"; AFIL is "flight plan filed in the
# air". None is an aerodrome and none can be looked up, so calling them
# Brazilian is an assumption. It is a defensible one, and the reason is worth
# writing down rather than leaving as a default nobody remembers choosing:
#
#   AN INTERNATIONAL FLIGHT PLAN REQUIRES A DEFINED AERODROME. ZZZZ is what gets
#   filed when the field is not in the ICAO list -- a private strip, a farm
#   runway, an unlisted field. Those exist at both ends of a domestic leg and
#   essentially never at the ends of an international one, so in a Brazilian
#   feed a ZZZZ is overwhelmingly a Brazilian airstrip. AFIL says the same thing
#   from another angle: a plan opened in the air is a flight that departed
#   outside controlled airspace, which is a domestic circumstance.
#
# AFIS joins them on the same reasoning: it is the Aerodrome Flight Information
# Service, a service and not a place, so what it marks is a field served by AFIS
# rather than a field with a code -- again a domestic circumstance in this feed.
# It is one flight in 2026-01, so nothing currently rests on it.
#
# The assumption is still measured rather than trusted -- see
# totalbr_daio_assumption_cost(), which prices it per class. Turn it off with
# totalbr_daio(assume_unknown_is_br = FALSE) to see the lookup-only floor.
TOTALBR_UNKNOWN_ADEP <- "^(ZZZZ|XXXX|AFIL|AFIS|[0-9])"

# 9P.. IS NOT AN ASSUMPTION. These are Brazilian OFFSHORE PLATFORMS -- the oil
# installations off Rio and Espírito Santo that helicopters shuttle to from
# Macaé, Vitória and Cabo Frio. They are not in any aerodrome database because
# they are not aerodromes, but their location is not in doubt: they sit on the
# Brazilian continental shelf, inside Brazilian airspace. So they get their own
# provenance rather than being lumped in with "unstated": in January 2026 they
# were a THIRD of everything the assumption was carrying, and counting known
# platforms as guesswork makes the guesswork look three times worse than it is.
#
# Two consequences worth knowing before quoting a figure built on this:
#   * they are internal traffic, correctly -- both ends in Brazil;
#   * they are HELICOPTER SHUTTLES, not airline movements. For anything that
#     compares airports or airline networks, filter them out:
#       d[d$ADEP_SRC != "platform" & d$ADES_SRC != "platform", ]
#
# S9.. is the same thing under another spelling and is counted with them. Its
# weight is nothing like 9P..'s: 112 distinct 9P codes carry 6,078 movements in
# 2026-01, while S9 is the single code S9FN on a single flight. Worth knowing
# before reading anything into it -- SN9F, a Brazilian-prefixed code one
# character away, carries 416 movements in the same month, so S9FN may well be
# that code mistyped rather than a platform of its own. It is classified
# Brazilian either way, which is why this is a note and not a blocker.
TOTALBR_BR_PLATFORM <- "^(9P|S9)"

# =============================================================================
# totalbr_country_lookup() -- ICAO -> ISO2 country, from whatever file you have
#
#   totalbr_country_lookup()                                   # auto-detect
#   totalbr_country_lookup("some/other/airports.csv")
#
# WHERE THE FILE COMES FROM -- written down because the next person to refresh it
# will not remember:
#
#   data-raw/airports.csv        https://ourairports.com/data/
#                                the full OurAirports dump. Columns ident, type,
#                                icao_code, iata_code, gps_code, iso_country,
#                                iso_region, ... Big (86k rows) because it counts
#                                every heliport and closed strip; the ~22k rows
#                                carrying a four-letter code are what a flight
#                                can be matched against, and they resolve 99.99%
#                                of a month's flying.
#
# A second database (world-airport-database.com) was tried and dropped: it holds
# a fraction of the aerodromes, gives the country as a NAME rather than a code,
# and ships an ISO country column that is entirely empty. It resolved nothing
# this one does not.
#
# THE COLUMNS ARE STILL FOUND RATHER THAN NAMED, for two reasons that outlived
# that file. A column is used only if it HOLDS VALUES -- readr types a column of
# nothing as `lgl`, and a lookup built on such a column joins successfully,
# returns NA for every aerodrome, and leaves every flight unclassified without
# raising one error. And the key is looked for under several names because this
# dump keeps the ICAO code in three of them; see TOTALBR_ALT_ICAO_COLS.
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

# =============================================================================
# READING A LOOKUP FILE: EVERYTHING AS TEXT, NOTHING AUTO-BLANKED
#
# Two defaults have to be turned off, and both cost aerodromes silently.
#
#   na = character(0). readr treats the string "NA" as missing by default, and
#   "NA" IS THE ISO2 CODE FOR NAMIBIA. Every Namibian aerodrome was therefore
#   read as having no country and dropped from the lookup -- which is how FYWH
#   (Windhoek) turned up in totalbr_daio_unresolved(). Nothing is auto-blanked;
#   emptiness is decided here, by nzchar.
#
#   col_character(). Type inference is what turned a dropped database's empty
#   iso_country column into a logical one -- a lookup built on it joined
#   cleanly and resolved nothing. Read as text, an empty column is a column of
#   "", rejected by .tb_usable() for the right reason (no values) rather than by
#   accident of type.
# =============================================================================
.tb_read <- function(path, ...) {
  readr::read_csv(path,
                  col_types = readr::cols(.default = readr::col_character()),
                  na = character(0), show_col_types = FALSE, progress = FALSE,
                  ...)
}

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

totalbr_country_lookup <- function(
    file       = totalbr_lookup_file(),
    patch_file = here::here("data", "oa-patch-bra.csv"),
    quiet      = FALSE) {

  if (!file.exists(file))
    stop("Aerodrome database not found: ", file,
         "\nDownload airports.csv from https://ourairports.com/data/ into",
         "\ndata-raw/, or pass file =, or set BRA_AIRPORT_DB.")

  raw <- .tb_read(file)

  icao_col <- .tb_usable(raw, TOTALBR_ICAO_COLS)
  if (is.null(icao_col))
    stop(basename(file), " has no usable ICAO column. Looked for: ",
         paste(TOTALBR_ICAO_COLS, collapse = ", "), ".\nIt has: ",
         paste(names(raw), collapse = ", "))

  # A code column first; a name column only if no code column holds values.
  iso_col  <- .tb_usable(raw, TOTALBR_ISO_COLS)
  if (!is.null(iso_col) && !.tb_looks_iso2(raw[[iso_col]])) iso_col <- NULL
  name_col <- if (is.null(iso_col)) .tb_usable(raw, TOTALBR_CNTRY_COLS) else NULL
  # A column called `country` may hold codes rather than names, and the header is
  # not evidence either way. Decide by the content: two letters is a code.
  if (!is.null(name_col) && .tb_looks_iso2(raw[[name_col]])) {
    iso_col <- name_col; name_col <- NULL
  }
  if (is.null(iso_col))
    stop(basename(file), " has no usable ISO2 country column",
         if (!is.null(name_col))
           paste0(" -- `", name_col, "` holds country NAMES, not codes, and ",
                  "translating them is no longer supported (the one database ",
                  "that needed it was dropped)")
         else ": the ones it has are empty or unrecognised",
         ".\nIt has: ", paste(names(raw), collapse = ", "))

  icao <- toupper(trimws(as.character(raw[[icao_col]])))
  iso  <- toupper(trimws(as.character(raw[[iso_col]])))
  if (!quiet) message(sprintf("Lookup: %s -> %s (ISO2 codes) from %s",
                              icao_col, iso_col, basename(file)))
  # "" is the empty value, not NA -- see .tb_read() on why Namibia made that
  # necessary. is.na() is still checked, because a code the file simply does not
  # have comes back from the match as a real NA.
  base <- tibble::tibble(ICAO = icao, CNTRY_ISO = iso, SOURCE = "db") |>
    dplyr::filter(!is.na(.data$ICAO), nzchar(.data$ICAO),
                  !is.na(.data$CNTRY_ISO), nzchar(.data$CNTRY_ISO))

  # Rows the primary key missed, recovered from a secondary one. Bound AFTER the
  # primary, so the de-duplication below keeps the primary's answer wherever
  # both have the code and this can only ever add aerodromes, never change one.
  for (alt in setdiff(TOTALBR_ALT_ICAO_COLS, icao_col)) {
    hit <- which(tolower(names(raw)) == alt)
    if (length(hit) == 0) next
    v <- toupper(trimws(as.character(raw[[hit[1]]])))
    ok <- !is.na(v) & grepl(TOTALBR_ICAO_RE, v) &
          !is.na(iso) & nzchar(iso) & !(v %in% base$ICAO)
    if (!any(ok)) next
    if (!quiet)
      message(sprintf("  +%d aerodrome(s) keyed on %s where %s was empty",
                      sum(ok), alt, icao_col))
    base <- dplyr::bind_rows(
      base, tibble::tibble(ICAO = v[ok], CNTRY_ISO = iso[ok], SOURCE = "db"))
  }

  patch <- if (file.exists(patch_file)) {
    .tb_read(patch_file, comment = "#") |>
      dplyr::filter(nzchar(trimws(.data$ICAO)), nzchar(trimws(.data$CNTRY_ISO))) |>
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
  # The OurAirports dump, under either the name it downloads as or the dated one
  # an earlier extract used.
  cand <- c(here::here("data-raw", "airports.csv"),
            here::here("data", "airports.csv"),
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
#   CODES_PCT   the share of the DISTINCT aerodrome codes in your data it knows.
#   ENDS_PCT    the share of FLIGHT ENDS it resolves -- the same thing weighted
#               by traffic. THIS is the one to read. The two diverge sharply:
#               the codes a database misses are overwhelmingly aerodromes seen
#               once or twice, so a file can know 70% of the codes and still
#               resolve 99% of the flying.
#   NOTE        why a file produced nothing, when it did.
#
# Pass the month you are actually working on. Comparing files in the abstract is
# how a smaller database gets rejected for being smaller when it happens to
# cover Brazilian traffic better.
# =============================================================================
totalbr_lookup_coverage <- function(files = NULL, d = NULL) {
  if (is.null(files)) {
    cand <- c(here::here("data-raw", "airports.csv"),
              here::here("data", "airports.csv"),
              list.files(here::here("data"), pattern = "^oa-[0-9]{6}\\.csv$",
                         full.names = TRUE))
    files <- cand[file.exists(cand)]
  }
  if (length(files) == 0) stop("No aerodrome database found to compare.")

  # Both ends of every flight, kept WITH their repetitions: the distinct codes
  # answer "how much of the world does this file know", and the ends answer
  # "how much of my traffic does it resolve". They differ enormously -- a
  # thousand codes seen once each weigh the same as one code seen a thousand
  # times in the first, and nothing like it in the second.
  ends   <- if (!is.null(d)) { v <- c(d$ADEP, d$ADES); v[!is.na(v)] } else NULL
  codes  <- if (!is.null(ends)) unique(ends) else NULL

  out <- lapply(files, function(f) {
    lk <- tryCatch(totalbr_country_lookup(file = f, quiet = TRUE),
                   error = function(e) e)
    if (inherits(lk, "condition"))
      # The reason, not just the absence of an answer. A file that cannot be
      # read and a file that resolves nothing are different problems, and NA
      # said neither.
      return(tibble::tibble(FILE = basename(f), AERODROMES = NA_integer_,
                            CODES_PCT = NA_real_, ENDS_PCT = NA_real_,
                            NOTE = substr(conditionMessage(lk), 1, 120)))
    tibble::tibble(
      FILE       = basename(f),
      AERODROMES = nrow(lk),
      CODES_PCT  = if (is.null(codes)) NA_real_
                   else round(100 * mean(codes %in% lk$ICAO), 1),
      ENDS_PCT   = if (is.null(ends)) NA_real_
                   else round(100 * mean(ends %in% lk$ICAO), 2),
      NOTE       = "")
  })
  out <- dplyr::bind_rows(out)
  if (is.null(codes))
    message("No data passed: the percentages need `d`, e.g. ",
            "totalbr_lookup_coverage(d = totalbr_daio_month(2026, 1)).")
  out[order(-out$ENDS_PCT, -out$AERODROMES), ]
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

# =============================================================================
# THE SIX FIELDS, AND WHAT EACH FEED CALLS THEM
#
# TOTALBR arrives from two APIs and they do not agree on every spelling: the
# ODIN download writes li_tipovoo, the CGNA writes co_tipo_voo for the same
# thing. So the canonical name is on the LEFT and every spelling seen in the
# wild is on the right, matched case- and punctuation-insensitively. Naming one
# feed's spellings in the reader is what made a column that is present look
# missing -- and, worse, what quietly made the ODIN part the only readable file.
# =============================================================================
TOTALBR_DAIO_COLS <- list(
  FLTID = c("co_indicativo"),
  ADEP  = c("co_addep"),
  ADES  = c("co_addes"),
  TYPE  = c("co_modelo"),
  DATE  = c("dt_dia"),
  SVC   = c("li_tipovoo", "co_tipo_voo")   # ODIN, then CGNA
)

# SVC is not required: a feed that omits it narrows the result instead of
# stopping the run. The other five decide the classification and are not
# optional.
TOTALBR_DAIO_REQUIRED <- c("FLTID", "ADEP", "ADES", "TYPE", "DATE")

# canonical name -> the column actually present, or NA
.tb_daio_match <- function(have, want = TOTALBR_DAIO_COLS) {
  flat <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  fh   <- flat(have)
  lapply(want, function(cands) {
    hit <- which(fh %in% flat(cands))
    if (length(hit) == 0) NA_character_ else have[hit[1]]
  })
}

totalbr_daio <- function(src   = totalbr_daio_source(),
                         years = NULL,
                         lookup = totalbr_country_lookup(),
                         assume_unknown_is_br = TRUE,
                         feed  = NULL,
                         quiet = FALSE) {

  # Which feed produced these rows. Named by the caller (totalbr_daio_month
  # knows), otherwise read off the file name, otherwise unknown -- never
  # guessed as the CGNA just because that is the default elsewhere.
  if (is.null(feed))
    feed <- if (is.character(src) && length(src) == 1 &&
                grepl("cgna", basename(src), ignore.case = TRUE)) "cgna"
            else if (is.character(src) && length(src) == 1 &&
                     grepl("\\.csv$", src, ignore.case = TRUE)) "odin"
            else NA_character_

  ndf <- if (is.data.frame(src)) {
    # Matched, not named: a frame handed in from the CGNA side spells the
    # service column co_tipo_voo, the ODIN side li_tipovoo.
    pick <- .tb_daio_match(names(src), TOTALBR_DAIO_COLS)
    missing <- names(TOTALBR_DAIO_COLS)[vapply(pick, is.na, logical(1)) &
                 names(TOTALBR_DAIO_COLS) %in% TOTALBR_DAIO_REQUIRED]
    if (length(missing) > 0)
      stop("The frame lacks: ", paste(missing, collapse = ", "))
    # NA of the FRAME'S length, not a scalar: as.data.frame() recycles nothing
    # and a length-1 column against n rows is an error, not a missing field.
    out <- lapply(pick, function(col)
      if (is.na(col)) rep(NA_character_, nrow(src)) else src[[col]])
    as.data.frame(out, stringsAsFactors = FALSE)
  } else {
    if (!file.exists(src))
      stop("TOTALBR source not found: ", src,
           "\nPass src = (a .csv month part or a .parquet), or set ",
           "BRA_TOTALBR_PARQUET.")
    if (!quiet) message("Reading ", src)
    want <- TOTALBR_DAIO_COLS

    if (grepl("\\.csv$", src, ignore.case = TRUE)) {
      # A raw download, semicolon-separated and quoted, read as text. Only the
      # six wanted columns are selected, so a month part costs its own six
      # columns and not its forty.
      head1 <- data.table::fread(file = src, sep = ";", nrows = 0,
                                 showProgress = FALSE)
      # The two feeds spell the same field differently -- the ODIN download
      # writes li_tipovoo where the CGNA writes co_tipo_voo -- so the columns
      # are MATCHED, not named. Reading the CGNA with the ODIN's spellings
      # hard-coded is what makes a present column look missing.
      pick <- .tb_daio_match(names(head1), want)
      missing <- names(want)[vapply(pick, is.na, logical(1)) &
                             names(want) %in% TOTALBR_DAIO_REQUIRED]
      if (length(missing) > 0)
        stop(basename(src), " lacks: ", paste(missing, collapse = ", "),
             "\nColumns found: ", paste(names(head1), collapse = ", "))
      got <- pick[!vapply(pick, is.na, logical(1))]
      d <- data.table::fread(file = src, sep = ";", select = unname(unlist(got)),
                             colClasses = "character", na.strings = "",
                             showProgress = FALSE, fill = Inf, header = TRUE)
      data.table::setnames(d, unname(unlist(got)), names(got))
      # An optional field the feed does not carry is still a column here, as
      # NA: the result keeps one shape whichever feed produced it.
      for (nm in setdiff(names(want), names(got))) d[, (nm) := NA_character_]
      # fwrite wrote the stamps as text; they are UTC, whatever a parquet
      # column's label may claim elsewhere (see totalbr_sources.R on that trap).
      d[, DATE := as.POSIXct(DATE, tz = "UTC")]
      d <- as.data.frame(d)
    } else {
      ds <- arrow::open_dataset(src)
      pick <- .tb_daio_match(names(ds), want)
      missing <- names(want)[vapply(pick, is.na, logical(1)) &
                             names(want) %in% TOTALBR_DAIO_REQUIRED]
      if (length(missing) > 0)
        stop("The parquet lacks: ", paste(missing, collapse = ", "))
      got <- pick[!vapply(pick, is.na, logical(1))]
      d <- ds |> dplyr::select(dplyr::all_of(unname(unlist(got)))) |>
        dplyr::collect()
      names(d) <- names(got)[match(names(d), unname(unlist(got)))]
      for (nm in setdiff(names(want), names(got))) d[[nm]] <- NA_character_
      d <- as.data.frame(d)
    }
    d[, names(want), drop = FALSE]
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

  # Which side of the lookup answered: the database, or the hand-maintained
  # patch file. Both are step 1-2 and equally trusted, but they are not equally
  # MAINTAINED -- the patch is 48 lines somebody wrote, and knowing how much
  # traffic leans on it is the difference between a list worth curating and one
  # that no longer matters.
  from_patch <- lookup$ICAO[lookup$SOURCE == "patch"]

  # How each end was decided, kept in the table. Without it, "why is this flight
  # internal" can only be answered by re-running the rules by hand.
  src_of <- function(code, cntry) {
    ifelse(!is.na(cntry), ifelse(code %in% from_patch, "patch", "lookup"),
    ifelse(is.na(code), "no code",
    ifelse(grepl(TOTALBR_BR_PLATFORM, code), "platform",
    ifelse(grepl(TOTALBR_BR_PREFIX, code), "prefix",
    ifelse(grepl(TOTALBR_UNKNOWN_ADEP, code),
           if (assume_unknown_is_br) "assumed" else "unresolved",
           "unresolved")))))
  }
  ndf$ADEP_SRC <- src_of(ndf$ADEP, ndf$ADEP_CNTRY)
  ndf$ADES_SRC <- src_of(ndf$ADES, ndf$ADES_CNTRY)

  fill <- function(cntry, srcs)
    ifelse(is.na(cntry) & srcs %in% c("platform", "prefix", "assumed"), "BR", cntry)
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
  out <- tibble::as_tibble(ndf)
  out$FEED <- feed
  if (!quiet)
    message("  feed: ", if (is.na(feed)) "unknown (archive)" else toupper(feed))
  out
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
# WHICH FEED A MONTH COMES FROM -- CGNA FIRST
#
# Both APIs write their month parts into data-raw/totalbr/parts/, under names
# that differ only by a tag:
#
#   CGNA   totalbr_<year>cgna_<YYYY-MM>.csv     <- THE PRIMARY SOURCE
#   ODIN   totalbr_<YYYY-MM>.csv
#
# The CGNA is the primary source for DAIO: it is the feed the national table is
# reconciled against (compare_totalbr_cgna.R), and for 2026 it is the one that
# was actually downloaded for this purpose. The ODIN part is a FALLBACK, and
# never a silent one -- reading it without saying so is how a classification
# ends up quoting a source nobody chose.
# =============================================================================
TOTALBR_DAIO_FEEDS <- c("cgna", "odin")

totalbr_daio_part <- function(year, month, raw_dir = here::here("data-raw", "totalbr"),
                              feed = TOTALBR_DAIO_FEEDS) {
  mm   <- sprintf("%02d", as.integer(month))
  year <- as.integer(year)
  file.path(raw_dir, "parts",
            switch(match.arg(feed, TOTALBR_DAIO_FEEDS),
                   cgna = sprintf("totalbr_%dcgna_%d-%s.csv", year, year, mm),
                   odin = sprintf("totalbr_%d-%s.csv", year, mm)))
}

# =============================================================================
# totalbr_daio_month(year, month) -- ONE MONTH, from the raw download
#
#   totalbr_daio_month(2026, 1)                 # the CGNA part -- the default
#   totalbr_daio_month(2026, 1, feed = "odin")  # the ODIN part, deliberately
#
# The month parts are the cheapest way to work: one month is a few hundred
# megabytes of CSV against a gigabyte of parquet, it is already on disk, and it
# is the same rows the year file holds for that month. Start here, and only
# reach for the whole archive once the rules and the patch file are settled on a
# month you have actually looked at.
#
# `feed` defaults to the CGNA. When that part is not on disk the ODIN part is
# NOT read in its place: the function stops and says which months each feed has,
# because the two do not carry the same rows (compare_totalbr_cgna.R exists
# precisely because they differ) and a DAIO table cannot be half of each.
# Passing feed = "odin" reads the ODIN part, and the result says so.
#
# The part is the RAW download, before the duplicate handling in
# prepare_totalbr.R. That is deliberate for a traffic profile -- every record
# the source served, classified as it stands -- but it means a flight reported
# twice is counted twice. Run totalbr_prepare(year, month) first and pass its
# result as `src` when the count itself has to be right.
# =============================================================================
totalbr_daio_month <- function(year, month, raw_dir = here::here("data-raw", "totalbr"),
                               feed = TOTALBR_DAIO_FEEDS, ...) {
  feed <- match.arg(feed, TOTALBR_DAIO_FEEDS)
  part <- totalbr_daio_part(year, month, raw_dir, feed)
  if (!file.exists(part)) {
    have <- function(f) {
      pat <- if (f == "cgna") "^totalbr_[0-9]{4}cgna_([0-9]{4}-[0-9]{2})\\.csv$"
             else             "^totalbr_([0-9]{4}-[0-9]{2})\\.csv$"
      m <- list.files(file.path(raw_dir, "parts"), pattern = pat)
      if (length(m) == 0) "none" else paste(sub(pat, "\\1", m), collapse = ", ")
    }
    stop("Not found: ", part,
         "\n  CGNA months on disk: ", have("cgna"),
         "\n  ODIN months on disk: ", have("odin"),
         "\nThe CGNA is the primary source; pass feed = \"odin\" to read the ",
         "ODIN part instead.")
  }
  totalbr_daio(src = part, feed = feed, ...)
}

# =============================================================================
# totalbr_daio_summary(d) -- flights per class per year, and what decided them
# =============================================================================
totalbr_daio_summary <- function(d) {
  # Say which feed the counts are of. The same month classified from the CGNA
  # and from the ODIN does not give the same totals, so a table of DAIO counts
  # without its source is not comparable to anything.
  f <- unique(d$FEED[!is.na(d$FEED)])
  if (length(f) > 0) message("Source: ", paste(toupper(f), collapse = " + "))
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
# totalbr_daio_assumed(d) -- WHICH codes were assumed Brazilian, and how often
#
# Step 4 of the classification is the only one that invents an answer, so it is
# the only one that can be wrong without anything looking wrong. This lists what
# it fired on. ZZZZ carrying most of it is a different situation from AFIL
# carrying most of it: the first is an unstated aerodrome, the second a plan
# filed in the air, and they are not equally likely to be Brazilian.
# =============================================================================
totalbr_daio_assumed <- function(d, n = 20) {
  v <- c(d$ADEP[d$ADEP_SRC == "assumed"], d$ADES[d$ADES_SRC == "assumed"])
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    message("Nothing was assumed: every code resolved or was left unresolved.")
    return(tibble::tibble(CODE = character(0), ENDS = integer(0)))
  }
  tb <- sort(table(v), decreasing = TRUE)
  utils::head(tibble::tibble(CODE = names(tb), ENDS = as.integer(tb)), n)
}

# =============================================================================
# totalbr_daio_assumption_cost(d) -- what the Brazil assumption is worth
#
# The same flights counted twice: as classified, and with step 4 withdrawn so
# that an unstated aerodrome leaves the flight unclassified instead of
# Brazilian. The difference is the part of every DAIO figure that rests on an
# assumption rather than on a lookup.
#
# Read the DELTA column. A month where D and A barely move is a month where the
# assumption is cheap; one where they move by a fifth is a month where "flights
# departing Brazil" cannot be quoted without saying what was assumed to get it.
# It matters most for D, A and O: a flight from ZZZZ to SBGR is called internal,
# and if that unstated aerodrome was in fact abroad, it was an arrival.
# =============================================================================
totalbr_daio_assumption_cost <- function(d) {
  strict <- ifelse(d$ADEP_SRC == "assumed" | d$ADES_SRC == "assumed",
                   NA_character_, d$DAIO)
  lv  <- c("I", "D", "A", "O")
  cnt <- function(x) vapply(lv, function(k) sum(!is.na(x) & x == k), integer(1))
  a <- cnt(d$DAIO); b <- cnt(strict)
  tibble::tibble(
    DAIO        = lv,
    AS_CLASSED  = as.integer(a),
    LOOKUP_ONLY = as.integer(b),
    DELTA       = as.integer(a - b),
    PCT_ASSUMED = ifelse(a > 0, round(100 * (a - b) / a, 1), NA_real_)
  )
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
# Written to outputs/totalbr/, one folder per dataset rather than one flat pile.
#
# CSV by default, because the file is meant to be opened and checked while this
# classification is still being worked on -- a month is ~180,000 rows, which any
# tool on the machine will read, and being able to look at it matters more right
# now than the size or the round-trip.
#
# Pass format = "parquet" for the archive-sized runs, where that trade flips:
# 11.6 million rows is not a CSV anyone wants to re-read, and parquet keeps the
# timestamps as timestamps instead of re-parsing text.
#
# WHAT THE CSV COSTS, said out loud rather than discovered later: DATE and the
# stamps go out as text. Reading the file back gives character columns unless
# the reader is told otherwise, so a DATE compared against a real date silently
# fails to match. totalbr_daio_read() below does that conversion; anything else
# reading these files has to do the same.
# =============================================================================
# Its own folder under outputs/, because DAIO is not the only thing this dataset
# will produce and a flat outputs/ stops being readable at about the fifth
# product. Created on first write.
TOTALBR_OUT_DIR <- here::here("outputs", "totalbr")

totalbr_daio_write <- function(d, out_dir = TOTALBR_OUT_DIR,
                               format = c("csv", "parquet"), file = NULL) {
  format <- match.arg(format)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # The feed is in the NAME, not only in the column: a CGNA classification and
  # an ODIN one for the same month are different numbers, and the one that is
  # written second must not overwrite the first.
  path <- if (!is.null(file)) file
          else file.path(out_dir, sprintf("totalbr-daio-%s%s.%s",
                                          totalbr_daio_feed_tag(d),
                                          totalbr_daio_span(d), format))
  if (format == "parquet") arrow::write_parquet(d, path)
  else data.table::fwrite(d, path, sep = ";", na = "", quote = TRUE)
  message(sprintf("Wrote %s row(s) -> %s", format(nrow(d), big.mark = ","), path))
  invisible(path)
}

# =============================================================================
# totalbr_daio_read(path) -- read a written result back with its types
#
# The counterpart to writing CSV by default. fwrite puts DATE and the stamps out
# as text, and a reader that does not convert them back gets character columns
# that compare unequal to any real date -- the failure is silent, which is why
# this exists instead of a note telling everyone to remember.
#
# A .parquet is handed to arrow, which carried its types all along.
# =============================================================================
totalbr_daio_read <- function(path) {
  if (!file.exists(path)) stop("Not found: ", path)
  if (grepl("\\.parquet$", path, ignore.case = TRUE))
    return(tibble::as_tibble(arrow::read_parquet(path)))
  d <- data.table::fread(file = path, sep = ";", na.strings = "",
                         showProgress = FALSE)
  # DATE IS A TIMESTAMP, NOT A DATE, AND THE ISO FORM MUST BE PARSED AS ONE.
  # The classification carries DATE as POSIXct and 92% of a month's rows hold a
  # real time of day, not midnight. Two ways to lose it, both silent, both hit
  # while writing this:
  #
  #   as.Date()   rounds the time away, and the column no longer compares equal
  #               to the one that was written.
  #   as.POSIXct() with no format does NOT understand fwrite's ISO output
  #               ("2026-01-15T13:45:00Z"). It parses the date, discards the
  #               rest, and returns midnight -- no warning, no NA, just 164,629
  #               timestamps quietly flattened.
  #
  # So the separator and the zone marker are handled explicitly, and anything
  # that still fails to parse becomes NA rather than a wrong time.
  #
  # AND THE COLUMN MAY ALREADY BE A TIMESTAMP. fread recognises the ISO form on
  # its own and hands back POSIXct, in which case there is nothing to parse --
  # only a zone to assert. Forcing it through as.character() first is a third
  # way to lose the time, and the nastiest: as.character() of a POSIXct at
  # midnight drops the "00:00:00" entirely, so a strict format then rejects
  # exactly the midnight rows and turns 13,652 of them into NA.
  iso <- function(x) {
    if (inherits(x, "POSIXct")) return(as.POSIXct(as.numeric(x),
                                                  origin = "1970-01-01", tz = "UTC"))
    if (inherits(x, c("Date", "IDate")))
      return(as.POSIXct(as.character(x), format = "%Y-%m-%d", tz = "UTC"))
    x <- sub("T", " ", as.character(x), fixed = TRUE)
    x <- sub("Z$", "", x)
    # midnight may be written with no time part at all
    x <- ifelse(!is.na(x) & nchar(x) == 10, paste(x, "00:00:00"), x)
    as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  for (cl in intersect(c("DATE", "dh_inicio", "dh_fim", "dh_eobt"), names(d)))
    data.table::set(d, j = cl, value = iso(d[[cl]]))
  tibble::as_tibble(d)
}

# The feed as a file-name prefix: "cgna-", "odin-", or nothing when the rows
# came from the archive and no feed was recorded.
totalbr_daio_feed_tag <- function(d) {
  f <- unique(d$FEED[!is.na(d$FEED)])
  if (length(f) == 0) "" else paste0(paste(sort(f), collapse = "-"), "-")
}

# =============================================================================
# totalbr_daio_span(d) -- the period a result covers, as a file-name tag
#
# THE TAG MUST NOT CLAIM MORE THAN THE DATA HOLDS. Naming a result by its YEARS
# writes January 2026 as "2026", which reads as the whole year and is then
# overwritten by, or confused with, a run that really is the whole year. The tag
# is built from the year-MONTHS present, and follows the naming the rest of the
# pipeline uses (outputs/totalbr-2026-01-flights.csv):
#
#   one month            2026-01
#   months of one year   2026-01-06
#   spanning years       2024-01-2026-03
# =============================================================================
totalbr_daio_span <- function(d) {
  ym <- format(d$DATE, "%Y-%m")
  ym <- ym[!is.na(ym)]
  if (length(ym) == 0) return("empty")
  lo <- min(ym); hi <- max(ym)
  if (lo == hi) return(lo)                                  # 2026-01
  if (substr(lo, 1, 4) == substr(hi, 1, 4))
    return(paste0(lo, "-", substr(hi, 6, 7)))               # 2026-01-06
  paste0(lo, "-", hi)                                       # 2024-01-2026-03
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  d <- totalbr_daio(years = if (length(args)) as.integer(args) else NULL)
  print(totalbr_daio_summary(d))
  print(totalbr_daio_unresolved(d))
  totalbr_daio_write(d)
}
