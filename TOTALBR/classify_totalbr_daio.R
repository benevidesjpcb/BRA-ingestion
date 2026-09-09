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
#   d <- totalbr_daio()                       # the whole parquet
#   d <- totalbr_daio(years = 2024:2026)      # a slice
#   totalbr_daio_summary(d)                   # counts by class and year
#   totalbr_daio_unresolved(d)                # the codes still unclassified
#   totalbr_daio_write(d)                     # -> outputs/
#
# HOW A COUNTRY IS DECIDED, in order. Each step is separately visible in the
# result, because a classification nobody can audit is a number nobody should
# quote:
#
#   1. the OurAirports extract (data/oa-<yyyymm>.csv), ICAO -> iso_country
#   2. data/oa-patch-bra.csv, for what that extract lacks or gets wrong
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
# totalbr_oa_lookup() -- ICAO -> ISO2 country, from the extract plus the patch
#
# THE JOIN MUST NOT MULTIPLY ROWS. A left join on a lookup holding an ICAO twice
# duplicates every flight through that aerodrome, and the result still looks
# plausible -- the row count is simply wrong, in a direction nobody checks. So
# duplicates are collapsed here and reported, never carried into the join.
# =============================================================================
totalbr_oa_lookup <- function(
    oa_file    = totalbr_oa_file(),
    patch_file = here::here("data", "oa-patch-bra.csv"),
    quiet      = FALSE) {

  if (!file.exists(oa_file))
    stop("OurAirports extract not found: ", oa_file,
         "\nPut it in data/ as oa-<yyyymm>.csv, or pass oa_file =.")

  oa <- readr::read_csv(oa_file, show_col_types = FALSE, progress = FALSE) |>
    dplyr::filter(!is.na(.data$icao_code), !is.na(.data$iso_country)) |>
    dplyr::transmute(ICAO = toupper(trimws(.data$icao_code)),
                     CNTRY_ISO = toupper(trimws(.data$iso_country)),
                     SOURCE = "oa")

  patch <- if (file.exists(patch_file)) {
    readr::read_csv(patch_file, comment = "#", show_col_types = FALSE,
                    progress = FALSE) |>
      dplyr::filter(!is.na(.data$ICAO), !is.na(.data$CNTRY_ISO)) |>
      dplyr::transmute(ICAO = toupper(trimws(.data$ICAO)),
                       CNTRY_ISO = toupper(trimws(.data$CNTRY_ISO)),
                       SOURCE = "patch")
  } else {
    if (!quiet) message("No patch file at ", patch_file, " -- using the extract alone.")
    NULL
  }

  # The patch wins where both have the code: it exists precisely to correct the
  # extract, so letting the extract win would make it a no-op on its main job.
  lk <- dplyr::bind_rows(patch, oa)
  dup <- lk$ICAO[duplicated(lk$ICAO)]
  lk  <- lk[!duplicated(lk$ICAO), ]

  if (!quiet) {
    message(sprintf("Lookup: %d aerodrome(s) (%d from the patch, %d from %s)",
                    nrow(lk), sum(lk$SOURCE == "patch"), sum(lk$SOURCE == "oa"),
                    basename(oa_file)))
    if (length(dup) > 0) {
      # A code the patch also holds is an intended override, not a conflict; a
      # code the extract holds twice is a duplicate to know about.
      inner <- unique(dup[dup %in% oa$ICAO & !(dup %in% patch$ICAO)])
      if (length(inner) > 0)
        message(sprintf("  %d code(s) repeated WITHIN the extract, first kept: %s",
                        length(inner), paste(utils::head(inner, 10), collapse = ", ")))
    }
  }
  lk
}

# the newest data/oa-<yyyymm>.csv on disk
totalbr_oa_file <- function(dir = here::here("data")) {
  f <- list.files(dir, pattern = "^oa-[0-9]{6}\\.csv$", full.names = TRUE)
  if (length(f) == 0) return(file.path(dir, "oa-202603.csv"))  # named, so the error says what to add
  sort(f, decreasing = TRUE)[1]
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
                         lookup = totalbr_oa_lookup(),
                         assume_unknown_is_br = TRUE,
                         quiet = FALSE) {

  ndf <- if (is.data.frame(src)) {
    dplyr::transmute(src,
                     FLTID = .data$co_indicativo, ADEP = .data$co_addep,
                     ADES  = .data$co_addes,      TYPE = .data$co_modelo,
                     DATE  = .data$dt_dia,        SVC  = .data$li_tipovoo)
  } else {
    if (!file.exists(src))
      stop("TOTALBR parquet not found: ", src,
           "\nPass src =, or set BRA_TOTALBR_PARQUET.")
    if (!quiet) message("Reading ", src)
    ds <- arrow::open_dataset(src)
    want <- c("co_indicativo", "co_addep", "co_addes", "co_modelo",
              "dt_dia", "li_tipovoo")
    missing <- setdiff(want, names(ds))
    if (length(missing) > 0)
      stop("The parquet lacks: ", paste(missing, collapse = ", "))
    ds |>
      dplyr::select(dplyr::all_of(want)) |>
      dplyr::collect() |>
      dplyr::rename(FLTID = "co_indicativo", ADEP = "co_addep",
                    ADES  = "co_addes",      TYPE = "co_modelo",
                    DATE  = "dt_dia",        SVC  = "li_tipovoo")
  }

  if (!is.null(years)) {
    yr  <- as.integer(format(as.Date(ndf$DATE), "%Y"))
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
# totalbr_daio_summary(d) -- flights per class per year, and what decided them
# =============================================================================
totalbr_daio_summary <- function(d) {
  yr   <- format(as.Date(d$DATE), "%Y")
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
  yrs <- range(format(as.Date(d$DATE), "%Y"), na.rm = TRUE)
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
