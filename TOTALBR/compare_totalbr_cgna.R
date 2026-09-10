#!/usr/bin/env Rscript
# =============================================================================
# compare_totalbr_cgna.R
#
# The same national table from the TWO APIs that serve it: ODIN
# (data-raw/totalbr/totalbr_YYYY.csv) and the CGNA
# (data-raw/totalbr/totalbr_YYYYcgna.csv).
#
#   source(here::here("TOTALBR", "compare_totalbr_cgna.R"))
#   totalbr_cgna_daily(2026)                  # START HERE -- rows per day, both sides
#   totalbr_cgna_fill(2026, month = 1)        # THEN THIS -- is the key column there?
#   totalbr_cgna_time_shift(2026, month = 1)  # how far apart the two stamp
#   totalbr_cgna_stamp_check(2026, month = 1) # ... and whether that is a clock
#   compare_totalbr_cgna(2026)                # the summary, over the common window
#   compare_totalbr_cgna(2026, month = 1)     # the month PARTS, not the merged years
#   totalbr_cgna_field_diffs(2026, month = 1) # same flight, different values
#   totalbr_cgna_examples(2026, month = 1)    # rows only one side has
#   totalbr_cgna_scope(2026, "airport", month = 1)   # WHERE the extra rows are
#
# This is the taxi comparison (TAXI/compare_taxi_cgna.R) asked of the national
# table, and it is deliberately NOT compare_totalbr_sources.R. That one compares
# the parquet archive against ODIN and carries a measured time shift between
# them, because those two sources stamp dh_inicio ten minutes apart. Two APIs
# serving the same table are a different question and start from a clean slate:
# assume nothing about a shift, measure it, and only then decide what the key
# has to survive.
#
# THE WINDOW. Everything except totalbr_cgna_daily() compares only the days the
# CGNA file holds, because a half-downloaded year is the normal state and its
# missing months are not a difference between the sources. totalbr_cgna_daily()
# is deliberately NOT windowed: seeing which days exist on one side only is the
# whole point of it.
#
# THE COLUMN NAMES ARE NOT ASSUMED EQUAL. The CGNA taxi endpoint spells four
# columns without the underscores the files on disk use (dhbimtra for
# dh_bimtra, and so on), so every column here is looked up by its name with the
# underscores removed and the case folded. A rename that silently does not fire
# is how a comparison ends up reporting that two identical files share nothing.
#
# WHAT COUNTS AS "THE SAME FLIGHT". Three keys, and the summary reports the
# match rate of each, because a low rate is a finding about the KEY before it is
# a finding about the data:
#
#   "pk"        TRY THIS FIRST. Measured at 100% populated on both sides for
#               2026-01, so if the two APIs compute the hash the same way it
#               settles the comparison outright and nothing below matters.
#
#   "reg_seq"   Registration + aerodrome pair + calendar day, plus
#               the rotation number within that day. The registration is the
#               AIRFRAME, which is what physically flew; a callsign is an
#               operational label that gets reused and re-filed. No time of day
#               enters the key at all, so it cannot be broken by the two sources
#               stamping the same event differently -- which is the failure the
#               planned-time keys are exposed to.
#
#               Its own exposure is co_matricula, measured at 61.3% (ODIN) and
#               60.5% (CGNA) for 2026-01 -- not the "largely null" the qmd's open
#               point 4 recorded from an early sample, but 39% of flights that
#               this key cannot pair. They are reported as KEYLESS rather than
#               matched to each other. Run totalbr_cgna_fill() on any new period
#               before relying on it.
#
#   "flight_seq" The registration where it is known, the callsign where it is
#               not. Keeps the 39% at the cost of a weaker identifier on that
#               part. Read beside reg_seq, not instead of it.
#
#   "eobt_seq"  Callsign + aerodrome pair + the day of the filed off-block time,
#               plus the rotation number. dh_eobt is PLANNED, not observed --
#               both sources copy it from the same flight plan, so it survives a
#               shift in the observed stamps, but a re-filed EOBT changes it and
#               two sources snapshotting the plan at different moments disagree.
#               Kept as the counterweight: if it matches far better than
#               reg_seq, the registration is the problem, and vice versa.
#

# WHY THE ROTATION NUMBER. Without it, aerodrome pair plus day is ONE key for
# every rotation of a route in a day, so a side holding three legs of
# SBRJ-SBSP and a side holding two still "match" and the missing leg is never
# reported. Numbering them earliest-first pairs leg to leg and leaves the
# surplus one unmatched, which is the thing being looked for.
#
# AND WHY NO TIME WINDOW IN THE KEY. A tolerance cannot be chosen before the
# offset is measured -- picking "15 minutes" because it sounds reasonable is how
# a systematic difference gets absorbed and reported as agreement.
# totalbr_cgna_time_shift() measures it on pairs the airframe key found without
# using time at all; only then is a window worth building.
#
# Nothing is merged or corrected here. Which source wins is a decision about the
# study, not about the files.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# columns compared field by field once the flights are paired
TOTALBR_CGNA_CMP_COLS <- c("co_indicativo", "co_addep", "co_addes",
                           "co_matricula", "co_modelo", "co_tipo_voo",
                           "dh_inicio", "dh_fim", "dh_eobt", "dt_dia")

# ---- where the two files are -------------------------------------------------
# Left = ODIN, right = CGNA. `month` ("01", or 1) compares the MONTH PARTS
# instead of the merged years: cheaper when only one month is on the CGNA side,
# and it does not depend on the merge having been re-run. The two sides name
# their parts differently -- the ODIN engine writes totalbr_2026-01.csv, this
# project's CGNA download writes totalbr_2026cgna_2026-01.csv -- which is the
# only reason this needs a function rather than a path.
totalbr_cgna_paths <- function(year, month = NULL,
                               dir = here::here("data-raw", "totalbr")) {
  if (is.null(month))
    return(list(odin = file.path(dir, sprintf("totalbr_%d.csv", year)),
                cgna = file.path(dir, sprintf("totalbr_%dcgna.csv", year))))
  mm <- sprintf("%02d", as.integer(month))
  list(odin = file.path(dir, "parts", sprintf("totalbr_%d-%s.csv", year, mm)),
       cgna = file.path(dir, "parts", sprintf("totalbr_%dcgna_%d-%s.csv",
                                              year, year, mm)))
}

# ---- reading -----------------------------------------------------------------
totalbr_cgna_read <- function(path) {
  if (!file.exists(path)) stop("Not found: ", path)
  d <- data.table::fread(file = path, sep = ";", colClasses = "character",
                         na.strings = "", showProgress = FALSE,
                         fill = Inf, header = TRUE)
  data.table::setnames(d, names(d), tolower(names(d)))
  d[]
}

# One column, found under either spelling. Returns NA of the right length when
# the side does not carry it at all, so a missing column narrows the comparison
# instead of stopping it.
totalbr_cgna_col <- function(d, name) {
  flat <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  hit  <- which(flat(names(d)) == flat(name))
  if (length(hit) == 0) return(rep(NA_character_, nrow(d)))
  as.character(d[[hit[1]]])
}

# Text that means the same thing must compare equal: trailing blanks, case and
# the ISO "T" separator are formatting, not data.
totalbr_cgna_norm <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  toupper(x)
}
totalbr_cgna_norm_time <- function(x) {
  x <- totalbr_cgna_norm(x)
  x <- sub("T", " ", x, fixed = TRUE)
  substr(x, 1, 19)
}

# ---- the keys ----------------------------------------------------------------
# blank the key wherever the column it rests on is missing
.tb_na_if_missing <- function(k, on) ifelse(is.na(on) | !nzchar(on), NA_character_, k)

totalbr_cgna_key <- function(d, key = "reg_seq") {
  cs   <- totalbr_cgna_norm(totalbr_cgna_col(d, "co_indicativo"))
  dep  <- totalbr_cgna_norm(totalbr_cgna_col(d, "co_addep"))
  des  <- totalbr_cgna_norm(totalbr_cgna_col(d, "co_addes"))
  reg  <- totalbr_cgna_norm(totalbr_cgna_col(d, "co_matricula"))
  eobt <- totalbr_cgna_norm_time(totalbr_cgna_col(d, "dh_eobt"))
  day  <- totalbr_cgna_day(d)

  # Numbers the rotations of one route within one day, earliest first, so a side
  # holding three legs and a side holding two match leg-to-leg and leave the
  # surplus one unmatched -- which is the thing being looked for. Without it,
  # ADEP+ADES+day is ONE key for every rotation, and the shuttle routes
  # (SBRJ-SBSP and the like) silently "match" at the wrong multiplicity.
  seq_within <- function(base, ord_by) {
    ord    <- order(base, ord_by, na.last = TRUE)
    seq_no <- integer(length(base))
    seq_no[ord] <- stats::ave(seq_along(ord), base[ord], FUN = seq_along)
    paste(base, seq_no, sep = "|")
  }

  switch(key,
    # the row hash, case-folded -- it matches only if both sides compute it the
    # same way, which is itself worth knowing
    pk       = totalbr_cgna_norm(totalbr_cgna_col(d, "pk")),

    # ---- the airframe keys: what identifies a flight physically -------------
    # The REGISTRATION, not the callsign. A callsign is an operational label --
    # reused across the day, re-filed, and in general aviation often just the
    # registration anyway; the registration is the aircraft. Paired with the
    # aerodromes and the calendar day it says "this airframe flew this route
    # that day", which is a fact both sources should agree on however they stamp
    # their times.
    #
    # CHECK co_matricula IS POPULATED FIRST. This project has already recorded
    # it as "largely null" in an early sample (TOTALBR qmd, open point 4), and a
    # key built on a mostly-empty column reports two identical files as sharing
    # nothing. totalbr_cgna_fill() measures it, per side, before any of this is
    # believed.
    # NA where the registration is missing, NOT the string "NA". paste() turns a
    # missing value into the literal "NA", which would collapse every
    # registration-less flight of a route-day into ONE key and then match them
    # to each other by rotation order -- a false pairing, arrived at silently.
    # A flight with no registration simply cannot be matched on the airframe,
    # and is reported as unmatched, which is the truth.
    reg_day  = .tb_na_if_missing(paste(reg, dep, des, day, sep = "|"), reg),
    reg_seq  = .tb_na_if_missing(seq_within(paste(reg, dep, des, day, sep = "|"),
                          # ordered on the filed time where there is one, since
                          # it is the field least likely to differ between the
                          # sources; the observed start breaks the remaining ties
                          paste0(eobt, totalbr_cgna_norm_time(
                            totalbr_cgna_col(d, "dh_inicio")))), reg),

    # The practical key when the registration is only partly there, as it is:
    # the airframe where it is known, the callsign where it is not. The callsign
    # is a weaker identifier -- reused, re-filed -- but it is present on every
    # row, so this keeps the 39% of flights reg_seq has to drop. Which half a
    # match came from is not distinguished, so read it beside reg_seq rather
    # than instead of it.
    flight_seq = seq_within(
      paste(ifelse(is.na(reg), cs, reg), dep, des, day, sep = "|"),
      paste0(eobt, totalbr_cgna_norm_time(totalbr_cgna_col(d, "dh_inicio")))),

    # ---- the planned-time keys ---------------------------------------------
    # dh_eobt is a PLANNED value, not an observed one. Both sources copy it from
    # the same flight plan, which is why it survives a shift in the observed
    # stamps -- but a re-filed off-block time changes it, and two sources that
    # snapshot the plan at different moments will disagree. Kept for comparison
    # with the airframe keys rather than as the answer: if eobt matches far
    # better than reg, the registration is the problem, and the other way round.
    eobt     = paste(cs, dep, des, substr(eobt, 1, 16), sep = "|"),
    eobt_day = paste(cs, dep, des, substr(eobt, 1, 10), sep = "|"),
    eobt_seq = seq_within(paste(cs, dep, des, substr(eobt, 1, 10), sep = "|"), eobt),
    stop("Unknown key '", key,
         "'. Use pk, reg_day, reg_seq, flight_seq, eobt, eobt_day or eobt_seq.")
  )
}

# The day a row belongs to, on the table's own date column. dt_dia is what the
# ODIN download anchors its month windows on; dh_inicio is the fallback for a
# side that does not carry it.
totalbr_cgna_day <- function(d) {
  day <- substr(totalbr_cgna_norm_time(totalbr_cgna_col(d, "dt_dia")), 1, 10)
  if (all(is.na(day)))
    day <- substr(totalbr_cgna_norm_time(totalbr_cgna_col(d, "dh_inicio")), 1, 10)
  day
}

.totalbr_cgna_prep <- function(path, key = "reg_seq") {
  d <- totalbr_cgna_read(path)
  d[, KEY := totalbr_cgna_key(d, key)]
  d[, DAY := totalbr_cgna_day(d)]
  d[]
}

# ---- the period the two files have in common --------------------------------
# A partial download is the normal state while a year is being filled in, and
# comparing January of one source against a whole year of the other reports the
# months not downloaded yet as a difference between the sources. They are not.
# So the comparison is confined to a window, and the DEFAULT window is the span
# of days the CGNA file actually holds. Pass from/to ("YYYY-MM-DD") to narrow.
totalbr_cgna_window <- function(year, from = NULL, to = NULL, month = NULL,
                                key = "reg_seq",
                                paths = totalbr_cgna_paths(year, month),
                                quiet = FALSE) {
  load1 <- function(path, what) {
    if (!file.exists(path)) stop(what, " not found: ", path)
    if (!quiet) message("Reading ", what, ": ", path)
    .totalbr_cgna_prep(path, key)
  }
  a <- load1(paths$odin, "ODIN")
  b <- load1(paths$cgna, "CGNA")

  lo <- if (is.null(from)) min(b$DAY, na.rm = TRUE) else as.character(from)
  hi <- if (is.null(to))   max(b$DAY, na.rm = TRUE) else as.character(to)
  if (!quiet) message(sprintf("Window: %s -> %s (%s), key = %s", lo, hi,
                              if (is.null(from) && is.null(to))
                                "the days the CGNA file holds" else "given", key))
  list(a = a[!is.na(DAY) & DAY >= lo & DAY <= hi],
       b = b[!is.na(DAY) & DAY >= lo & DAY <= hi],
       from = lo, to = hi, key = key)
}

# =============================================================================
# totalbr_cgna_fill(year, month) -- RUN THIS BEFORE CHOOSING A KEY
#
# How populated each candidate key column is, on each side. A key built on a
# column one source leaves empty reports two identical files as sharing nothing,
# and that failure is indistinguishable from a real disagreement.
#
# co_matricula is the one to look at. It is what identifies the airframe, so it
# is the right thing to key on IF IT IS THERE -- and this project has already
# recorded it as "largely null" in an early sample (TOTALBR qmd, open point 4).
# Whether that still holds for 2026, and whether it holds equally on both sides,
# decides between reg_seq and eobt_seq. Measure, then choose.
# =============================================================================
totalbr_cgna_fill <- function(year, month = NULL,
                              cols = c("pk", "co_matricula", "co_indicativo",
                                       "co_addep", "co_addes", "co_modelo",
                                       "dh_eobt", "dh_inicio", "dh_fim", "dt_dia"),
                              paths = totalbr_cgna_paths(year, month)) {
  read1 <- function(path, what) {
    if (!file.exists(path)) { message("Not found: ", path); return(NULL) }
    message("Reading ", what, ": ", path)
    totalbr_cgna_read(path)
  }
  a <- read1(paths$odin, "ODIN"); b <- read1(paths$cgna, "CGNA")
  if (is.null(a) || is.null(b)) stop("Both sides are needed.")

  pct <- function(d, cl) {
    v <- totalbr_cgna_col(d, cl)
    if (all(is.na(v))) return(NA_real_)      # column absent entirely
    round(100 * mean(!is.na(v) & nzchar(trimws(v))), 1)
  }
  out <- data.table::data.table(
    COLUMN    = cols,
    ODIN_PCT  = vapply(cols, function(cl) pct(a, cl), numeric(1)),
    CGNA_PCT  = vapply(cols, function(cl) pct(b, cl), numeric(1)))
  out[, NOTE := data.table::fifelse(
    is.na(ODIN_PCT) & is.na(CGNA_PCT), "absent both sides",
    data.table::fifelse(is.na(ODIN_PCT), "absent from ODIN",
    data.table::fifelse(is.na(CGNA_PCT), "absent from CGNA",
    data.table::fifelse(pmin(ODIN_PCT, CGNA_PCT) < 50,
                        "TOO SPARSE TO KEY ON", ""))))]
  out[]
}

# =============================================================================
# totalbr_cgna_time_shift(year, month) -- how far apart the two sources stamp
#
# Pairs flights on the airframe key (which carries no time) and reports the
# distribution of the difference in dh_inicio.
#
# reg_seq, not reg_day: on a route-day with several legs, reg_day takes whichever
# leg happens to come first in each file, and the two files are not in the same
# order -- so it can difference one leg against another and produce outliers that
# look like data (a MIN of -364 and a MAX of 1430 minutes came from exactly
# that). The rotation number pairs leg to leg. A constant offset preserves the
# ordering it is built on, so this stays sound even while the offset is what is
# being measured. It is the measurement that must
# come before any tolerance is chosen: picking "15 minutes" because it sounds
# reasonable is how a systematic offset gets absorbed into a tolerance and
# reported as agreement.
#
# Read the quartiles, not the mean. A tight spread around zero means the sources
# stamp alike and an exact key would have worked; a tight spread around a
# non-zero value is a systematic offset, and THAT is the number a window has to
# straddle; a wide spread means they are measuring different events, and no
# window makes them the same.
# =============================================================================
totalbr_cgna_time_shift <- function(year, month = NULL, key = "reg_seq",
                                    from = NULL, to = NULL) {
  w <- totalbr_cgna_window(year, from, to, month, key = key, quiet = TRUE)
  a <- w$a[!duplicated(KEY) & !is.na(KEY)]
  b <- w$b[!duplicated(KEY) & !is.na(KEY)]
  common <- intersect(a$KEY, b$KEY)
  if (length(common) == 0)
    stop("No flight pairs under key '", key, "' -- run totalbr_cgna_fill() first.")
  a <- a[KEY %in% common][order(KEY)]
  b <- b[KEY %in% common][order(KEY)]

  ts <- function(d) as.POSIXct(totalbr_cgna_norm_time(
    totalbr_cgna_col(d, "dh_inicio")), tz = "UTC")
  diff_min <- as.numeric(difftime(ts(b), ts(a), units = "mins"))
  diff_min <- diff_min[is.finite(diff_min)]
  if (length(diff_min) == 0)
    stop("Neither side carries a usable dh_inicio for the paired flights.")

  q <- stats::quantile(diff_min, c(0, .05, .25, .5, .75, .95, 1), na.rm = TRUE)
  message(sprintf("%d pair(s) on key '%s'. CGNA minus ODIN, in minutes:",
                  length(diff_min), key))
  message(sprintf("  exactly equal: %.1f%%   within 1 min: %.1f%%   within 15: %.1f%%",
                  100 * mean(diff_min == 0), 100 * mean(abs(diff_min) <= 1),
                  100 * mean(abs(diff_min) <= 15)))
  tibble::tibble(PAIRS = length(diff_min),
                 MIN = q[[1]], P05 = q[[2]], P25 = q[[3]], MEDIAN = q[[4]],
                 P75 = q[[5]], P95 = q[[6]], MAX = q[[7]],
                 EQUAL_PCT   = round(100 * mean(diff_min == 0), 1),
                 WITHIN_1    = round(100 * mean(abs(diff_min) <= 1), 1),
                 WITHIN_15   = round(100 * mean(abs(diff_min) <= 15), 1))
}

# =============================================================================
# totalbr_cgna_stamp_check(year, month) -- IS THE SHIFT A CLOCK OR AN EVENT?
#
# The decisive test, and a cheap one. dh_eobt is a PLANNED value: both sources
# copy it from the same flight plan, so it is the same number on both sides
# unless something mechanical is moving every timestamp -- a timezone label
# taken at face value, a parse in the session's zone instead of UTC, a source
# writing local time.
#
#   dh_eobt shifts by the same amount as dh_inicio  -> A CLOCK. Every stamp in
#       one of the files is displaced. Fix the reading, not the data. This
#       project has been here before: the archive-vs-ODIN offset was measured at
#       +50 minutes while the parquet's Europe/Paris label was read as real, and
#       the true figure was 50 - 60 = -10 once the spurious hour came out
#       (TOTALBR_SHIFT_MIN in compare_totalbr_sources.R).
#
#   dh_eobt agrees and only dh_inicio moves      -> AN EVENT. The two sources
#       define the start of a flight differently, and no correction is
#       legitimate: it is a finding about the sources, to take to ICEA/CGNA.
#
# Also prints raw strings for a few paired flights, because a displaced clock is
# usually obvious the moment the two are seen side by side.
# =============================================================================
totalbr_cgna_stamp_check <- function(year, month = NULL, key = "reg_seq",
                                     n = 8, from = NULL, to = NULL) {
  w <- totalbr_cgna_window(year, from, to, month, key = key, quiet = TRUE)
  a <- w$a[!duplicated(KEY) & !is.na(KEY)]
  b <- w$b[!duplicated(KEY) & !is.na(KEY)]
  common <- intersect(a$KEY, b$KEY)
  if (length(common) == 0) stop("No pairs under key '", key, "'.")
  a <- a[KEY %in% common][order(KEY)]
  b <- b[KEY %in% common][order(KEY)]

  ts <- function(d, cl) as.POSIXct(
    totalbr_cgna_norm_time(totalbr_cgna_col(d, cl)), tz = "UTC")

  cols <- c("dh_eobt", "dh_inicio", "dh_fim", "dt_dia")
  out <- data.table::rbindlist(lapply(cols, function(cl) {
    dm <- as.numeric(difftime(ts(b, cl), ts(a, cl), units = "mins"))
    dm <- dm[is.finite(dm)]
    if (length(dm) == 0)
      return(data.table::data.table(COLUMN = cl, PAIRS = 0L, MEDIAN_MIN = NA_real_,
                                    EQUAL_PCT = NA_real_, SAME_AS_MEDIAN_PCT = NA_real_))
    md <- stats::median(dm)
    data.table::data.table(
      COLUMN = cl, PAIRS = length(dm), MEDIAN_MIN = md,
      EQUAL_PCT = round(100 * mean(dm == 0), 1),
      # how CONSTANT the shift is: a clock displaces everything by one number,
      # an event difference spreads
      SAME_AS_MEDIAN_PCT = round(100 * mean(dm == md), 1))
  }))

  message("Raw stamps for ", min(n, nrow(a)), " paired flight(s):")
  show <- utils::head(seq_len(nrow(a)), n)
  print(data.table::data.table(
    KEY        = a$KEY[show],
    EOBT_ODIN  = totalbr_cgna_col(a, "dh_eobt")[show],
    EOBT_CGNA  = totalbr_cgna_col(b, "dh_eobt")[show],
    START_ODIN = totalbr_cgna_col(a, "dh_inicio")[show],
    START_CGNA = totalbr_cgna_col(b, "dh_inicio")[show]))

  eobt <- out[COLUMN == "dh_eobt"]
  ini  <- out[COLUMN == "dh_inicio"]
  if (nrow(eobt) && nrow(ini) && !is.na(eobt$MEDIAN_MIN) && !is.na(ini$MEDIAN_MIN)) {
    if (abs(eobt$MEDIAN_MIN - ini$MEDIAN_MIN) < 1)
      message("\nVERDICT: dh_eobt moves with dh_inicio (both ~",
              round(ini$MEDIAN_MIN), " min). A PLANNED field cannot drift, so this\n",
              "  is a CLOCK: every stamp in one file is displaced. Fix the reading.")
    else if (abs(eobt$MEDIAN_MIN) < 1)
      message("\nVERDICT: dh_eobt agrees exactly and only dh_inicio moves (~",
              round(ini$MEDIAN_MIN), " min).\n",
              "  The clocks are fine; the two sources define the start of a flight\n",
              "  differently. That is a finding for ICEA/CGNA, not something to correct.")
    else
      message("\nVERDICT: dh_eobt moves by ", round(eobt$MEDIAN_MIN),
              " min and dh_inicio by ", round(ini$MEDIAN_MIN),
              " min -- neither\n  a clean clock nor a clean event difference. Read the raw stamps above.")
  }
  out[]
}

# =============================================================================
# compare_totalbr_cgna(year) -- the summary, one row per key
#
#   ROWS/KEYS/DUP per side, then the set arithmetic:
#     BOTH        flights both sources report
#     ONLY_ODIN   reported by ODIN, absent from the CGNA file
#     ONLY_CGNA   the reverse
#     MATCH_PCT   BOTH as a share of the smaller side -- read this FIRST. A low
#                 figure on pk with a high one on eobt_seq says the two sources
#                 hash their rows differently, not that they hold different
#                 flights.
# =============================================================================
compare_totalbr_cgna <- function(year, from = NULL, to = NULL, month = NULL,
                                 keys = c("pk", "reg_seq", "flight_seq", "eobt_seq"),
                                 quiet = FALSE) {
  out <- lapply(keys, function(k) {
    w  <- totalbr_cgna_window(year, from, to, month, key = k, quiet = quiet)
    # A key built on a column one side does not carry is all-NA there, and would
    # be reported as "nothing in common" -- a difference between the sources
    # where there is only a missing column. Say which it is and skip the row.
    if (all(is.na(w$a$KEY)) || all(is.na(w$b$KEY))) {
      side <- if (all(is.na(w$a$KEY))) "ODIN" else "CGNA"
      message("Key '", k, "' skipped: the ", side,
              " file does not carry the column it is built from.")
      return(NULL)
    }
    ka <- w$a$KEY; kb <- w$b$KEY
    ua <- unique(ka[!is.na(ka)]); ub <- unique(kb[!is.na(kb)])
    both <- length(intersect(ua, ub))
    data.table::data.table(
      YEAR      = year,
      KEY       = k,
      KEYLESS   = sum(is.na(ka)) + sum(is.na(kb)),
      FROM      = w$from,
      TO        = w$to,
      ROWS_ODIN = length(ka),
      ROWS_CGNA = length(kb),
      KEYS_ODIN = length(ua),
      KEYS_CGNA = length(ub),
      DUP_ODIN  = length(ka) - length(ua),
      DUP_CGNA  = length(kb) - length(ub),
      BOTH      = both,
      ONLY_ODIN = length(setdiff(ua, ub)),
      ONLY_CGNA = length(setdiff(ub, ua)),
      # over the keys that EXIST: a row the key cannot be built for is counted
      # in KEYLESS, not held against the sources
      MATCH_PCT = round(100 * both / max(1L, min(length(ua), length(ub))), 1)
    )
  })
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0)
    stop("None of the keys (", paste(keys, collapse = ", "),
         ") can be built from both files.")
  data.table::rbindlist(out)[]
}

# =============================================================================
# totalbr_cgna_field_diffs(year) -- same flight, different values
#
# Paired on the key, then compared column by column. summary_only = TRUE gives
# one row per column (how often it disagrees); FALSE gives the flights.
# =============================================================================
totalbr_cgna_field_diffs <- function(year, cols = TOTALBR_CGNA_CMP_COLS,
                                     summary_only = TRUE, key = "reg_seq",
                                     from = NULL, to = NULL, month = NULL) {
  w <- totalbr_cgna_window(year, from, to, month, key = key, quiet = TRUE)
  # one row per key on each side: a key that repeats within a file is a
  # duplicate, reported by the summary, and pairing on it here would multiply
  # the rows instead of comparing them
  a <- w$a[!duplicated(KEY) & !is.na(KEY)]
  b <- w$b[!duplicated(KEY) & !is.na(KEY)]
  common <- intersect(a$KEY, b$KEY)
  a <- a[KEY %in% common][order(KEY)]
  b <- b[KEY %in% common][order(KEY)]
  if (nrow(a) == 0) {
    message("No flight is present on both sides under key '", key, "'.")
    return(data.table::data.table())
  }

  time_like <- function(nm) grepl("^dh_|^dt_", nm)
  res <- lapply(cols, function(cl) {
    va <- totalbr_cgna_col(a, cl); vb <- totalbr_cgna_col(b, cl)
    if (all(is.na(va)) && all(is.na(vb)))
      return(data.table::data.table(COLUMN = cl, DIFF = NA_integer_,
                                    PCT = NA_real_, NOTE = "absent both sides"))
    na <- if (time_like(cl)) totalbr_cgna_norm_time(va) else totalbr_cgna_norm(va)
    nb <- if (time_like(cl)) totalbr_cgna_norm_time(vb) else totalbr_cgna_norm(vb)
    dif <- !(is.na(na) & is.na(nb)) & (is.na(na) | is.na(nb) | na != nb)
    data.table::data.table(
      COLUMN = cl, DIFF = sum(dif), PCT = round(100 * mean(dif), 2),
      NOTE = if (all(is.na(va))) "absent from ODIN"
             else if (all(is.na(vb))) "absent from CGNA" else "")
  })
  summ <- data.table::rbindlist(res)[order(-DIFF)]
  if (summary_only) return(summ[])

  keep <- summ[!is.na(DIFF) & DIFF > 0, COLUMN]
  if (length(keep) == 0) return(data.table::data.table())
  out <- data.table::data.table(KEY = a$KEY)
  for (cl in keep) {
    out[[paste0(cl, "_ODIN")]] <- totalbr_cgna_col(a, cl)
    out[[paste0(cl, "_CGNA")]] <- totalbr_cgna_col(b, cl)
  }
  differs <- Reduce(`|`, lapply(keep, function(cl) {
    va <- out[[paste0(cl, "_ODIN")]]; vb <- out[[paste0(cl, "_CGNA")]]
    na <- if (time_like(cl)) totalbr_cgna_norm_time(va) else totalbr_cgna_norm(va)
    nb <- if (time_like(cl)) totalbr_cgna_norm_time(vb) else totalbr_cgna_norm(vb)
    !(is.na(na) & is.na(nb)) & (is.na(na) | is.na(nb) | na != nb)
  }))
  out[differs][]
}

# ---- rows one side only, in time order --------------------------------------
totalbr_cgna_examples <- function(year, n = 10, key = "reg_seq",
                                  from = NULL, to = NULL, month = NULL) {
  w <- totalbr_cgna_window(year, from, to, month, key = key, quiet = TRUE)
  pick <- function(d, label) {
    cols <- intersect(c("co_indicativo", "co_addep", "co_addes", "co_matricula",
                        "dh_eobt", "dh_inicio", "dh_fim", "dt_dia", "pk"),
                      names(d))
    x <- utils::head(d[order(DAY)], n)[, c(cols, "DAY"), with = FALSE]
    x[, SOURCE := label][]
  }
  rbind(pick(w$a[!KEY %in% w$b$KEY], "ODIN"),
        pick(w$b[!KEY %in% w$a$KEY], "CGNA"), fill = TRUE)[]
}

# =============================================================================
# totalbr_cgna_daily(year) -- rows per day on each side
#
# One row per calendar day present in either file:
#   ODIN / CGNA   rows on each side that day
#   RATIO         ODIN / CGNA, so a constant factor is visible at a glance
#   FLAG          "cgna missing" (the day is absent from the CGNA file),
#                 "odin missing", or "" when both have it
#
# This is the truncation test, and the first thing to run. A downloader that
# keeps only the first page of a paginated answer produces the SAME count on
# almost every day; a source that genuinely reports less produces a count that
# moves with traffic.
# =============================================================================
totalbr_cgna_daily <- function(year, month = NULL,
                               paths = totalbr_cgna_paths(year, month)) {
  count_by_day <- function(path, what) {
    if (!file.exists(path)) {
      message("Not found (treated as empty): ", path); return(NULL)
    }
    message("Reading ", what, ": ", path)
    d <- totalbr_cgna_read(path)
    data.table::data.table(DAY = totalbr_cgna_day(d))[!is.na(DAY)]
  }
  a <- count_by_day(paths$odin, "ODIN")
  b <- count_by_day(paths$cgna, "CGNA")

  agg <- function(d) if (is.null(d))
    data.table::data.table(DAY = character(0), N = integer(0))
    else d[, .(N = .N), by = DAY]

  ta <- agg(a); tb <- agg(b)
  days <- sort(union(ta$DAY, tb$DAY))
  out <- data.table::data.table(
    DAY  = days,
    ODIN = ta$N[match(days, ta$DAY)],
    CGNA = tb$N[match(days, tb$DAY)]
  )
  out[is.na(ODIN), ODIN := 0L][is.na(CGNA), CGNA := 0L]
  out[, RATIO := round(ODIN / pmax(CGNA, 1), 2)]
  out[, FLAG := data.table::fifelse(CGNA == 0L, "cgna missing",
               data.table::fifelse(ODIN == 0L, "odin missing", ""))]

  # The page-limit signature, said out loud rather than left to be spotted in a
  # 31-row table: one count repeating across many days is not what traffic does.
  nz <- out[CGNA > 0L, CGNA]
  if (length(nz) >= 5) {
    top <- sort(table(nz), decreasing = TRUE)[1]
    if (as.integer(top) >= max(3L, ceiling(0.3 * length(nz))))
      message(sprintf(
        "NOTE: %d of %d CGNA day(s) hold exactly %s row(s). A repeated count is the\n",
        as.integer(top), length(nz), names(top)),
        "      signature of a page limit, not of traffic -- check whether every\n",
        "      page of the envelope is being fetched.")
  }
  out[]
}

# =============================================================================
# totalbr_cgna_scope(year, by) -- WHERE the extra rows are, not how many
#
# When the daily profile rules truncation out, the difference is scope. This
# counts each side by aerodrome or by another categorical column, so a source
# that simply does not carry (say) general aviation, or only the IFR movements,
# shows up as a category present on one side and empty on the other.
# =============================================================================
totalbr_cgna_scope <- function(year, by = c("addep", "addes", "co_tipo_voo",
                                            "co_modelo", "co_empresa"),
                               from = NULL, to = NULL, month = NULL,
                               key = "reg_seq", n = 25) {
  by <- match.arg(by)
  w  <- totalbr_cgna_window(year, from, to, month, key = key, quiet = TRUE)
  grab <- function(d) totalbr_cgna_norm(totalbr_cgna_col(
    d, if (by %in% c("addep", "addes")) paste0("co_", by) else by))
  va <- grab(w$a); vb <- grab(w$b)
  if (all(is.na(va)) && all(is.na(vb)))
    stop("Neither side carries a column named like '", by, "'.")
  ta <- table(va, useNA = "ifany"); tb <- table(vb, useNA = "ifany")
  lv <- union(names(ta), names(tb))
  out <- data.table::data.table(GROUP = lv,
                                ODIN  = as.integer(ta[lv]),
                                CGNA  = as.integer(tb[lv]))
  out[is.na(ODIN), ODIN := 0L][is.na(CGNA), CGNA := 0L]
  out[, ONLY := data.table::fifelse(CGNA == 0L, "ODIN only",
              data.table::fifelse(ODIN == 0L, "CGNA only", ""))]
  utils::head(out[order(-(ODIN + CGNA))], n)[]
}
