#!/usr/bin/env Rscript
# =============================================================================
# harmonise_tatic.R
#
# TATIC as downloaded -> the APDF aerodrome schema the study reports in.
#
#   source(here::here("API_TATIC", "harmonise_tatic.R"))
#   apdf <- harmonise_tatic(tatic_2026)                    # everything
#   apdf <- harmonise_tatic(tatic_2026, apts = bra_apts_names$ICAO)
#
# WHAT IT DOES, AND WHY EACH STEP IS THERE
#
#   1. DROPS THE BRASILIA CLOCKS. Every milestone has a *_BSB twin in local time.
#      Keeping both is how a duration ends up three hours wrong: the two columns
#      look alike, and nothing in a name like `Dep` says which clock it is on.
#      Only the UTC ones survive here.
#   2. RENAMES to the target schema (FLTID, ADEP, ADES, ...).
#   3. FILLS MOV_TIME from whichever end the record has. A movement is a
#      departure or an arrival, so exactly one of `Dep`/`Arr` is populated and
#      the other says nothing; one column carrying whichever exists is what makes
#      the rows comparable.
#   4. TAKES BLOCK_TIME FROM THE PHASE. Off-blocks for a departure is `cPush`;
#      on-blocks for an arrival is `cPos`. They are different events and there is
#      no single column holding both.
#   5. FILTERS to the study aerodromes, when asked, keeping a flight that TOUCHES
#      one -- departing OR arriving, not both.
#
# NOT FILLED IN, DELIBERATELY: `REG` and `CLASS` come back NA. The TATIC field
# list carries no registration, and `CLASS` (the slide's example is "H") could be
# the wake category or the flight class -- two different things. Guessing a
# column here would put a wrong value under a right name, which is worse than an
# empty one. Point them at their source and they take two lines.
# `STAND/BOX` and the C40/C100 rings are not TATIC's at all: the slide sources
# them from BI and SINTESE.
# =============================================================================

# The target schema, in the order the slide lists it. Columns TATIC cannot fill
# are still created, so every year harmonises to the SAME shape and a later join
# does not have to ask which columns exist.
TATIC_APDF_COLS <- c("FLTID", "ADEP", "ADES", "REG", "CLASS", "ARCTYP", "PHASE",
                     "STAND", "RWY", "BLOCK_TIME", "MOV_TIME", "SCHEDULE_TIME",
                     "TATIC_DAY")

# ---- find a column whatever case the export used ----------------------------
# The JSON exports and the API download have disagreed on capitalisation before.
# Matching case-insensitively costs nothing and turns a silent all-NA column into
# a column that works.
.tatic_col <- function(d, name) {
  hit <- names(d)[tolower(names(d)) == tolower(name)]
  if (length(hit) == 0) NULL else hit[1]
}

.tatic_get <- function(d, name) {
  cl <- .tatic_col(d, name)
  if (is.null(cl)) rep(NA_character_, nrow(d)) else as.character(d[[cl]])
}

# ---- times ------------------------------------------------------------------
# Parsed in UTC, because the columns kept here are the UTC ones. Several layouts
# are tried rather than one assumed: the export has used ISO and d/m/Y, and a
# format that does not match returns NA silently, which is exactly the failure
# that is hardest to notice afterwards.
tatic_parse_time <- function(x) {
  if (inherits(x, "POSIXt")) return(x)
  x <- trimws(as.character(x))
  x[!nzchar(x) | x %in% c("NA", "NULL", "-")] <- NA_character_
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  fmts <- c("%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M",
            "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%d/%m/%y %H:%M")
  for (f in fmts) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- as.POSIXct(x[todo], format = f, tz = "UTC")
  }
  # a stamp that matched nothing is worth saying out loud, once
  bad <- sum(is.na(out) & !is.na(x))
  if (bad > 0) message("  ", bad, " timestamp(s) matched no known layout -> NA")
  out
}

# =============================================================================
# harmonise_tatic(d, apts, keep_extra)
#
#   d          : the TATIC data frame (a downloaded year, or several bound)
#   apts       : ICAO codes to keep. A row survives if ADEP **or** ADES is one of
#                them -- a flight that touches the study, not only the domestic
#                legs between two study aerodromes. NULL keeps everything.
#   keep_extra : also carry the TATIC columns the schema has no place for
#                (Locality, Transponder, Equipment, the other milestones), so a
#                question asked later does not need the raw file again.
#
# Returns a tibble with TATIC_APDF_COLS, plus DEP_BRA/ARR_BRA/ROLE when `apts`
# is given, plus the extra columns when asked for.
# =============================================================================
harmonise_tatic <- function(d, apts = NULL, keep_extra = FALSE, quiet = FALSE) {

  if (!is.data.frame(d) || nrow(d) == 0) {
    message("Nothing to harmonise."); return(tibble::tibble())
  }
  say <- function(...) if (!quiet) message(...)
  n_in <- nrow(d)

  # ---- 1. the Brasilia clocks go ------------------------------------------
  bsb <- grep("_BSB$", names(d), ignore.case = TRUE, value = TRUE)
  if (length(bsb) > 0) {
    say("Dropping ", length(bsb), " Brasilia-time column(s).")
    d <- d[, setdiff(names(d), bsb), drop = FALSE]
  }

  # ---- 2. identity ---------------------------------------------------------
  up <- function(x) toupper(trimws(x))
  out <- tibble::tibble(
    FLTID  = up(.tatic_get(d, "Callsign")),
    ADEP   = up(.tatic_get(d, "Adep")),
    ADES   = up(.tatic_get(d, "Ades")),
    REG    = NA_character_,   # no registration in the TATIC field list
    CLASS  = NA_character_,   # source not settled -- see the header
    ARCTYP = up(.tatic_get(d, "AcftType")),
    STAND  = NA_character_,   # BI, not TATIC
    RWY    = up(.tatic_get(d, "Runway"))
  )

  # ---- 3. phase, and the times that depend on it ---------------------------
  dep <- tatic_parse_time(.tatic_get(d, "Dep"))
  arr <- tatic_parse_time(.tatic_get(d, "Arr"))

  # EventType is the source's own answer; where it is absent or says something
  # else, the timestamp the record actually carries decides. A record with
  # neither is left NA rather than assigned a phase it does not have.
  ev <- up(.tatic_get(d, "EventType"))
  out$PHASE <- dplyr::case_when(
    ev %in% c("DEP", "D", "DEPARTURE") ~ "DEP",
    ev %in% c("ARR", "A", "ARRIVAL")   ~ "ARR",
    !is.na(dep) & is.na(arr)           ~ "DEP",
    !is.na(arr) & is.na(dep)           ~ "ARR",
    TRUE                               ~ NA_character_
  )

  # one movement time, from whichever end the record has
  out$MOV_TIME <- dplyr::coalesce(dep, arr)

  # off-blocks for a departure, on-blocks for an arrival
  cpush <- tatic_parse_time(.tatic_get(d, "cPush"))
  cpos  <- tatic_parse_time(.tatic_get(d, "cPos"))
  out$BLOCK_TIME <- dplyr::case_when(out$PHASE == "DEP" ~ cpush,
                                     out$PHASE == "ARR" ~ cpos,
                                     TRUE               ~ as.POSIXct(NA, tz = "UTC"))

  out$SCHEDULE_TIME <- tatic_parse_time(.tatic_get(d, "EOBT"))
  out$TATIC_DAY     <- .tatic_get(d, "TATIC_DAY")

  out <- out[, TATIC_APDF_COLS]

  if (keep_extra) {
    used  <- c("Callsign", "Adep", "Ades", "AcftType", "Runway", "EventType",
               "Dep", "Arr", "cPush", "cPos", "EOBT", "TATIC_DAY")
    extra <- setdiff(names(d), unlist(lapply(used, function(n) .tatic_col(d, n))))
    if (length(extra) > 0) out <- dplyr::bind_cols(out, tibble::as_tibble(d[, extra, drop = FALSE]))
  }

  # ---- 4. the study aerodromes --------------------------------------------
  if (!is.null(apts)) {
    apts <- up(as.character(apts))
    out$DEP_BRA <- out$ADEP %in% apts
    out$ARR_BRA <- out$ADES %in% apts
    out <- out[out$DEP_BRA | out$ARR_BRA, , drop = FALSE]
    out$ROLE <- dplyr::case_when(out$DEP_BRA & out$ARR_BRA ~ "both",
                                 out$DEP_BRA               ~ "departure",
                                 TRUE                      ~ "arrival")
    say("Study aerodromes: ", nrow(out), " of ", n_in, " row(s) touch one.")
  }

  # What the caller needs to know before trusting the result: a movement with no
  # phase has no BLOCK_TIME either, and a movement with no MOV_TIME cannot be
  # placed in time at all.
  say(sprintf("PHASE missing: %d | MOV_TIME missing: %d | BLOCK_TIME missing: %d",
              sum(is.na(out$PHASE)), sum(is.na(out$MOV_TIME)),
              sum(is.na(out$BLOCK_TIME))))
  tibble::as_tibble(out)
}
