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
#   "pk"        the row hash, case-folded. Matches only if both sides compute it
#               the same way -- which is exactly what is being tested.
#   "eobt"      callsign + aerodrome pair + the filed off-block time. dh_eobt is
#               a planned value both sources copy from the same flight plan,
#               where dh_inicio is an observed moment each may define
#               differently, so it survives a shift between the sources.
#   "eobt_seq"  the same, plus the rotation number within the day. eobt alone is
#               one key for every rotation of a route in a day, so a side
#               holding three flights and a side holding two still "match" and
#               the missing flight is never reported.
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
totalbr_cgna_key <- function(d, key = "eobt_seq") {
  cs   <- totalbr_cgna_norm(totalbr_cgna_col(d, "co_indicativo"))
  dep  <- totalbr_cgna_norm(totalbr_cgna_col(d, "co_addep"))
  des  <- totalbr_cgna_norm(totalbr_cgna_col(d, "co_addes"))
  eobt <- totalbr_cgna_norm_time(totalbr_cgna_col(d, "dh_eobt"))
  switch(key,
    # the archive writes it uppercase and the API lowercase; folding the case is
    # the only concession made -- a pk that still does not match is a real
    # difference in how the hash is computed
    pk       = totalbr_cgna_norm(totalbr_cgna_col(d, "pk")),
    eobt     = paste(cs, dep, des, substr(eobt, 1, 16), sep = "|"),
    eobt_day = paste(cs, dep, des, substr(eobt, 1, 10), sep = "|"),
    eobt_seq = {
      base <- paste(cs, dep, des, substr(eobt, 1, 10), sep = "|")
      # earliest filed first, so the first flight matches the first and the
      # surplus one is left unmatched -- which is the thing being looked for
      ord    <- order(base, eobt, na.last = TRUE)
      seq_no <- integer(length(base))
      seq_no[ord] <- stats::ave(seq_along(ord), base[ord], FUN = seq_along)
      paste(base, seq_no, sep = "|")
    },
    stop("Unknown key '", key, "'. Use pk, eobt, eobt_day or eobt_seq.")
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

.totalbr_cgna_prep <- function(path, key = "eobt_seq") {
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
                                key = "eobt_seq",
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
                                 keys = c("pk", "eobt", "eobt_seq"),
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
                                     summary_only = TRUE, key = "eobt_seq",
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
totalbr_cgna_examples <- function(year, n = 10, key = "eobt_seq",
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
                               key = "eobt_seq", n = 25) {
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
