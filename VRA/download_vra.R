#!/usr/bin/env Rscript
# =============================================================================
# download_vra.R
#
# ANAC's VRA (Voo Regular Ativo) into ONE FILE PER YEAR, fetched a month at a
# time, resumable, with every month checked against the days it should hold.
#
#   1) From a terminal
#        Rscript VRA/download_vra.R              # the current year
#        Rscript VRA/download_vra.R 2024 2025
#
#   2) From R / a Quarto chunk
#        source(here::here("VRA", "download_vra.R"))
#        download_vra(2024:2026)
#
# Output: data-raw/vra/vra_<year>.csv, with the month parts under parts/.
#
# ---------------------------------------------------------------------------
# WHAT THIS SOURCE IS, AND WHY IT IS NOT LIKE THE OTHERS
#
# VRA is filed BY THE AIRLINES: the scheduled and the realised times of every
# regular flight, plus ANAC's own verdict on each one (ds_situacao_voo,
# ds_situacao_partida, ds_situacao_chegada, ds_justificativa). It is
# schedule adherence as declared, not movement as observed by ATC. So a
# disagreement between VRA and TOTALBR is not automatically an ingestion fault:
# they are two different measurements of the same flights.
#
# FOUR THINGS ABOUT THE ENDPOINT THAT THIS FILE ENCODES
#
#   The window is dt_referencia1 / dt_referencia2, both DDMMYYYY, both
#   INCLUSIVE. Neither ISO nor d/m/Y; a wrong format comes back empty rather
#   than as an error.
#
#   The answer is JSON INSIDE JSON -- a JSON string wrapping the array of
#   records. One decode yields a character vector of length one. See
#   vra_parse_json() in probe_vra.R.
#
#   dt_referencia dates a flight BY ITS DEPARTURE, and comes as dd/mm/YYYY with
#   no time. A night flight therefore carries a dt_chegada_real on the following
#   day -- AAL0904 leaves SBGL on 01/12 at 23:51 and lands at KMIA on 02/12. The
#   month windows filter on dt_referencia; never read a time of day off it.
#
#   There is NO unique key. No id, no hash. sg_empresa_icao + nr_voo +
#   dt_partida_prevista is the natural candidate, but it is a candidate until
#   measured -- TOTALBR's `id` looked like a key and was not, and de-duplicating
#   on it would have deleted almost every row. So nothing is de-duplicated here.
#   The .qmd measures it; the downloader keeps exactly what the API served.
# ---------------------------------------------------------------------------
# =============================================================================

source(here::here("VRA", "probe_vra.R"))   # VRA_API_URL, vra_window, vra_parse_json

VRA_PREFIX   <- "vra"
VRA_DATE_COL <- "dt_referencia"

vra_default_years <- function() {
  if (exists("vra_data_years", inherits = TRUE))
    get("vra_data_years", inherits = TRUE)
  else as.integer(format(Sys.Date(), "%Y"))
}

# ---- the days a month holds, read off what arrived --------------------------
# Coverage is judged by DAYS PRESENT, never by "the request returned". A month
# fetched while it was still open is short by design and must be fetched again
# once it closes; a month that came back truncated is short by fault. Both look
# the same here, which is the point -- neither is accepted as complete.
vra_days_in <- function(d, date_col = VRA_DATE_COL) {
  if (is.null(d) || nrow(d) == 0 || !date_col %in% names(d)) return(character(0))
  sort(unique(substr(as.character(d[[date_col]]), 1, 10)))
}

vra_month_days <- function(year, month) {
  from <- as.Date(sprintf("%d-%02d-01", year, month))
  to   <- seq(from, by = "month", length.out = 2)[2] - 1
  format(seq(from, to, by = "day"), "%d/%m/%Y")
}

# =============================================================================
# vra_fetch(from, to)
#
# One window, as a data frame. Returns an empty frame rather than failing when
# the window holds nothing, so a caller can tell "no rows" from "no answer".
# =============================================================================
vra_fetch <- function(from, to, url = VRA_API_URL, timeout = 600) {
  req  <- .vra_req(url, vra_window(from, to), timeout = timeout)
  resp <- httr2::req_perform(req)
  st   <- httr2::resp_status(resp)
  if (st >= 400)
    stop("VRA refused ", vra_date(from), "-", vra_date(to), ": HTTP ", st, " ",
         substr(httr2::resp_body_string(resp), 1, 300))
  obj <- vra_parse_json(httr2::resp_body_string(resp))
  if (is.data.frame(obj)) return(obj)
  if (is.null(obj) || length(obj) == 0) return(data.frame())
  out <- tryCatch(as.data.frame(data.table::rbindlist(obj, fill = TRUE,
                                                      use.names = TRUE)),
                  error = function(e) data.frame())
  out
}

# =============================================================================
# download_vra(years, out_dir, force, chunk)
#
#   years   : one or more years
#   force   : re-fetch months already on disk and complete
#   chunk   : "month" asks for a whole month in one request; "day" walks the
#             month a day at a time. Month is the default because it is one
#             request instead of thirty -- but if the API turns out to ignore
#             the far bound (TATIC answers a wide window with only its first
#             day, silently), the day-count check below CATCHES it and says so,
#             and chunk = "day" is then the whole fix.
#
# Each month is written to parts/ as soon as it arrives, so an interrupted run
# keeps everything it had. The year file is rebuilt from the parts at the end.
# =============================================================================
download_vra <- function(years   = vra_default_years(),
                         out_dir = here::here("data-raw", "vra"),
                         force   = FALSE,
                         chunk   = c("month", "day"),
                         quiet   = FALSE) {

  chunk     <- match.arg(chunk)
  parts_dir <- file.path(out_dir, "parts")
  for (p in c(out_dir, parts_dir))
    if (!dir.exists(p)) dir.create(p, recursive = TRUE)

  today <- Sys.Date()
  say   <- function(...) if (!quiet) message(...)

  for (yr in as.integer(years)) {
    say("VRA ", yr)
    for (mo in 1:12) {
      first <- as.Date(sprintf("%d-%02d-01", yr, mo))
      last  <- seq(first, by = "month", length.out = 2)[2] - 1
      if (first > today) next                      # a month that has not begun
      f     <- file.path(parts_dir, sprintf("vra_%d-%02d.csv", yr, mo))
      want  <- vra_month_days(yr, mo)
      # a month still running is only expected up to today
      if (last >= today) want <- want[as.Date(want, "%d/%m/%Y") <= today]

      if (file.exists(f) && !force) {
        have <- vra_days_in(data.table::fread(file = f, sep = ";",
                                              select = VRA_DATE_COL,
                                              colClasses = "character",
                                              showProgress = FALSE))
        # complete is a statement about DAYS, not about the file existing
        if (length(setdiff(want, have)) == 0) {
          say(sprintf("  %d-%02d  have %d/%d day(s), skip", yr, mo,
                      length(have), length(want)))
          next
        }
        say(sprintf("  %d-%02d  have %d/%d day(s), refetching", yr, mo,
                    length(have), length(want)))
      }

      d <- if (chunk == "month") {
        say(sprintf("  %d-%02d  fetching the month ...", yr, mo))
        vra_fetch(first, min(last, today))
      } else {
        days <- as.Date(want, "%d/%m/%Y")
        say(sprintf("  %d-%02d  fetching %d day(s) ...", yr, mo, length(days)))
        acc <- lapply(days, function(dd) vra_fetch(dd, dd))
        acc <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, acc)
        if (length(acc) == 0) data.frame() else
          as.data.frame(data.table::rbindlist(acc, fill = TRUE, use.names = TRUE))
      }

      if (nrow(d) == 0) { say("    nothing returned"); next }

      got <- vra_days_in(d)
      # THE CHECK THAT MATTERS. Asking for a month and receiving one day is not
      # an error the API reports; it is a silence. Say it out loud.
      missing <- setdiff(want, got)
      if (length(missing) > 0) {
        say(sprintf("    WARNING: asked %d day(s), got %d. Missing: %s",
                    length(want), length(got),
                    paste(utils::head(missing, 5), collapse = ", ")))
        if (chunk == "month" && length(got) <= 1)
          say("    A month window that returns a single day means the far bound ",
              "is being ignored. Re-run with chunk = \"day\".")
      }
      data.table::fwrite(d, f, sep = ";", na = "", quote = TRUE)
      say(sprintf("    %s rows, %d day(s) -> %s",
                  format(nrow(d), big.mark = ","), length(got), basename(f)))
    }

    # ---- the year file, rebuilt from whatever parts exist ------------------
    # Always from the parts, never appended to: a year assembled by appending
    # carries whatever a half-finished earlier run left behind.
    ps <- list.files(parts_dir, pattern = sprintf("^vra_%d-\\d{2}\\.csv$", yr),
                     full.names = TRUE)
    if (length(ps) == 0) next
    all <- data.table::rbindlist(
      lapply(sort(ps), function(p) data.table::fread(file = p, sep = ";",
                                                     colClasses = "character",
                                                     showProgress = FALSE,
                                                     fill = TRUE)),
      fill = TRUE, use.names = TRUE)
    yf <- file.path(out_dir, sprintf("vra_%d.csv", yr))
    data.table::fwrite(all, yf, sep = ";", na = "", quote = TRUE)
    say(sprintf("  %d: %s row(s), %d day(s) -> %s", yr,
                format(nrow(all), big.mark = ","),
                length(vra_days_in(all)), basename(yf)))
  }
  invisible(TRUE)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  download_vra(if (length(args) == 0) vra_default_years() else as.integer(args))
}
