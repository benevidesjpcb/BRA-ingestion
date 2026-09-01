#!/usr/bin/env Rscript
# =============================================================================
# probe_vra.R
#
# ASK THE VRA API WHAT IT IS, BEFORE WRITING A DOWNLOADER FOR IT.
#
#   source(here::here("VRA", "probe_vra.R"))
#   vra_probe()                                  # the base endpoint, as it comes
#   vra_probe(query = list(ano = 2026, mes = 1)) # try a parameter shape
#
# WHY THIS EXISTS AS A STEP OF ITS OWN
# Every downloader in this project keys on things that cannot be guessed: which
# column carries the date, what the month window parameter is called, how many
# rows a page holds, whether there is a unique id. Guessing them costs hours of
# wrong download; asking costs one request. odin_probe() does the same job for
# ODIN, and the two TATIC surprises -- a wide window silently returning one day,
# and the milestones being CamelCase -- are what a probe is for.
#
# It does NOT assume the answer is JSON. The endpoint may serve an HTML
# documentation page, a CSV, or a JSON envelope with the rows nested inside; the
# probe reports what actually arrived and only then tries to read it.
#
# VRA is ANAC's "Voo Regular Ativo": the scheduled and realised times of the
# regular flights of Brazilian and foreign operators, filed by the airlines. It
# is a DIFFERENT KIND of source from the others here -- airline-reported schedule
# adherence, not ATC-observed movement -- so nothing about its shape should be
# assumed from dstaxi, kpi08 or TOTALBR.
# =============================================================================

source(here::here("proxy.R"))

VRA_API_URL <- Sys.getenv("VRA_API_URL",
                          unset = "https://sas.anac.gov.br/sas/vra_api")

.vra_req <- function(url = VRA_API_URL, query = list(), timeout = 120) {
  for (p in c("httr2", "jsonlite"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' is required. install.packages('", p, "').")
  req <- httr2::request(url) |>
    httr2::req_user_agent("BRA-ingestion/vra") |>
    httr2::req_timeout(timeout) |>
    bra_proxy()
  if (length(query) > 0) req <- do.call(httr2::req_url_query, c(list(req), query))
  # a 4xx/5xx should be READ, not thrown: the body of an error is usually where
  # the API says which parameter it wanted
  httr2::req_error(req, is_error = function(resp) FALSE)
}

# =============================================================================
# vra_probe(url, query, n)
#
# Performs one request and reports, in this order: the URL as sent, the status,
# the content type, the size, and then whatever the body turns out to be --
# columns and first rows for tabular JSON or CSV, the keys for a JSON envelope,
# the first lines for HTML or anything unrecognised.
#
# Returns the parsed object invisibly when it could parse one, so the result can
# be inspected further without a second request.
# =============================================================================
vra_probe <- function(url = VRA_API_URL, query = list(), n = 5L) {
  req  <- .vra_req(url, query)
  message("GET ", req$url)
  resp <- httr2::req_perform(req)

  status <- httr2::resp_status(resp)
  ctype  <- tryCatch(httr2::resp_content_type(resp), error = function(e) "unknown")
  body   <- httr2::resp_body_string(resp)
  message("status: ", status, " | type: ", ctype, " | ", nchar(body), " bytes")

  if (status >= 400) {
    message("The API refused the request. Its own words:")
    cat(substr(body, 1, 1500), "\n")
    return(invisible(NULL))
  }

  # ---- JSON ---------------------------------------------------------------
  if (grepl("json", ctype, ignore.case = TRUE) ||
      grepl("^\\s*[\\[{]", body)) {
    obj <- tryCatch(jsonlite::fromJSON(body, flatten = TRUE),
                    error = function(e) { message("Not valid JSON: ",
                                                  conditionMessage(e)); NULL })
    if (is.null(obj)) { cat(substr(body, 1, 1000), "\n"); return(invisible(NULL)) }

    if (is.data.frame(obj)) return(invisible(.vra_report_df(obj, n)))

    # an envelope: the rows are under one of the keys, and which one is exactly
    # the sort of thing worth reading rather than assuming
    message("A JSON object, not a table. Top-level keys: ",
            paste(names(obj), collapse = ", "))
    for (k in names(obj)) {
      el <- obj[[k]]
      message("  $", k, ": ", class(el)[1],
              if (is.data.frame(el)) paste0(" [", nrow(el), " x ", ncol(el), "]")
              else if (is.atomic(el) && length(el) <= 3) paste0(" = ", paste(el, collapse = ", "))
              else paste0(" (length ", length(el), ")"))
      if (is.data.frame(el) && nrow(el) > 0) {
        message("  -> this looks like the rows:")
        .vra_report_df(el, n)
      }
    }
    return(invisible(obj))
  }

  # ---- CSV ----------------------------------------------------------------
  if (grepl("csv|text/plain", ctype, ignore.case = TRUE)) {
    # the separator is not a given: ANAC's published VRA files are semicolon-
    # delimited, but an API may well answer with commas
    sep <- if (lengths(regmatches(body, gregexpr(";", body)))[1] >
               lengths(regmatches(body, gregexpr(",", body)))[1]) ";" else ","
    message("Reading as CSV, separator '", sep, "'")
    d <- tryCatch(data.table::fread(text = body, sep = sep, showProgress = FALSE),
                  error = function(e) { message("fread failed: ",
                                                conditionMessage(e)); NULL })
    if (!is.null(d)) return(invisible(.vra_report_df(as.data.frame(d), n)))
  }

  # ---- anything else ------------------------------------------------------
  message("Neither JSON nor CSV. The first lines, so you can see what it is:")
  cat(paste(utils::head(strsplit(body, "\n")[[1]], 25), collapse = "\n"), "\n")
  invisible(NULL)
}

# ---- what a table looks like ------------------------------------------------
.vra_report_df <- function(d, n = 5L) {
  message("Rows: ", nrow(d), " | columns: ", ncol(d))
  message("Columns: ", paste(names(d), collapse = ", "))
  # the two things a downloader has to be told, so name the candidates
  dt <- grep("(dat|dt|hora|time|partida|chegada|previst|real)", names(d),
             value = TRUE, ignore.case = TRUE)
  if (length(dt) > 0) message("Date-like: ", paste(dt, collapse = ", "))
  id <- grep("(^id$|_id$|chave|hash|pk|numero|voo)", names(d),
             value = TRUE, ignore.case = TRUE)
  if (length(id) > 0) message("Id-like: ", paste(id, collapse = ", "))
  print(utils::head(as.data.frame(d), n))
  invisible(d)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) vra_probe()
