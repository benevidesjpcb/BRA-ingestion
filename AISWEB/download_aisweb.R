#!/usr/bin/env Rscript
# =============================================================================
# download_aisweb.R
#
# The AISWEB API -- the aeronautical information the ICA/DECEA publishes:
# aerodromes (ROTAER), location indicators (GEILOC), waypoints, routes, NOTAM,
# charts. Documented at https://documenter.getpostman.com/view/7201070/SzKQyg3H
#
#   source(here::here("AISWEB", "download_aisweb.R"))
#   aisweb_check("geiloc", name = "SBGR")      # START HERE -- one request, shown
#   download_aisweb_geiloc()                   # every location indicator
#   download_aisweb_rotaer()                   # every aerodrome in the ROTAER
#   download_aisweb_waypoints()                # every waypoint
#   download_aisweb_aerodromes(c("SBGR", "SBSP"))   # the detail, with runways
#
# The files land in data-raw/aisweb/, one per area:
#
#   aisweb_geiloc.csv   aisweb_rotaer.csv   aisweb_waypoints.csv
#   aisweb_aerodromes.csv   aisweb_runways.csv
#
# ---------------------------------------------------------------------------
# THIS IS REFERENCE DATA, NOT A FEED. The other downloads in this project are
# movements, walked day by day. AISWEB describes the things movements happen
# AT -- which aerodromes exist, where they are, which runways they have -- and
# it changes with the AIRAC amendment, not with the clock. So there is no day
# to resume at: an area is fetched whole and its file is replaced, and the date
# it was fetched on is written into the file (AISWEB_FETCHED) because that is
# the only thing that says how old the picture is.
#
# ONE ENDPOINT, AND THE AREA IS A PARAMETER. Every call is a GET on the same URL
#   https://api.decea.mil.br/aisweb/?apiKey=..&apiPass=..&area=<area>&...
# and what comes back is XML, not JSON.
#
# A REFUSAL IS AN HTTP 200. Asked without credentials, the endpoint answers 200
# with an HTML page reading "Erro nos parametros obrigatorios" -- measured, not
# read in the documentation. The status code therefore says nothing: an answer
# counts only if it parses as XML with an <aisweb> root, and anything else is
# reported with the text the server sent.
#
# THE ROTAER LIST IS PAGED BY rowstart/rowend, AND THE DOCUMENTATION DOES NOT
# SAY WHAT THEY MEAN. Its one example asks rowstart=3000&rowend=50 and receives
# 50 aerodromes under total="50" -- so rowend behaves as a page SIZE, and
# `total` as the rows in this answer rather than in the ROTAER. That is an
# inference from a single example. The paging below does not depend on it being
# right: it advances by the rows actually received, stops on a short or empty
# page, and stops if a page brings back nothing it has not already seen -- which
# is what an API that ignored rowstart would produce, for ever.
#
# The credentials are read from the environment, never hardcoded: AISWEB_API_KEY
# and AISWEB_API_PASS in .Renviron (git-ignored). See setup_renviron.R.
# =============================================================================

# the CSV conventions (separator, read/write, bind) and the proxy
source(here::here("CGNA", "cgna_common.R"))

AISWEB_URL <- local({
  u <- Sys.getenv("AISWEB_URL", unset = "")
  if (nzchar(u)) u else "https://api.decea.mil.br/aisweb/"
})
AISWEB_OUT_DIR  <- here::here("data-raw", "aisweb")
AISWEB_TIMEOUT  <- 120
# Aerodromes per request on the ROTAER list. Not documented; 500 keeps the
# ROTAER (a few thousand aerodromes) to a handful of calls.
AISWEB_PAGE_SIZE <- local({
  n <- suppressWarnings(as.integer(Sys.getenv("AISWEB_PAGE_SIZE", unset = "500")))
  if (is.na(n) || n < 1L) 500L else n
})

# ---- the credential ----------------------------------------------------------
aisweb_credentials <- function() {
  key  <- Sys.getenv("AISWEB_API_KEY",  unset = "")
  pass <- Sys.getenv("AISWEB_API_PASS", unset = "")
  if (!nzchar(key) || !nzchar(pass))
    stop("AISWEB_API_KEY / AISWEB_API_PASS are not set. Put both in .Renviron ",
         "(git-ignored):\n",
         "  source(here::here(\"setup_renviron.R\")); setup_renviron()\n",
         "  set_token(\"AISWEB_API_KEY\"); set_token(\"AISWEB_API_PASS\")\n",
         "and restart R. Never write them into a script.", call. = FALSE)
  list(key = key, pass = pass)
}

# ---- one request -------------------------------------------------------------
# list(ok, status, secs, type, body, doc, error). ok = TRUE only when the body
# is XML rooted at <aisweb>; `error` otherwise carries what the server said, as
# text, because the status code does not (see the header).
aisweb_fetch <- function(area, ..., base_url = AISWEB_URL,
                         timeout = AISWEB_TIMEOUT) {
  cred  <- aisweb_credentials()
  extra <- list(...)
  extra <- extra[!vapply(extra, is.null, logical(1))]

  req <- httr2::request(base_url) |>
    httr2::req_url_query(apiKey = cred$key, apiPass = cred$pass, area = area,
                         !!!extra) |>
    httr2::req_user_agent("BRA-ingestion/aisweb") |>
    httr2::req_timeout(timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    bra_proxy()

  t0   <- Sys.time()
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(resp, "error"))
    return(list(ok = FALSE, status = NA_integer_, secs = secs, type = NA_character_,
                body = "", doc = NULL,
                error = paste("transport error:", conditionMessage(resp))))

  st   <- httr2::resp_status(resp)
  type <- tryCatch(httr2::resp_content_type(resp), error = function(e) NA_character_)
  body <- tryCatch(httr2::resp_body_string(resp, encoding = "UTF-8"),
                   error = function(e) "")
  # as raw bytes: handed a string that does not look like XML, read_xml() takes
  # it for a file path -- and a refusal is exactly such a string
  doc  <- tryCatch(xml2::read_xml(charToRaw(enc2utf8(body))), error = function(e) NULL)

  ok <- st < 400 && !is.null(doc) && identical(xml2::xml_name(doc), "aisweb")
  list(ok = ok, status = st, secs = secs, type = type, body = body,
       doc = if (ok) doc else NULL,
       error = if (ok) NA_character_ else .aisweb_said(body, st))
}

# what a non-XML answer says, with the markup and the blank lines taken out
.aisweb_said <- function(body, status) {
  txt <- gsub("<[^>]+>", " ", body)
  txt <- trimws(gsub("\\s+", " ", txt))
  if (!nzchar(txt)) txt <- "(empty body)"
  sprintf("HTTP %s, not an <aisweb> answer: %s", status, substr(txt, 1, 300))
}

# ---- XML -> rows --------------------------------------------------------------
# The record nodes of an answer. Most areas wrap them once -- <aisweb><rotaer>
# <item>.. -- and the wrapper carries the counters (total, emenda, lastupdate).
# Found by shape rather than by name, so an area not listed here still reads:
# the wrapper is the first child of the root that has element children of its
# own, and the records are those children.
aisweb_wrapper <- function(doc) {
  kids <- xml2::xml_children(doc)
  has  <- vapply(kids, function(k) length(xml2::xml_children(k)) > 0, logical(1))
  if (!any(has)) return(NULL)
  kids[[which(has)[1]]]
}

# One record -> a named character vector: its attributes, then its children.
# A child that has children of its own (a runway inside an aerodrome) cannot be
# a cell; it is collapsed to its text, pipe-separated, the same convention the
# other downloaders use for a nested array. The tables that need those nested
# records as rows of their own read them explicitly (see aisweb_runways()).
.aisweb_record <- function(node) {
  at   <- xml2::xml_attrs(node)
  kids <- xml2::xml_children(node)
  if (length(kids) == 0) {
    v <- c(at, value = xml2::xml_text(node, trim = TRUE))
    return(v[!duplicated(names(v))])
  }
  nm  <- xml2::xml_name(kids)
  val <- vapply(kids, function(k) {
    sub <- xml2::xml_children(k)
    if (length(sub) == 0) xml2::xml_text(k, trim = TRUE)
    else paste(xml2::xml_text(sub, trim = TRUE), collapse = "|")
  }, character(1))
  # the same tag twice in one record (two <light>) -> one cell, pipe-separated
  val <- tapply(val, factor(nm, levels = unique(nm)), paste, collapse = "|")
  v <- c(at, stats::setNames(as.character(val), names(val)))
  v[!duplicated(names(v))]
}

# The rows of an answer as an all-character data.frame (0 rows when the answer
# holds no record). Empty strings become NA, as in every other raw file here.
aisweb_rows <- function(doc) {
  w <- aisweb_wrapper(doc)
  if (is.null(w)) return(data.frame())
  recs <- lapply(xml2::xml_children(w), .aisweb_record)
  recs <- Filter(function(r) length(r) > 0, recs)
  if (length(recs) == 0) return(data.frame())
  cols <- unique(unlist(lapply(recs, names)))
  out  <- as.data.frame(
    do.call(rbind, lapply(recs, function(r) unname(r[cols]))),
    stringsAsFactors = FALSE)
  names(out) <- cols
  out[] <- lapply(out, function(x) { x[!is.na(x) & !nzchar(x)] <- NA_character_; x })
  out
}

# the counters the wrapper carries (total, emenda, lastupdate ...), as text
aisweb_meta <- function(doc) {
  w <- aisweb_wrapper(doc)
  if (is.null(w)) return(character(0))
  c(wrapper = xml2::xml_name(w), xml2::xml_attrs(w))
}

# =============================================================================
# aisweb_check(area, ...) -- ONE request, everything it answered
#
#   aisweb_check("geiloc", name = "SBGR")
#   aisweb_check("rotaer", rowstart = 0, rowend = 5)
#   aisweb_check("rotaer", icaoCode = "SBGR")        # the aerodrome detail
#   aisweb_check("waypoints")
#
# The URL, the query with the credentials redacted, the status AND how long it
# took, the start of the raw body, the wrapper's counters, the columns found
# and the first row. Run it before a download and again whenever one comes back
# empty: it is the difference between "the API is down", "the credential is
# wrong" and "the area simply holds nothing for that question".
# =============================================================================
aisweb_check <- function(area, ..., base_url = AISWEB_URL) {
  extra <- list(...)
  q <- paste(c("apiKey=<redacted>", "apiPass=<redacted>", paste0("area=", area),
               if (length(extra)) paste0(names(extra), "=", unlist(extra))),
             collapse = " ")
  message("URL      : ", base_url)
  message("Query    : ", q)
  r <- aisweb_fetch(area, ..., base_url = base_url)
  message(sprintf("Status   : %s  (%.1fs)  %s",
                  if (is.na(r$status)) "no answer" else paste("HTTP", r$status),
                  r$secs, if (is.na(r$type)) "" else r$type))
  message("Body[1:400]:\n", substr(trimws(r$body), 1, 400), "\n")
  if (!r$ok) {
    message("NOT AN ANSWER: ", r$error)
    return(invisible(r))
  }
  meta <- aisweb_meta(r$doc)
  if (length(meta))
    message("Wrapper  : ", paste0(names(meta), "=", meta, collapse = "  "))
  rows <- aisweb_rows(r$doc)
  message(sprintf("Rows     : %d x %d", nrow(rows), ncol(rows)))
  if (nrow(rows) > 0) {
    message("Columns  : ", paste(names(rows), collapse = ", "))
    message("\nFirst row:")
    print(utils::head(rows, 1), row.names = FALSE)
  } else {
    message("No record under a wrapper. Top-level elements: ",
            paste(unique(xml2::xml_name(xml2::xml_children(r$doc))), collapse = ", "))
  }
  invisible(r)
}

# ---- writing -------------------------------------------------------------------
.aisweb_write <- function(df, name, out_dir) {
  if (!dir.exists(out_dir)) { dir.create(out_dir, recursive = TRUE); message("Created ", out_dir) }
  df$AISWEB_FETCHED <- format(Sys.Date())
  path <- file.path(out_dir, sprintf("aisweb_%s.csv", name))
  cgna_write_csv(df, path)
  message(sprintf("%s: %d record(s), %d column(s) -> %s", name, nrow(df), ncol(df), path))
  invisible(path)
}

# =============================================================================
# download_aisweb(area, ..., name) -- an area that answers in ONE call
#
# geiloc and waypoints are not paged: asked with no filter, the whole table
# comes back. Nothing is written when the call fails -- a file left from an
# earlier run is worth more than an empty one in its place.
# =============================================================================
download_aisweb <- function(area, ..., name = area, out_dir = AISWEB_OUT_DIR) {
  message(sprintf("AISWEB %s ...", area))
  r <- aisweb_fetch(area, ...)
  if (!r$ok) { message("  FAILED: ", r$error); return(invisible(NULL)) }
  rows <- aisweb_rows(r$doc)
  message(sprintf("  %d record(s) in %.1fs", nrow(rows), r$secs))
  if (nrow(rows) == 0) { message("  nothing to write."); return(invisible(NULL)) }
  .aisweb_write(rows, name, out_dir)
}

download_aisweb_geiloc    <- function(...) download_aisweb("geiloc", ...)
download_aisweb_waypoints <- function(...) download_aisweb("waypoints", ...)

# =============================================================================
# download_aisweb_rotaer(...) -- the ROTAER list, every page
#
# Filters go through as documented (uf = "SP", type = "AD", fir = ...). See the
# header for why the loop trusts the rows it receives and not `total`.
# =============================================================================
download_aisweb_rotaer <- function(..., page_size = AISWEB_PAGE_SIZE,
                                   max_pages = 200L, out_dir = AISWEB_OUT_DIR) {
  message(sprintf("AISWEB rotaer, %d aerodrome(s) per page ...", page_size))
  pages <- list(); seen <- character(0); start <- 0L
  for (p in seq_len(max_pages)) {
    r <- aisweb_fetch("rotaer", rowstart = start, rowend = page_size, ...)
    if (!r$ok) {
      message(sprintf("  page %d (rowstart %d) FAILED: %s", p, start, r$error))
      message("  Nothing written: a ROTAER short of its last pages would read as ",
              "a complete one.")
      return(invisible(NULL))
    }
    rows <- aisweb_rows(r$doc)
    meta <- aisweb_meta(r$doc)
    message(sprintf("  page %d  rowstart %-6d %4d row(s)  total=%s  %.1fs", p, start,
                    nrow(rows), if ("total" %in% names(meta)) meta[["total"]] else "?",
                    r$secs))
    if (nrow(rows) == 0) break
    id  <- if ("id" %in% names(rows)) rows$id else do.call(paste, c(rows, sep = "|"))
    new <- !(id %in% seen)
    if (!any(new)) {
      message("  this page repeats rows already held: rowstart is not advancing ",
              "the answer. Stopping.")
      break
    }
    pages[[length(pages) + 1L]] <- rows[new, , drop = FALSE]
    seen  <- c(seen, id[new])
    start <- start + nrow(rows)
    if (nrow(rows) < page_size) break
  }
  all <- cgna_rbind_fill(pages)
  if (is.null(all)) { message("  nothing to write."); return(invisible(NULL)) }
  .aisweb_write(all, "rotaer", out_dir)
}

# =============================================================================
# download_aisweb_aerodromes(icao) -- the detail of each aerodrome, and its runways
#
# One call per aerodrome (area=rotaer&icaoCode=XXXX). Unlike the list, the answer
# is not a table: the aerodrome's fields sit directly under <aisweb>, and its
# runways are nested records. Two files come out of it, because they are two
# grains:
#
#   aisweb_aerodromes.csv   one row per aerodrome
#   aisweb_runways.csv      one row per runway, keyed on AeroCode
#
# An aerodrome that fails is named and left out; the others are still written.
# =============================================================================
aisweb_aerodrome <- function(doc) {
  kids <- xml2::xml_children(doc)
  leaf <- vapply(kids, function(k) length(xml2::xml_children(k)) == 0, logical(1))
  v <- stats::setNames(xml2::xml_text(kids[leaf], trim = TRUE), xml2::xml_name(kids[leaf]))
  v <- v[!duplicated(names(v))]
  org <- xml2::xml_find_first(doc, "./org")
  if (!inherits(org, "xml_missing")) {
    o <- xml2::xml_children(org)
    v <- c(v, stats::setNames(xml2::xml_text(o, trim = TRUE),
                              paste0("org_", xml2::xml_name(o))))
  }
  v[["runways_count"]] <- as.character(length(xml2::xml_find_all(doc, "./runways/runway")))
  as.data.frame(as.list(v), stringsAsFactors = FALSE, check.names = FALSE)
}

aisweb_runways <- function(doc) {
  code <- xml2::xml_text(xml2::xml_find_first(doc, "./AeroCode"), trim = TRUE)
  rw   <- xml2::xml_find_all(doc, "./runways/runway")
  if (length(rw) == 0) return(NULL)
  one <- function(node) {
    f <- function(tag) {
      x <- xml2::xml_find_first(node, paste0("./", tag))
      if (inherits(x, "xml_missing")) NA_character_ else xml2::xml_text(x, trim = TRUE)
    }
    data.frame(AeroCode = code, ident = f("ident"), type = f("type"),
               surface = f("surface"), length = f("length"), width = f("width"),
               surface_c = f("surface_c"),
               thr = paste(xml2::xml_text(xml2::xml_find_all(node, "./thr/ident"),
                                          trim = TRUE), collapse = "|"),
               stringsAsFactors = FALSE)
  }
  do.call(rbind, lapply(rw, one))
}

download_aisweb_aerodromes <- function(icao, out_dir = AISWEB_OUT_DIR) {
  icao <- unique(toupper(trimws(icao)))
  ad <- list(); rw <- list(); failed <- character(0)
  for (i in seq_along(icao)) {
    r <- aisweb_fetch("rotaer", icaoCode = icao[i])
    if (!r$ok) {
      message(sprintf("  %s  FAILED: %s", icao[i], r$error))
      failed <- c(failed, icao[i]); next
    }
    a <- aisweb_aerodrome(r$doc)
    if (!"AeroCode" %in% names(a) || is.na(a$AeroCode) || !nzchar(a$AeroCode)) {
      message(sprintf("  %s  no aerodrome in the answer", icao[i]))
      failed <- c(failed, icao[i]); next
    }
    w <- aisweb_runways(r$doc)
    message(sprintf("  %s  %s, %d runway(s)  %.1fs", icao[i],
                    if ("name" %in% names(a)) a$name else "?",
                    if (is.null(w)) 0L else nrow(w), r$secs))
    ad[[length(ad) + 1L]] <- a
    if (!is.null(w)) rw[[length(rw) + 1L]] <- w
  }
  if (length(failed))
    message(sprintf("%d aerodrome(s) not fetched: %s", length(failed),
                    paste(failed, collapse = ", ")))
  out <- character(0)
  a <- cgna_rbind_fill(ad); w <- cgna_rbind_fill(rw)
  if (!is.null(a)) out <- c(out, .aisweb_write(a, "aerodromes", out_dir))
  if (!is.null(w)) out <- c(out, .aisweb_write(w, "runways", out_dir))
  invisible(out)
}

# ---- run only when executed as a script (not when sourced) --------------------
#   Rscript AISWEB/download_aisweb.R            # geiloc, rotaer, waypoints
if (sys.nframe() == 0L) {
  download_aisweb_geiloc()
  download_aisweb_rotaer()
  download_aisweb_waypoints()
}
