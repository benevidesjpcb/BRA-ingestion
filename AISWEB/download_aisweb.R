#!/usr/bin/env Rscript
# =============================================================================
# download_aisweb.R
#
# The AISWEB API -- the aeronautical information the ICA/DECEA publishes:
# aerodromes (ROTAER), location indicators (GEILOC), waypoints, routes, NOTAM,
# charts. Documented at https://documenter.getpostman.com/view/7201070/SzKQyg3H
#
#   source(here::here("AISWEB", "download_aisweb.R"))
#   aisweb_check("geiloc", name = "cong", type = "ad", feature = "airport")      # START HERE -- one request, shown
#   download_aisweb_rotaer()                   # every aerodrome in the ROTAER
#   download_aisweb_geiloc()                   # location indicators: AD, HP, HD
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
# PAGED BY rowstart/rowend, WHICH THE DOCUMENTATION DOES NOT EXPLAIN. Measured
# against the service: rowend is the page SIZE (not the last row), and the
# wrapper's `total` is the size of the WHOLE result, repeated on every page --
# the ROTAER answered total=6123 thirteen times and delivered 12 x 500 + 123.
# So a download can be checked against what the API itself says it holds, and
# it is: a total that does not match what was stored is said out loud.
#
# AN AREA CAN BE PAGED WITHOUT SAYING SO. waypoints is documented as a call
# with no parameters, and asked that way it returns exactly 100 rows -- a round
# number, which is what a default page size looks like and not what a table of
# waypoints looks like. The first page alone is therefore never taken for the
# table. Every list is walked the same way: advance by the rows received, and
# stop only on an empty page, on a page that brings nothing new (which is what
# an area that ignores rowstart produces, for ever), or when `total` is reached.
# A SHORT page does not end the walk -- a server that caps the page below what
# was asked returns short pages all the way through.
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
#   aisweb_check("geiloc", name = "cong", type = "ad", feature = "airport")
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
  # The aerodrome detail is not a table: its fields sit directly under <aisweb>.
  # Read as one, the first nested element (the operator) would be taken for the
  # wrapper and the answer shown as a single cell -- "GRU Airport" and nothing
  # else. It is shown as what it is: one aerodrome, and its runways.
  if (!inherits(xml2::xml_find_first(r$doc, "./AeroCode"), "xml_missing")) {
    a <- aisweb_aerodrome(r$doc); w <- aisweb_runways(r$doc)
    message(sprintf("Aerodrome: %d field(s), %d runway(s)", ncol(a),
                    if (is.null(w)) 0L else nrow(w)))
    print(t(a[1, , drop = FALSE]))
    if (!is.null(w)) { message("\nRunways:"); print(w, row.names = FALSE) }
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
    # an empty wrapper is an ANSWER: the credential was accepted and the
    # question matched nothing. In geiloc and rotaer `name` searches the NAME
    # of the place ("cong" finds Congonhas); an ICAO code there matches nothing.
    message("The credential was accepted; the question matched no record. ",
            "Top-level elements: ",
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
# download_aisweb(area, ..., name) -- a list area, every page
#
#   download_aisweb_rotaer()                      # the whole ROTAER
#   download_aisweb_rotaer(uf = "SP", type = "AD")   # filters, as documented
#   download_aisweb_waypoints()
#   download_aisweb_geiloc()                      # AD, HP and HD, in one file
#
# See the header for the stopping rule. Nothing is written when a page fails: a
# table short of its last pages would read as a complete one, and a file left
# from an earlier run is worth more than that.
# =============================================================================
download_aisweb <- function(area, ..., name = area, page_size = AISWEB_PAGE_SIZE,
                            max_pages = 2000L, out_dir = AISWEB_OUT_DIR) {
  all <- aisweb_walk(area, ..., page_size = page_size, max_pages = max_pages)
  if (is.null(all)) return(invisible(NULL))
  .aisweb_write(all, name, out_dir)
}

# The walk itself: every page of one question, as a data.frame, or NULL when a
# page failed or nothing came back. Kept apart from the writing so that several
# questions can be put into ONE file (see download_aisweb_geiloc()).
aisweb_walk <- function(area, ..., page_size = AISWEB_PAGE_SIZE, max_pages = 2000L) {
  message(sprintf("AISWEB %s, asking %d row(s) per page ...", area, page_size))
  pages <- list(); seen <- character(0); start <- 0L; total <- NA_integer_
  for (p in seq_len(max_pages)) {
    r <- aisweb_fetch(area, rowstart = start, rowend = page_size, ...)
    if (!r$ok) {
      message(sprintf("  page %d (rowstart %d) FAILED: %s", p, start, r$error))
      message("  Nothing kept: a table short of its last pages would read as ",
              "a complete one.")
      return(NULL)
    }
    rows <- aisweb_rows(r$doc)
    meta <- aisweb_meta(r$doc)
    if ("total" %in% names(meta))
      total <- suppressWarnings(as.integer(meta[["total"]]))
    message(sprintf("  page %d  rowstart %-6d %4d row(s)  total=%s  %.1fs", p, start,
                    nrow(rows), if (is.na(total)) "?" else total, r$secs))
    if (nrow(rows) == 0) break
    id  <- if ("id" %in% names(rows)) rows$id else do.call(paste, c(rows, sep = "|"))
    new <- !(id %in% seen)
    if (!any(new)) {
      message("  this page repeats rows already held: rowstart is not advancing ",
              "the answer, so this area cannot be walked past its first page.")
      break
    }
    pages[[length(pages) + 1L]] <- rows[new, , drop = FALSE]
    seen  <- c(seen, id[new])
    start <- start + nrow(rows)
    if (!is.na(total) && length(seen) >= total) break
  }
  all <- cgna_rbind_fill(pages)
  if (is.null(all)) { message("  no record."); return(NULL) }

  # the API's own count against what is about to be stored
  if (!is.na(total) && nrow(all) != total)
    message(sprintf("  NOTE: the API reports total=%d and %d row(s) were received.",
                    total, nrow(all)))
  else if (is.na(total))
    message("  The answer carries no total, so this count cannot be checked ",
            "against the API's own.")
  all
}

download_aisweb_rotaer    <- function(...) download_aisweb("rotaer", ...)
download_aisweb_waypoints <- function(...) download_aisweb("waypoints", ...)

# geiloc answers only to a filter, and `type` is the one that covers the table:
# AD aerodrome, HP heliport, HD helideck -- the three types the ROTAER carries.
# Each type is a question of its own, and they all go into ONE file: written
# type by type under the same name, the last would replace the others. If any
# type fails nothing is written, for the same reason a short table is not.
download_aisweb_geiloc <- function(type = c("ad", "hp", "hd"), ...,
                                   out_dir = AISWEB_OUT_DIR) {
  parts <- list()
  for (t in type) {
    d <- aisweb_walk("geiloc", type = t, ...)
    if (is.null(d)) {
      message(sprintf("geiloc type=%s gave nothing; aisweb_geiloc.csv not written.", t))
      return(invisible(NULL))
    }
    parts[[length(parts) + 1L]] <- d
  }
  all <- cgna_rbind_fill(parts)
  if ("id" %in% names(all)) all <- all[!duplicated(all$id), , drop = FALSE]
  .aisweb_write(all, "geiloc", out_dir)
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
#   Rscript AISWEB/download_aisweb.R            # rotaer, waypoints
if (sys.nframe() == 0L) {
  download_aisweb_rotaer()
  download_aisweb_waypoints()
}
