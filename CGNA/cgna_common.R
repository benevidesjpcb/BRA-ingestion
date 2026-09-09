#!/usr/bin/env Rscript
# =============================================================================
# cgna_common.R
#
# SHARED, NOT A DATASET. What every CGNA/DECEA downloader needs and none of them
# owns: the CSV conventions the raw files are written in, the JSON flattening a
# nested field needs to survive a CSV, and the proxy settings.
#
# This is to the CGNA API what ODIN/download_odin.R is to the ODIN API, with one
# difference: there is no shared engine here, because the CGNA endpoints do not
# share a contract. /apiv1/tatic wants YYYYMMDD dates and returns a bare array;
# /apiv1/voossisceab refuses YYYYMMDD, wants YYYY-MM-DD, and returns
# {"count": N, "data": [...]}. Only the plumbing is common, so only the plumbing
# lives here.
#
# API_TATIC/ carries its own copy of these four helpers and does NOT read this
# file. That duplication is deliberate for now: TATIC is a working pipeline that
# nobody asked to change, and rewriting it to source this would be an untested
# edit to something outside the task that created this file. When TATIC is next
# touched for its own reasons, the four definitions there can be deleted in
# favour of these.
#
#   source(here::here("CGNA", "cgna_common.R"))
#
# They share ONE thing beyond this file: TATIC_TOKEN. The token authenticates a
# person against the portal, not against one endpoint, so every CGNA downloader
# reads that same variable. That is the only reason the name says TATIC, and it
# is not a reason for one dataset's downloader to source another's -- which is
# what this file exists to stop.
# =============================================================================

# Corporate proxy. On a network where everything external goes through one,
# these APIs fail with a 407 on the CONNECT tunnel, surfacing as a transport
# error. See proxy.R.
source(here::here("proxy.R"))

CGNA_SEP <- ";"   # the delimiter every raw file in data-raw/ uses

# ---- JSON arrays -> one string ----------------------------------------------
# A record can carry a nested array (a list of sectors, say). A list column
# cannot be written to CSV, so it is collapsed to a pipe-separated string -- the
# same convention the ODIN downloader uses for its own array columns.
cgna_flatten_lists <- function(df) {
  for (nm in names(df)) {
    if (is.list(df[[nm]]))
      df[[nm]] <- vapply(df[[nm]], function(v) {
        if (is.null(v) || length(v) == 0) NA_character_
        else paste(unlist(v), collapse = "|")
      }, character(1))
  }
  df
}

# ---- part files --------------------------------------------------------------
# Everything is read as character on purpose: a column that looks numeric in one
# month and not in the next would otherwise bind into a mess, and the raw files
# are storage, not the analysis.
cgna_read_csv <- function(path) {
  as.data.frame(data.table::fread(file = path, sep = CGNA_SEP,
                                  colClasses = "character", na.strings = "",
                                  showProgress = FALSE, fill = Inf,
                                  header = TRUE))
}

cgna_write_csv <- function(df, path) {
  data.table::fwrite(df, path, sep = CGNA_SEP, na = "", quote = TRUE)
}

# Bind parts that need not share columns: an endpoint can add a field between
# two months, and rbind() would refuse rather than fill.
cgna_rbind_fill <- function(lst) {
  lst <- Filter(function(d) !is.null(d) && nrow(d) > 0, lst)
  if (length(lst) == 0) return(NULL)
  cols <- unique(unlist(lapply(lst, names)))
  lst  <- lapply(lst, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] <- NA_character_
    d[cols]
  })
  do.call(rbind, lst)
}
