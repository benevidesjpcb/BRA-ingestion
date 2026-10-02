#!/usr/bin/env Rscript
# =============================================================================
# read_aixm.R
#
# What is inside the AIXM package the AISWEB publishes (download_aisweb_aixm()).
#
#   source(here::here("AISWEB", "read_aixm.R"))
#   aixm_files()             # the packages on disk, and the XML each one holds
#   aixm_inventory()         # features by type, in the newest package
#
# THE PACKAGE. One zip per amendment, holding an AIXM 5.1 BasicMessage: a flat
# sequence of <message:hasMember>, each carrying ONE feature -- an aerodrome, a
# runway, an aircraft stand, a designated point, an airspace, a route segment.
# "Completo" holds two files, BL_.xml and BL__decoded.xml, ~335 MB each.
#
# READ WITHOUT UNPACKING, AND WITHOUT PARSING. At 335 MB the file does not fit
# an XML parser's tree in an ordinary session, and the inventory does not need
# one: the type of a feature is the name of the element that follows hasMember.
# The file is streamed out of the zip a block of lines at a time and that name
# is all that is read.
# =============================================================================

AIXM_DIR <- here::here("data-raw", "aisweb", "aixm")

# the packages on disk, newest first, with the XML members of each
aixm_files <- function(dir = AIXM_DIR) {
  z <- list.files(dir, pattern = "\\.zip$", full.names = TRUE)
  if (length(z) == 0) {
    message("No AIXM package in ", dir, ". Run download_aisweb_aixm()."); return(invisible(NULL))
  }
  z <- z[order(file.mtime(z), decreasing = TRUE)]
  do.call(rbind, lapply(z, function(f) {
    m <- utils::unzip(f, list = TRUE)
    data.frame(PACKAGE = basename(f), MEMBER = m$Name,
               SIZE_MB = round(m$Length / 1024^2, 1), stringsAsFactors = FALSE)
  }))
}

# =============================================================================
# aixm_inventory(zip, member) -- features by type
#
# One row per feature type, most numerous first. `member` defaults to BL_.xml,
# the first XML in the package.
# =============================================================================
aixm_inventory <- function(zip = NULL, member = NULL, block = 200000L, quiet = FALSE) {
  if (is.null(zip)) {
    z <- list.files(AIXM_DIR, pattern = "\\.zip$", full.names = TRUE)
    if (length(z) == 0) stop("No AIXM package in ", AIXM_DIR,
                             ". Run download_aisweb_aixm().", call. = FALSE)
    zip <- z[which.max(file.mtime(z))]
  }
  if (is.null(member)) member <- utils::unzip(zip, list = TRUE)$Name[1]
  if (!quiet) message("Reading ", member, " in ", basename(zip), " ...")

  con <- unz(zip, member, open = "rb"); on.exit(close(con))
  counts <- integer(0); pending <- FALSE; lines_read <- 0
  feature <- function(x) sub("^.*?<aixm:([A-Za-z0-9_]+).*$", "\\1", x, perl = TRUE)
  repeat {
    ln <- readLines(con, n = block, warn = FALSE, skipNul = TRUE)
    if (length(ln) == 0) break
    lines_read <- lines_read + length(ln)
    hm <- grepl("<message:hasMember", ln, fixed = TRUE)
    # the feature opens on the hasMember line itself or on the line after it;
    # `pending` carries a hasMember that was the last line of the block before
    same <- hm & grepl("<message:hasMember[^>]*>\\s*<aixm:", ln, perl = TRUE)
    nxt  <- c(pending, hm[-length(hm)] & !same[-length(hm)])
    pending <- hm[length(hm)] && !same[length(hm)]
    got <- c(feature(sub("^.*<message:hasMember[^>]*>", "", ln[same], perl = TRUE)),
             feature(ln[nxt & grepl("<aixm:", ln, fixed = TRUE)]))
    if (length(got)) {
      t <- table(got)
      nm <- union(names(counts), names(t))
      new <- stats::setNames(integer(length(nm)), nm)
      new[names(counts)] <- counts
      new[names(t)] <- new[names(t)] + as.integer(t)
      counts <- new
    }
  }
  out <- data.frame(FEATURE = names(counts), N = as.integer(counts), stringsAsFactors = FALSE)
  out <- out[order(-out$N, out$FEATURE), ]
  rownames(out) <- NULL
  if (!quiet) message(sprintf("%d feature(s) of %d type(s), %s lines.",
                              sum(out$N), nrow(out), format(lines_read, big.mark = ",")))
  out
}
