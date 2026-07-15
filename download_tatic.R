#!/usr/bin/env Rscript
# =============================================================================
# download_tatic.R
#
# Downloads TATIC movement data from the CGNA/DECEA API and drops the raw JSON
# into data-raw/tatic/, then flattens it to CSV (via ingest_tatic.R).
#
#   Rscript download_tatic.R                # last 30 days (default)
#   Rscript download_tatic.R 20250101 20250201   # explicit datai dataf (YYYYMMDD)
#
# The token is NEVER written in this file. Provide it via an environment
# variable before running:
#   export TATIC_TOKEN="your-token-here"
#
# API (GET): https://portal.cgna.decea.mil.br/apiv1/tatic?token=...&datai=YYYYMMDD&dataf=YYYYMMDD
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. Install it with install.packages('httr2').")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').")
})

# ---- configuration ----------------------------------------------------------
base_url <- Sys.getenv("TATIC_URL", unset = "https://portal.cgna.decea.mil.br/apiv1/tatic")
token    <- Sys.getenv("TATIC_TOKEN", unset = "")
if (!nzchar(token)) {
  stop("TATIC_TOKEN is not set. Run:  export TATIC_TOKEN=\"your-token\"  before this script.")
}

raw_dir <- file.path("data-raw", "tatic")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# ---- date window: default = last 30 days, or take it from the CLI -----------
fmt <- function(d) format(as.Date(d), "%Y%m%d")   # API wants YYYYMMDD
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  datai <- fmt(as.Date(args[1], format = "%Y%m%d"))
  dataf <- fmt(as.Date(args[2], format = "%Y%m%d"))
} else {
  today <- Sys.Date()
  datai <- fmt(today - 30)   # inferred: 30 days back
  dataf <- fmt(today)
}
message(sprintf("TATIC window: datai=%s  dataf=%s", datai, dataf))

# ---- call the API -----------------------------------------------------------
resp <- httr2::request(base_url) |>
  httr2::req_url_query(token = token, datai = datai, dataf = dataf) |>
  httr2::req_user_agent("BRA-ingestion/tatic") |>
  httr2::req_retry(max_tries = 4) |>            # backoff on transient failures
  httr2::req_perform()

status <- httr2::resp_status(resp)
if (status != 200) {
  stop("TATIC API returned HTTP ", status, ". Check the token and the date window.")
}

body <- httr2::resp_body_string(resp)
if (!jsonlite::validate(body)) {
  stop("TATIC API response is not valid JSON. First 200 chars:\n",
       substr(body, 1, 200))
}

# ---- save the raw JSON into data-raw/tatic/ ---------------------------------
out_json <- file.path(raw_dir, sprintf("tatic-%s-%s.json", datai, dataf))
writeLines(body, out_json)

n <- tryCatch(length(jsonlite::fromJSON(body, simplifyVector = FALSE)),
              error = function(e) NA_integer_)
message(sprintf("Saved %s record(s) -> %s",
                ifelse(is.na(n), "?", n), out_json))

# ---- flatten everything in data-raw/tatic/ to CSV ---------------------------
if (file.exists("ingest_tatic.R")) {
  message("Flattening to CSV via ingest_tatic.R ...")
  source("ingest_tatic.R")
} else {
  message("Raw JSON saved. Run  Rscript ingest_tatic.R  to build the CSV.")
}
