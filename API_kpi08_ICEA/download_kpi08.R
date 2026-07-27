#!/usr/bin/env Rscript
# =============================================================================
# download_kpi08.R
#
# Downloads KPI08 (ASMA — additional time in terminal airspace) data from the
# ICEA/DECEA ODIN API and saves it into this folder (API_kpi08_ICEA/).
#
# ODIN is a PostgREST API. Query shape:  <base>/<table>?<column>=<op>.<value>
#   operators: eq neq gt gte lt lte like ilike in is   (e.g. addestino=eq.SBGR)
# Results are paginated with `limit` / `offset`; this script walks all pages.
#
#   Rscript API_kpi08_ICEA/download_kpi08.R
#   Rscript API_kpi08_ICEA/download_kpi08.R "addestino=eq.SBGR"        # filter
#   Rscript API_kpi08_ICEA/download_kpi08.R "dt=gte.2026-01-01" "dt=lt.2026-02-01"
#
# Each extra argument is a raw PostgREST filter "<column>=<op>.<value>" and they
# are AND-ed together. No token is required by the example endpoint; if ODIN ever
# needs one, set ODIN_TOKEN in the environment (sent as a Bearer header).
#
# Output: API_kpi08_ICEA/kpi08.json   (all rows, raw)
#         API_kpi08_ICEA/kpi08.csv    (same, flattened to a table)
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. install.packages('httr2').")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required. install.packages('jsonlite').")
})

# ---- configuration ----------------------------------------------------------
base_url  <- Sys.getenv("ODIN_KPI08_URL",
                        unset = "https://odin-ms.icea.decea.mil.br/api/kpi08")
token     <- Sys.getenv("ODIN_TOKEN", unset = "")           # optional
page_size <- as.integer(Sys.getenv("ODIN_PAGE_SIZE", unset = "1000"))

# this script lives in API_kpi08_ICEA/; write outputs next to it
out_dir  <- if (dir.exists("API_kpi08_ICEA")) "API_kpi08_ICEA" else "."
out_json <- file.path(out_dir, "kpi08.json")
out_csv  <- file.path(out_dir, "kpi08.csv")

# extra CLI args are PostgREST filters "<column>=<op>.<value>"
filters <- commandArgs(trailingOnly = TRUE)
filter_list <- list()
for (f in filters) {
  kv <- strsplit(f, "=", fixed = TRUE)[[1]]
  if (length(kv) < 2) stop("Bad filter '", f, "'. Use column=op.value, e.g. dt=gte.2026-01-01")
  filter_list[[kv[1]]] <- paste(kv[-1], collapse = "=")
}
if (length(filter_list) > 0)
  message("Filters: ", paste(names(filter_list), unlist(filter_list), sep = "=", collapse = "  "))

# ---- one page ---------------------------------------------------------------
fetch_page <- function(offset) {
  req <- httr2::request(base_url) |>
    httr2::req_url_query(!!!filter_list, limit = page_size, offset = offset) |>
    httr2::req_user_agent("BRA-ingestion/kpi08") |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 4)
  if (nzchar(token))
    req <- httr2::req_headers(req, Authorization = paste("Bearer", token))

  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) != 200)
    stop("ODIN returned HTTP ", httr2::resp_status(resp), " at offset ", offset)

  body <- httr2::resp_body_string(resp)
  if (!jsonlite::validate(body))
    stop("ODIN response is not valid JSON (first 200 chars):\n", substr(body, 1, 200))
  jsonlite::fromJSON(body, simplifyVector = FALSE)
}

# ---- walk all pages ---------------------------------------------------------
message("Downloading kpi08 from ", base_url, " (page size ", page_size, ") ...")
rows   <- list()
offset <- 0L
repeat {
  page <- fetch_page(offset)
  rows <- c(rows, page)
  message(sprintf("  offset %-7d +%d rows  (total %d)", offset, length(page), length(rows)))
  if (length(page) < page_size) break     # last page reached
  offset <- offset + page_size
}

if (length(rows) == 0) {
  message("No rows returned. Check the filters or the endpoint.")
  quit(save = "no", status = 0)
}

# ---- save raw JSON + flattened CSV ------------------------------------------
jsonlite::write_json(rows, out_json, auto_unbox = TRUE, null = "null")

df <- jsonlite::fromJSON(jsonlite::toJSON(rows, auto_unbox = TRUE, null = "null"),
                         flatten = TRUE)
utils::write.csv(df, out_csv, row.names = FALSE, na = "")

message(sprintf("Done: %d row(s), %d column(s).", nrow(df), ncol(df)))
message("Columns: ", paste(names(df), collapse = ", "))
message("Saved -> ", out_json, "  and  ", out_csv)
