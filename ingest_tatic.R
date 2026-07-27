#!/usr/bin/env Rscript
# =============================================================================
# ingest_tatic.R
#
# First step of the TATIC ingestion. The TATIC data arrives as JSON (an array of
# movement records). This script does NOT download anything: it reads whatever
# TATIC JSON files are dropped into data-raw/tatic/, flattens them into a single
# rectangular table, and writes it back into data-raw/ so the rest of the
# pipeline has a stable, tabular starting point.
#
#   Rscript ingest_tatic.R
#
# Input : data-raw/tatic/*.json   (one or many files; each a JSON array)
# Output: data-raw/tatic/tatic-movements.csv   (all files combined)
#
# Nothing is discarded — every JSON field becomes a column. The columns that
# matter for the taxi-time metric are listed in `key_fields` below and checked
# for presence so we notice early if the source layout changes.
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

raw_dir <- file.path("data-raw", "tatic")
out_csv <- file.path(raw_dir, "tatic-movements.csv")

# ---- the fields we care about (for reference / sanity check) ----------------
# Identity + context
id_fields <- c("Callsign", "EventType", "AcftType", "FlightType", "RV",
               "Adep", "Ades", "Locality", "Runway", "Transponder", "Equipment")
# Event date parts supplied by the source
date_parts <- c("NR_ANO_EVENTO", "NR_MES_EVENTO", "NR_DIA_EVENTO")
# Movement milestones in UTC (the *_BSB twins are Brasilia local time)
milestones <- c("EOBT", "wPush", "cPush", "wTaxi", "Taxi",
                "Hold", "cRwy", "cDep", "Dep", "Arr", "ETA", "cPos")
key_fields <- c(id_fields, date_parts, milestones)

# ---- read every JSON file in data-raw/tatic/ --------------------------------
files <- list.files(raw_dir, pattern = "\\.json$", full.names = TRUE)
if (length(files) == 0) {
  stop("No TATIC JSON files found in ", raw_dir,
       "/. Drop the exported *.json there and re-run.")
}

read_one <- function(f) {
  df <- jsonlite::fromJSON(f, simplifyDataFrame = TRUE, flatten = TRUE)
  if (is.list(df) && !is.data.frame(df)) df <- as.data.frame(df)
  df$SOURCE_FILE <- basename(f)
  df
}

parts <- lapply(files, read_one)

# union of all columns across files, so files with different shapes still bind
all_cols <- unique(unlist(lapply(parts, names)))
parts <- lapply(parts, function(df) {
  missing <- setdiff(all_cols, names(df))
  for (m in missing) df[[m]] <- NA
  df[all_cols]
})
movements <- do.call(rbind, parts)

# ---- de-duplicate across day files ------------------------------------------
# The API is day-based, but a movement can appear on the UTC/BSB boundary of two
# adjacent daily pulls. Drop exact duplicate records (ignoring which file they
# came from), keeping the first occurrence.
before <- nrow(movements)
dup_key <- setdiff(names(movements), "SOURCE_FILE")
movements <- movements[!duplicated(movements[dup_key]), , drop = FALSE]
if (before > nrow(movements))
  message(sprintf("Removed %d duplicate record(s) across day files.",
                  before - nrow(movements)))

# ---- report on the important fields -----------------------------------------
present <- intersect(key_fields, names(movements))
absent  <- setdiff(key_fields, names(movements))
if (length(absent) > 0) {
  warning("Expected TATIC field(s) not found: ", paste(absent, collapse = ", "),
          "\n  -> the source layout may have changed; check the JSON.")
}

# ---- write the combined table -----------------------------------------------
write.csv(movements, out_csv, row.names = FALSE, na = "")

message(sprintf(
  "TATIC ingestion: %d file(s), %d record(s), %d column(s) -> %s",
  length(files), nrow(movements), ncol(movements), out_csv
))
message("Key fields present: ", paste(present, collapse = ", "))
