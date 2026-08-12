#!/usr/bin/env Rscript
# =============================================================================
# ingest_tatic.R
#
# Second step of the TATIC ingestion. The data arrives as JSON (an array of
# movement records, one file per day). This script does NOT download anything:
# it reads whatever TATIC JSON is in data-raw/tatic/, flattens it into a single
# rectangular table, and writes it back alongside, so the rest of the pipeline
# has a stable, tabular starting point.
#
#   source(here::here("API_TATIC", "ingest_tatic.R"))
#   ingest_tatic()
#
# Input : data-raw/tatic/*.json   (one or many files; each a JSON array)
# Output: data-raw/tatic/tatic-movements.csv   (all files combined)
#
# Nothing is discarded — every JSON field becomes a column. The fields the
# taxi-time metric needs are listed in TATIC_KEY_FIELDS and checked for presence,
# so a change in the source layout is noticed here rather than three steps later.
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').")
})

# ---- the fields we care about (for reference / sanity check) ----------------
# Identity + context
TATIC_ID_FIELDS <- c("Callsign", "EventType", "AcftType", "FlightType", "RV",
                     "Adep", "Ades", "Locality", "Runway", "Transponder",
                     "Equipment")
# Event date parts supplied by the source
TATIC_DATE_PARTS <- c("NR_ANO_EVENTO", "NR_MES_EVENTO", "NR_DIA_EVENTO")
# Movement milestones in UTC (the *_BSB twins are Brasilia local time)
TATIC_MILESTONES <- c("EOBT", "wPush", "cPush", "wTaxi", "Taxi",
                      "Hold", "cRwy", "cDep", "Dep", "Arr", "ETA", "cPos")
TATIC_KEY_FIELDS <- c(TATIC_ID_FIELDS, TATIC_DATE_PARTS, TATIC_MILESTONES)

# =============================================================================
# ingest_tatic(raw_dir, out_csv)
#
# Returns, invisibly, the combined data frame.
# =============================================================================
ingest_tatic <- function(raw_dir = here::here("data-raw", "tatic"),
                         out_csv = NULL) {

  if (is.null(out_csv)) out_csv <- file.path(raw_dir, "tatic-movements.csv")

  files <- list.files(raw_dir, pattern = "^tatic-\\d{8}\\.json$", full.names = TRUE)
  if (length(files) == 0)
    files <- list.files(raw_dir, pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0)
    stop("No TATIC JSON files found in ", raw_dir,
         "/. Download them with download_tatic(), or drop the exported *.json there.")

  read_one <- function(f) {
    df <- jsonlite::fromJSON(f, simplifyDataFrame = TRUE, flatten = TRUE)
    if (is.list(df) && !is.data.frame(df)) df <- as.data.frame(df)
    # A day the source has no movements for is a legitimate empty answer, not a
    # failure; it just contributes no rows.
    if (nrow(df) == 0) return(NULL)
    df$SOURCE_FILE <- basename(f)
    df
  }

  parts <- Filter(Negate(is.null), lapply(files, read_one))
  if (length(parts) == 0)
    stop("Every JSON file in ", raw_dir, " is empty.")

  # union of all columns, so files with different shapes still bind
  all_cols <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(df) {
    for (m in setdiff(all_cols, names(df))) df[[m]] <- NA
    df[all_cols]
  })
  movements <- do.call(rbind, parts)

  # ---- de-duplicate across day files ---------------------------------------
  # The API is day-based, but a movement can appear on the UTC/BSB boundary of
  # two adjacent daily pulls. Drop exact duplicate records (ignoring which file
  # they came from), keeping the first occurrence.
  before  <- nrow(movements)
  dup_key <- setdiff(names(movements), "SOURCE_FILE")
  movements <- movements[!duplicated(movements[dup_key]), , drop = FALSE]
  if (before > nrow(movements))
    message(sprintf("Removed %d duplicate record(s) across day files.",
                    before - nrow(movements)))

  # ---- report on the important fields ---------------------------------------
  present <- intersect(TATIC_KEY_FIELDS, names(movements))
  absent  <- setdiff(TATIC_KEY_FIELDS, names(movements))
  if (length(absent) > 0)
    warning("Expected TATIC field(s) not found: ", paste(absent, collapse = ", "),
            "\n  -> the source layout may have changed; check the JSON.",
            call. = FALSE, immediate. = TRUE)

  utils::write.csv(movements, out_csv, row.names = FALSE, na = "")

  message(sprintf(
    "TATIC ingestion: %d file(s), %d record(s), %d column(s) -> %s",
    length(files), nrow(movements), ncol(movements), out_csv))
  message("Key fields present: ", paste(present, collapse = ", "))

  invisible(movements)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) ingest_tatic()
