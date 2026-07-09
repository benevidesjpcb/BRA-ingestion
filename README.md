# BRA taxi-time ingestion

Reproducible preparation of the Brazilian taxi-time data used in the Brazil–Europe
operational-efficiency study. The taxi-time metric (GANP 20th-percentile reference
and daily additional time) is computed in this repository with base tidyverse, so
**the private `PBWG` package is not required** — the pipeline runs on any machine.

## Requirements

- **R** (≥ 4.x) and **Quarto**.
- R packages: those loaded in `_chapter-setup.R` (tidyverse, lubridate, data.table,
  arrow, fs, here, vroom, scales, knitr, …). A working `unzip` on `PATH` is only
  needed if you read the source from a zip archive.

## Project layout

| Path | Role | Tracked in git? |
| --- | --- | --- |
| `data-raw/` | **Input** — put the raw `dsTaxiYYYY.csv` files here | no (only `.gitkeep`) |
| `data/` | Generated analytic CSVs + coverage summary (consumed by the report) | no (only `.gitkeep`) |
| `data/apdf/` | Generated harmonised parquet extracts | no |
| `outputs/` | Generated per-year daily outputs | no (only `.gitkeep`) |
| `golden/` | Reference result CSVs used to validate the reproduction | yes |
| `_chapter-setup.R` | Shared libraries, project paths, analysis parameters | yes |
| `Taxi-BRA-ingestion.qmd` | The documented pipeline | yes |
| `reproduce_txxt.R` | Standalone validation of the metric against `golden/` | yes |

The raw CSVs and every generated artefact are git-ignored, so a fresh clone only
carries the code, the setup, and the golden validation data.

## How to run anywhere

1. **Clone** the repository and open the project (`BRA-ingestion.Rproj`).
2. **Provide the source data** — either:
   - copy `dsTaxi2023.csv`, `dsTaxi2024.csv`, `dsTaxi2025.csv` into `data-raw/`, **or**
   - set the environment variable `BRA_TAXI_ZIP` to a zip archive that contains them.
3. **Run the preparation.** The `prepare-bra-taxi-data` chunk is `eval: false`
   (it is not executed on render), so run it manually once. It writes:
   - `data/apdf/PBWG-BRA-dsTaxi-apdf-YYYY.parquet`
   - `data/PBWG-BRA-txxt-analytic-YYYY-ref2024-icao_ganp_p20.csv`
   - `data/BRA-txxt-coverage-summary-2023-2025.csv`
   - `outputs/txxt-daily-YYYY/…`
4. **Render** the document: `quarto render Taxi-BRA-ingestion.qmd`.

> Rendering **before** step 3 does not fail: the inventory and coverage tables show a
> short notice explaining that the source/generated files are missing yet.

## Optional environment variables

| Variable | Purpose | Default |
| --- | --- | --- |
| `BRA_TAXI_ZIP` | Path to a zip archive holding the `dsTaxiYYYY.csv` files | unset → read from `data-raw/` |
| `BRA_REPORT_DATA` | Data directory of the sibling report project that receives the combined analytic CSVs | this project's `data/` |

## Configuration

All paths and analysis parameters live in one place, `_chapter-setup.R`:
`variant`, `ref_year`, `data_years`, `min_n`, `max_txxt`, `p_ref`, `ref_key`, and the
project directories. Change the reference year or variant there and the file names,
regex, and reporting period follow automatically.

## Validating the reproduction

`reproduce_txxt.R` rebuilds the analytic outputs from the raw data and compares them,
row by row, with the CSVs in `golden/`. It is the regression check that proves the
in-repo metric matches the original PBWG results exactly for 2023–2025.
