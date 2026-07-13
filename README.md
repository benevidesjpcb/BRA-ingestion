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
| `data/` | Analytic CSVs + coverage summary — consumed by the report **and the dashboard** | **yes** (the `.csv` files) |
| `data/apdf/` | Generated harmonised parquet extracts | no |
| `outputs/` | Generated per-year daily outputs | no (only `.gitkeep`) |
| `index.html` | Interactive dashboard — reads the CSVs in `data/` live | yes |
| `golden/` | Reference result CSVs used to validate the reproduction | yes |
| `_chapter-setup.R` | Shared libraries, project paths, analysis parameters | yes |
| `Taxi-BRA-ingestion.qmd` | The documented pipeline | yes |
| `reproduce_txxt.R` | Standalone validation of the metric against `golden/` | yes |

The raw `dsTaxi` files and the parquet extracts are git-ignored; the analytic CSVs in
`data/` **are** tracked, because both the report and the dashboard read them.

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

## Dashboard

`index.html` is a self-contained interactive dashboard comparing taxi time between
Brazil and Europe. It has **no build step**: it reads the analytic CSVs in `data/`
live in the browser and discovers the available years and airports on its own.

### Viewing it

The dashboard fetches CSV files, which browsers **block when you double-click the
file** (`file://`). Open it over HTTP instead:

- **Published (public):** enable **GitHub Pages** — repo → *Settings* → *Pages* →
  *Deploy from a branch* → **`main`** / **`/ (root)`**. The public URL will be
  `https://<user>.github.io/<repo>/`.
- **Locally:** run `python3 -m http.server` in the repo folder and open
  `http://localhost:8000/`.

Opening the file directly just shows a short message reminding you of this.

### Updating the data — only add or remove files

The dashboard shows whatever analytic CSVs exist in `data/`, named:

```
PBWG-<REGION>-txxt-analytic-<YEAR>-ref2024-icao_ganp_p20.csv
```

where `<REGION>` is `BRA` or `EUR`. So:

| You want to… | Do this |
| --- | --- |
| **Update Brazil** (e.g. more of 2026) | Re-run the pipeline — it writes the file into `data/`. The dashboard already reads it. |
| **Add Europe 2026** | Drop `PBWG-EUR-txxt-analytic-2026-ref2024-icao_ganp_p20.csv` into `data/`. |
| **Add 2027** | Drop the 2027 file(s) into `data/`; the year button appears by itself. |
| **Remove 2023** | Delete the 2023 file(s) from `data/`. |
| **Add more airports** | Nothing — new ICAO codes in the CSVs appear automatically. Add a label in `CONFIG.names` (in `index.html`) if you want a name instead of the code. |

No code edit is needed for years or airports. Commit the CSVs and `push`; the
published site updates on its own.

> Partial years (e.g. 2026 through June) are detected automatically and flagged as
> "partial". A region with no file for a given year is shown as "no data" instead of
> breaking the comparison.

### Structural changes (rare)

Only these need editing the `CONFIG` block at the top of the script in `index.html`:

- **Reference year / variant** — `refYear` / `variant` (they are part of the file
  names, so rename the CSVs to match).
- **The two regions** — `regions` (codes, labels, colours); it is a two-region
  comparison.
- **Airport display names** — `CONFIG.names`.

## Validating the reproduction

`reproduce_txxt.R` rebuilds the analytic outputs from the raw data and compares them,
row by row, with the CSVs in `golden/`. It is the regression check that proves the
in-repo metric matches the original PBWG results exactly for 2023–2025.
