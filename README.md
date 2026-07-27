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

### Viewing it locally (no server, no Python)

The page carries its data embedded, so:

1. Run once: **`Rscript build_dashboard.R`** — it reads `data/` and embeds the numbers
   into `index.html`. (Needs R with the `jsonlite` package: `install.packages("jsonlite")`.)
2. **Double-click `index.html`.** It opens in your browser, offline, no server needed.

Re-run step 1 whenever the CSVs change. If you open `index.html` before ever building
it, it shows a short note telling you to run the script.

### Publishing it (later)

When the page is *served over HTTP* it can also read the CSVs in `data/` live, so no
rebuild is needed there. To publish: enable **GitHub Pages** — repo → *Settings* →
*Pages* → *Deploy from a branch* → **`main`** / **`/ (root)`**; the URL will be
`https://<user>.github.io/<repo>/`. (Or serve locally with
`Rscript -e 'servr::httd()'` if you prefer the live-reading mode.)

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

No code edit is needed for years or airports. After changing files in `data/`, run
**`Rscript build_dashboard.R`** to refresh the double-click page. (A published/served
copy also updates on its own once you commit and `push`.)

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

## KPI08 — ASMA (additional time in terminal airspace)

KPI08 uses a **different** data source from the taxi-time pipeline above: the
**ODIN** API from ICEA/DECEA, a PostgREST service. Its downloader lives in its own
folder so it stays independent of the `dsTaxi`/TATIC ingestion.

| Path | Role | Tracked in git? |
| --- | --- | --- |
| `API_kpi08_ICEA/download_kpi08.R` | Downloads the `kpi08` table from the ODIN API, one file per year | yes |
| `ASMA-BRA-ingestion.qmd` | Documented ASMA ingestion pipeline (arrival KPI08) | yes |
| `data-raw/kpi08/` | Raw `kpi08_*.csv` source files | no (git-ignored) |
| `data/asma/` | Generated harmonised ASMA parquet extracts | no (git-ignored) |

### The ODIN API (PostgREST)

Base endpoint: `https://odin-ms.icea.decea.mil.br/api/kpi08`

Queries follow the PostgREST shape `<base>/<table>?<column>=<operator>.<value>`:

| Operator | Meaning | Example |
| --- | --- | --- |
| `eq` / `neq` | equal / not equal | `addestino=eq.SBGR` |
| `gt` `gte` `lt` `lte` | comparisons | `valor=gte.1000` |
| `like` / `ilike` | SQL LIKE (`%` wildcard) | `nome=ilike.jo%25` |
| `in` | in a list | `addestino=in.(SBGR,SBSP,SBKP)` |
| `is` | null / true / false | `valor=is.null` |

Results are paginated with `limit` / `offset` (the `limit=100` in the example is
just a cap); the script walks every page automatically.

### Running it — one file per year, incrementally

The download is **per year** and **resumable**: each year lands in
`data-raw/kpi08/kpi08_<year>.csv` (the folder is created on first run). If the year
file already exists, the script reads the latest date it already holds and asks ODIN
only for records from that point on, then merges them in and de-duplicates by `id`.
So the current year can be refreshed daily or monthly without re-downloading it.

```bash
# current year only — the usual daily/monthly refresh
Rscript API_kpi08_ICEA/download_kpi08.R

# a specific year, or several (a full year is downloaded the first time)
Rscript API_kpi08_ICEA/download_kpi08.R 2026
Rscript API_kpi08_ICEA/download_kpi08.R 2023 2024 2025
```

From R — or from the `download-bra-kpi08-data` chunk in `ASMA-BRA-ingestion.qmd` —
source the script and call the function instead. Sourcing only defines it; nothing is
downloaded until you call it:

```r
source(here::here("API_kpi08_ICEA", "download_kpi08.R"))
download_kpi08(2023:2026, out_dir = here::here("data-raw", "kpi08"))
```

Both bounds of the year window are pushed to the server (`and=(col.gte.…,col.lt.…)`),
so only the requested year travels over the wire. Files are written
semicolon-delimited, matching the annual archive layout that `ASMA-BRA-ingestion.qmd`
reads.

| Variable | Purpose | Default |
| --- | --- | --- |
| `ODIN_DATE_COL` | Date column used to filter by year and to resume | `aldt` |
| `ODIN_ID_COL` | Unique id column used to de-duplicate on merge | `id` |
| `ODIN_PAGE_SIZE` | Rows per request | `1000` |
| `ODIN_KPI08_URL` | API endpoint | the `kpi08` URL above |
| `ODIN_TOKEN` | Bearer token, if ODIN ever requires one | unset (not needed) |

> `ODIN_DATE_COL` defaults to `aldt` (landing time). Confirm it matches the `kpi08`
> table; if the date column has another name, set the variable rather than editing
> the script.

### The documented ASMA pipeline

`ASMA-BRA-ingestion.qmd` is the arrival-side companion to `Taxi-BRA-ingestion.qmd`. It
reads the raw `kpi08_*.csv` files from `data-raw/kpi08/` (or a zip via `BRA_ASMA_ZIP`),
harmonises them to APDF-like ARR fields, recomputes the 2024 GANP p20 reference in-repo,
and writes the daily analytic ASMA table plus coverage summaries to `data/` — keeping the
Brazil-supplied `kpi08`/`transito`/`desimp` alongside for validation. Like the taxi
pipeline, its `prepare-bra-asma-data` chunk is `eval: false` and is run once manually.

> A few source conventions still need confirmation with DECEA/ICEA before the output is
> final (what `c_time`/`bear` represent — C40 vs C100; the `feb`/`fev` monthly duplicate;
> and that these files are arrival-only, so a separate DSMA source is needed for
> departures). These are listed at the end of the `.qmd`.
