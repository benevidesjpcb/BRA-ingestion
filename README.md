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
| `index-data.js` | Numbers the dashboard loads, written by `build_taxi_dashboard.R` | no (git-ignored) |
| `golden/` | Reference result CSVs used to validate the reproduction | yes |
| `_chapter-setup.R` | Shared libraries, project paths, analysis parameters | yes |
| `Taxi-BRA-ingestion.qmd` | The documented pipeline | yes |
| `reproduce_txxt.R` | Standalone validation of the metric against `golden/` | yes |

The raw `dsTaxi` files and the parquet extracts are git-ignored; the analytic CSVs in
`data/` **are** tracked, because both the report and the dashboard read them.

## How to run anywhere

1. **Clone** the repository and open the project (`BRA-ingestion.Rproj`).
2. **Provide the source data.** An API for the taxi source is planned but not available
   yet, so for now either:
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

- **taxi-time** — `variant`, `ref_year`, `data_years`, `min_n`, `max_txxt`, `p_ref`,
  `ref_key`;
- **ASMA (KPI08)** — `asma_variant`, `asma_ref_year`, `asma_data_years`, `asma_min_n`,
  `asma_max_asma` (per ring), `asma_p_ref`, `asma_ring_col`, `asma_rings`, `asma_ref_key`;
- the project directories and the generated output file names for both.

Change the study period (`asma_data_years`) or the reference year there and the file
names, regex, download years, and reporting period follow automatically — the `.qmd`
files only source this script.

## Dashboards

There are **two**, one per metric. Each has its own page and its own build script:

| Metric | Page | Build script | Shows |
| --- | --- | --- | --- |
| Taxi time | `index.html` | `build_taxi_dashboard.R` | Brazil × Europe, arrivals and departures |
| ASMA (KPI08) | `kpi08.html` | `KPI08/build_kpi08_dashboard.R` | Brazil only, arrivals, C40 / C100 rings |

Both pages live at the repository root so GitHub Pages can serve them, and both load
their numbers from a generated, git-ignored `*-data.js` beside them.

### Taxi time

`index.html` is a self-contained interactive dashboard comparing taxi time between
Brazil and Europe. It has **no build step**: it reads the analytic CSVs in `data/`
live in the browser and discovers the available years and airports on its own.

### Viewing it locally (no server, no Python)

The page loads its numbers from a small generated file beside it, so:

1. Run once: **`Rscript build_taxi_dashboard.R`** — it reads `data/` and writes
   **`index-data.js`**. (Needs R with the `jsonlite` package: `install.packages("jsonlite")`.)
2. **Double-click `index.html`.** It opens in your browser, offline, no server needed.

Re-run step 1 whenever the CSVs change. If you open `index.html` before ever building
it, it shows a short note telling you to run the script.

`index-data.js` is generated and **git-ignored on purpose**: a build that rewrites the
tracked `index.html` leaves it modified after every run and turns any concurrent change
into a merge conflict on one enormous line. Nothing is lost by not tracking it — served
over HTTP the page reads the CSVs in `data/` live.

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
**`Rscript build_taxi_dashboard.R`** to refresh `index-data.js` for the double-click page.
(A published/served copy reads `data/` live, so it updates on its own once you commit
and `push`.)

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
| `KPI08/download_kpi08.R` | Downloads the `kpi08` table from the ODIN API, one file per year | yes |
| `KPI08/build_kpi08_dashboard.R` | Embeds the analytic numbers into `kpi08.html` | yes |
| `ASMA-BRA-ingestion.qmd` | Documented ASMA ingestion pipeline (arrival KPI08) | yes |
| `kpi08.html` | Interactive ASMA dashboard (C40 / C100) | yes |
| `kpi08-data.js` | Numbers the dashboard loads, written by the build | no (git-ignored) |
| `data-raw/kpi08/` | Raw `kpi08_*.csv` source files (and `parts/` month files) | no (git-ignored) |
| `data/asma/` | Generated harmonised ASMA parquet extracts | no (git-ignored) |

The ASMA parquet follows the APDF convention: **one row per arrival**, with the two ASMA
rings side by side as `C40_`/`C100_` prefixed columns (`CROSS_TIME`, `BEARING`, `SECTOR`,
`SECTOR_GROUP`, `TRANSIT`, `UNIMPEDED`, `KPI08`). Both ring times are therefore readable
from a single row, with no filtering or self-join. The metric itself works on the long
shape internally, because the reference is built per ring.

Both scripts are usable two ways: `Rscript KPI08/<script>.R` from a terminal, or
`source()` + call the function from R — which is how the `.qmd` chunks drive them.

### One download engine, one folder per dataset

Every ODIN table is fetched by the same engine, `ODIN/download_odin.R`, with a thin
wrapper per dataset supplying only what differs:

| Dataset | Wrapper | Table | Date column | Unique key |
| --- | --- | --- | --- | --- |
| KPI08 (ASMA) | `KPI08/download_kpi08.R` | `kpi08` | `aldt` | `id` + `c` (the ASMA ring) |
| TOTALBR | `TOTALBR/download_totalbr.R` | `totalbr` | `dt_dia` | `pk` |

The engine fetches **one month per request window**, saves each month as it arrives so an
interrupted run keeps its progress, judges a month downloaded by the **days it contains**
(a month fetched while still open is partial and is fetched again once it closes),
advances pagination by the **rows actually returned** rather than the requested limit, and
collapses JSON array columns to pipe-separated strings so they survive the CSV.

Earlier years already held as files go straight into the dataset's `data-raw/` folder with
the same naming (`<table>_<year>.csv`); the downloader inspects what each covers and only
fetches what is missing.

> TOTALBR's unique key is `pk`, not `id`: `id` is the callsign plus the airport pair, so it
> repeats on every flight of that route. De-duplicating on it would delete almost
> everything.

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
Rscript KPI08/download_kpi08.R

# a specific year, or several (a full year is downloaded the first time)
Rscript KPI08/download_kpi08.R 2026
Rscript KPI08/download_kpi08.R 2023 2024 2025
```

From R — or from the `download-bra-kpi08-data` chunk in `ASMA-BRA-ingestion.qmd` —
source the script and call the function instead. Sourcing only defines it; nothing is
downloaded until you call it:

```r
source(here::here("KPI08", "download_kpi08.R"))
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

### The KPI08 dashboard

`kpi08.html` is the ASMA counterpart of `index.html`, built the same way: a
self-contained page with no build step beyond embedding the numbers.

```bash
Rscript KPI08/build_kpi08_dashboard.R   # reads data/, embeds into kpi08.html
```

or, from R (this is what the `build-bra-kpi08-dashboard` chunk of the `.qmd` runs):

```r
source(here::here("KPI08", "build_kpi08_dashboard.R"))
build_kpi08_dashboard(ring = asma_ring)
```

It writes **`kpi08-data.js`** next to the page — the page itself is never rewritten.
Then double-click `kpi08.html`. Re-run the build whenever the ASMA analytic CSV changes.

`kpi08-data.js` is generated and **git-ignored on purpose**: a build that rewrites a
tracked HTML file turns every rebuild into a merge conflict on one enormous line. Nothing
is lost by not tracking it — served over HTTP the page reads the analytic CSV in `data/`
live, so a published dashboard still shows data without it.

It shows, for the selected year and metric (additional time / ASMA transit /
reference): headline tiles, additional time by year, an airport ranking, the
monthly trend, and a **validation panel** comparing the reference recomputed in this
repository against the `kpi08` value supplied by Brazil — close bars mean the
reproduction matches. Unlike the taxi dashboard this one is Brazil-only and
arrivals-only, because ASMA has no European counterpart in `data/` and the source
carries no departure (DSMA) side.

### The documented ASMA pipeline

`ASMA-BRA-ingestion.qmd` is the arrival-side companion to `Taxi-BRA-ingestion.qmd`. It
reads the raw `kpi08_*.csv` files from `data-raw/kpi08/` (or a zip via `BRA_ASMA_ZIP`),
harmonises them to APDF-like ARR fields, recomputes the 2024 GANP p20 reference in-repo,
and writes the daily analytic ASMA table plus coverage summaries to `data/` — keeping the
Brazil-supplied `kpi08`/`transito`/`desimp` alongside for validation. Like the taxi
pipeline, its `prepare-bra-asma-data` chunk is `eval: false` and is run once manually.

### Validating the ASMA reproduction

`KPI08/validate_asma_golden.R` checks the output against the reference file used in the
official report, stored in `golden/`, group by group on the full reference key:

```r
source(here::here("KPI08", "validate_asma_golden.R"))
validate_asma_golden()
```

**2024 reproduces the official figures exactly** (1,152,288 valid movements, 3,100,634
minutes of additional time); 326,423 of 326,441 shared 2024–2025 groups match on every
column. The residual 18 groups are all in late 2025 and reflect the reference file and the
ODIN API being different vintages of the same data, not a difference in calculation.

> Some source conventions still need confirmation with DECEA/ICEA (the `feb`/`fev` monthly
> duplicate, and that these files are arrival-only so a separate DSMA source is needed for
> departures). These are listed at the end of the `.qmd`.
