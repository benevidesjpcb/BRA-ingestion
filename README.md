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
| `data-raw/dstaxi/` | **Input** — the raw `dsTaxiYYYY.csv` files (and `parts/` month files) | no (git-ignored) |
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
2. **Provide the source data**, in any of three ways:
   - download it: `source(here::here("TAXI", "download_taxi.R")); download_taxi(dsTaxi_years)`
     — one file per year into `data-raw/dstaxi/`, resuming whatever is already there. Nothing to
     configure: the table, the date column and the column renaming are already set, **or**
   - copy `dsTaxi2023.csv`, `dsTaxi2024.csv`, `dsTaxi2025.csv` into `data-raw/dstaxi/`, **or**
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
| `BRA_TAXI_ZIP` | Path to a zip archive holding the `dsTaxiYYYY.csv` files | unset → read from `data-raw/dstaxi/` |
| `TAXI_TABLE` | The ODIN table holding the taxi source | `dstaxi` |
| `TAXI_DATE_COL` | The column the month windows filter on — the movement stamp, so both arrivals and departures fall in the month they are reported in | `dhbimtra` |
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
| Taxi time | `TAXI/download_taxi.R` | `dstaxi` | `dhbimtra` | none — see below |
| TOTALBR | `TOTALBR/download_totalbr.R` | `total_brasil` | `dt_dia` | `pk` |

The engine fetches **one month per request window**, saves each month as it arrives so an
interrupted run keeps its progress, judges a month downloaded by the **days it contains**
(a month fetched while still open is partial and is fetched again once it closes),
advances pagination by the **rows actually returned** rather than the requested limit, and
collapses JSON array columns to pipe-separated strings so they survive the CSV.

Every dataset has its own folder under `data-raw/`, with the months in a `parts/`
sub-folder: `data-raw/kpi08/`, `data-raw/dstaxi/`, `data-raw/totalbr/`. Both are created on
the first run, so there is nothing to prepare by hand.

Earlier years already held as files go straight into the dataset's folder with
the same naming (`<prefix>_<year>.csv`, e.g. `data-raw/totalbr/totalbr_2019.csv`); the
downloader inspects what each covers and only fetches what is missing. The endpoint table
and the local file prefix are separate, so TOTALBR reads `total_brasil` but writes the
shorter `totalbr_<year>.csv`.

> TOTALBR's unique key is `pk`, not `id`: `id` is the callsign plus the airport pair, so it
> repeats on every flight of that route. De-duplicating on it would delete almost
> everything.

> `dstaxi` has no unique key at all. Its wrapper therefore passes `id_col = NULL` — nothing
> is de-duplicated on merge — and gives the engine an explicit `order_cols`
> (`dhbimtra`, `indicativo`, `mov`, `dhvra`) so offset pagination still runs over a total
> order. It also passes `rename_cols`, because the established files use `dh_bimtra`,
> `dh_vra` and `match_vra` while the API returns `dhbimtra`, `dhvra` and `matchvra`; the
> rename happens on arrival, so a downloaded year reads exactly like one from the zip.

`TOTALBR-BRA-ingestion.qmd` documents that dataset end to end, the same way
`Taxi-BRA-ingestion.qmd` and `ASMA-BRA-ingestion.qmd` do their own: probe the endpoint,
download, verify what arrived (year completeness, duplicate keys, month-by-month coverage)
and profile the traffic. TOTALBR is the national movement table — one row per flight
anywhere in Brazil — so unlike the other two it carries no metric, no reference year and no
percentile; it is the volume/denominator dataset.

| Path | Role | Tracked in git? |
| --- | --- | --- |
| `TAXI/download_taxi.R` | Downloads the taxi source from the ODIN API into `data-raw/dstaxi/dsTaxiYYYY.csv` | yes |
| `TOTALBR/download_totalbr.R` | Downloads the `total_brasil` table, one file per year | yes |
| `TOTALBR/totalbr_sources.R` | Reads the parquet archive and the CSVs as one dataset; day counts, coverage, missing years | yes |
| `TOTALBR/check_totalbr_duplicates.R` | Measures duplication, and pulls the offending rows | yes |
| `TOTALBR/compare_totalbr_sources.R` | Parquet archive vs API download: what matches, what is one-sided, where they disagree | yes |
| `TOTALBR-BRA-ingestion.qmd` | Documented TOTALBR ingestion pipeline | yes |
| `data-raw/totalbr/` | The parquet archive plus raw `totalbr_*.csv` (and `parts/` month files) | no (git-ignored) |

TOTALBR has two sources and both count: a **parquet archive** holding the history (all
airports, up to 2025), and the **ODIN API** for the rest. `totalbr_sources.R` reads them as
one dataset, so `totalbr_missing_years()` asks the API only for what the archive lacks. Set
`BRA_TOTALBR_PARQUET` if the archive is kept outside the repository — it is around 1 GB.

> The API holds only a token sample of the early years: a 2019 download returns a few dozen
> rows in total. That is the source answering truthfully, not a broken filter. Judge
> coverage by the day counts in the qmd, never by the fact that a download ran.

ICEA/DECEA have reported duplication in the ODIN data. `check_totalbr_duplicates()`
measures it under a strict, operational definition: a repeated `pk` (the row hash — the
same row twice), or the **same registration at the same aerodrome pair with `dh_inicio` or
`dh_fim` within a few minutes**, since one airframe cannot be in one place twice at once.
Rows merely sharing a callsign, a route and a day are two flights, not a duplicate. It reads
the per-month parts for the `pk` test, because the merge has already de-duplicated the year
file, and pools them so a `pk` repeated across two months is not missed. It reports; it
never repairs, because which copy to keep is DECEA's decision.

> Pagination fix that came out of this: the downloader ordered by the date column alone, and
> thousands of rows share a timestamp. Offset pagination over a non-total order can return a
> row on two pages or on none. The order now includes the unique id, so the pages partition
> the window. Parts downloaded before this may carry duplicates of our own making.

`odin_tables()` lists everything the API exposes, for when a new dataset appears, and
`odin_count(table, date_col, from, to)` asks how many rows a window HAS without downloading
them — the cheapest way to tell a download gap from a source gap.

## TATIC (CGNA API)

TATIC is a different API and a different shape. It is the CGNA movement feed, and it carries
the **milestones** of a movement — EOBT, push, taxi, holding, runway, departure, arrival —
where the taxi source carries only the movement and block times.

| Path | Role | Tracked in git? |
| --- | --- | --- |
| `API_TATIC/download_tatic.R` | Downloads TATIC day by day into monthly parts, merged per year | yes |
| `API_TATIC/ingest_tatic.R` | Flattens TATIC `*.json` exports dropped in by hand | yes |
| `TATIC-BRA-ingestion.qmd` | Documented TATIC ingestion pipeline | yes |
| `data-raw/tatic/` | `tatic_<year>.csv` and `parts/tatic_<year>-<month>.csv` | no (git-ignored) |

> **One day per call.** A wide window is ignored: `datai=20250101&dataf=20250130` returns
> only the first day, with no error. The downloader therefore walks the period one day at a
> time and accumulates the days into the month file — the day is the unit of fetching, the
> month the unit of storage. Requests are serial by default because the API queues
> concurrent ones until they time out.

The day is recorded in an added column, `TATIC_DAY`: the day *requested*, not a date read
from the record, because the record's own date fields describe the event and can fall on a
different day. A day the source has no records for is stored as a placeholder row, so it is
not requested again on every run, and the placeholders are dropped when the year is merged.

`TATIC_TOKEN` goes in `.Renviron` (git-ignored) — never in a script, never in the
repository.

Before a first download of a table, check the endpoint with a single small request instead
of discovering a wrong column name hours in:

```r
source(here::here("ODIN", "download_odin.R"))
odin_probe("total_brasil")   # prints the columns and flags the date-like ones
```

Every column the engine keys on is overridable per dataset without touching code —
`TOTALBR_DATE_COL`, `TOTALBR_ID_COL`, `TOTALBR_DEDUP_COL` (and `ODIN_DATE_COL`,
`ODIN_ID_COL`, `ODIN_RING_COL` for KPI08) — so if the probe disagrees with the table
above, set the variable rather than editing the wrapper.

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
