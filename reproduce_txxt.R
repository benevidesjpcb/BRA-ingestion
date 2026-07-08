#!/usr/bin/env Rscript
# =============================================================================
# reproduce_txxt.R  (v3)
#
# Reproduz os analiticos diarios de taxi-time (PBWG-BRA-txxt-analytic-*) SEM o
# pacote PBWG, e VALIDA contra os CSVs de resultado (golden).
#
# ESTADO: MVTS_VALID, MVTS_NA e TOT_TXXT ja batem EXATO nos 3 anos.
# Falta so TOT_REF (~90%), com erro minusculo. v3 testa o recorte do periodo
# de referencia (o codigo original limita a ref a [ref_start, ref_end] de 2024)
# e imprime um diagnostico do que sobrar.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(data.table)
})

# ---- CONFIG -----------------------------------------------------------------
raw_dir <- "data-raw"
golden_paths <- c(
  "2023" = "golden/PBWG-BRA-txxt-analytic-2023-ref2024-icao_ganp_p20.csv",
  "2024" = "golden/PBWG-BRA-txxt-analytic-2024-ref2024-icao_ganp_p20.csv",
  "2025" = "golden/PBWG-BRA-txxt-analytic-2025-ref2024-icao_ganp_p20.csv"
)
study_airports <- c("SBGR","SBGL","SBRJ","SBCF","SBBR","SBSV","SBKP","SBSP",
                    "SBCT","SBPA","SBRF","SBEG")
ref_year   <- 2024L
data_years <- 2023:2025
tol        <- 1e-6

# ---- LOCKED (tudo confirmado) -----------------------------------------------
max_txxt <- 120
min_n    <- 5L
p_ref    <- 0.20
qtype    <- 7L
ref_key  <- c("ICAO", "PHASE", "STND", "RWY")

# ---- helpers ----------------------------------------------------------------
normalise_text <- function(x) {
  x <- as.character(x); x <- trimws(x); x[x == ""] <- NA_character_; x
}
parse_dt <- function(x) {
  parse_date_time(normalise_text(x), orders = c("ymd HMS", "ymd HMS OS"),
                  tz = "UTC", quiet = TRUE)
}
read_harmonised <- function(year) {
  f <- file.path(raw_dir, sprintf("dsTaxi%d.csv", year))
  raw <- fread(f, sep = ";", fill = 15, colClasses = "character",
               showProgress = FALSE) |> as_tibble()
  if (!"V15" %in% names(raw)) raw$V15 <- NA_character_
  raw |>
    transmute(
      PHASE = recode(normalise_text(mov), Arr = "ARR", Dep = "DEP",
                     .default = NA_character_),
      MVT_TIME   = parse_dt(dh_bimtra),
      BLOCK_TIME = parse_dt(dh_vra),
      STND = normalise_text(box),
      RWY  = normalise_text(pista),
      ADEP = normalise_text(adpartida),
      ADES = normalise_text(addestino),
      ICAO = case_when(normalise_text(mov) == "Arr" ~ normalise_text(addestino),
                       normalise_text(mov) == "Dep" ~ normalise_text(adpartida),
                       TRUE ~ NA_character_)
    ) |>
    filter(ICAO %in% study_airports, !is.na(PHASE))
}

txxt_fn <- function(d) if_else(
  d$PHASE == "ARR",
  as.numeric(difftime(d$BLOCK_TIME, d$MVT_TIME, units = "mins")),
  as.numeric(difftime(d$MVT_TIME, d$BLOCK_TIME, units = "mins"))
)

prep <- function(df) {
  df |>
    mutate(
      TXXT = txxt_fn(pick(everything())),
      DATE = as_date(coalesce(BLOCK_TIME, MVT_TIME)),   # confirmado
      VALID_TXXT = !is.na(TXXT) & TXXT > 0 & TXXT <= max_txxt
    )
}

# recorte do universo da referencia (2024) -- eixo em teste
ref_start <- ymd_hms(sprintf("%d-01-01 00:00:00", ref_year), tz = "UTC")
ref_end   <- ymd_hms(sprintf("%d-12-31 23:59:59", ref_year), tz = "UTC")
ref_filters <- list(
  "none"          = function(d) rep(TRUE, nrow(d)),
  "date_in_year"  = function(d) year(d$DATE) == ref_year,
  "mvt_in_year"   = function(d) d$MVT_TIME >= ref_start & d$MVT_TIME <= ref_end,
  "block_in_year" = function(d) d$BLOCK_TIME >= ref_start & d$BLOCK_TIME <= ref_end
)

build_reference <- function(mov_ref, ref_filter) {
  ref_src <- mov_ref |>
    filter(VALID_TXXT, if_all(all_of(ref_key), ~ !is.na(.x)))
  keep <- ref_filters[[ref_filter]](ref_src)
  keep[is.na(keep)] <- FALSE
  ref_src |>
    filter(keep) |>
    group_by(across(all_of(ref_key))) |>
    summarise(N = n(),
              TXXT_REF = quantile(TXXT, p_ref, type = qtype, names = FALSE),
              .groups = "drop") |>
    filter(N >= min_n) |>
    select(all_of(ref_key), TXXT_REF)
}

build_daily <- function(ref_tbl, mov_all) {
  mov_all |>
    filter(!is.na(DATE)) |>
    left_join(ref_tbl, by = ref_key) |>
    mutate(VALID = VALID_TXXT & !is.na(TXXT_REF)) |>
    group_by(ICAO, PHASE, DATE) |>
    summarise(
      MVTS_VALID   = sum(VALID),
      MVTS_NA      = sum(!VALID),
      TOT_TXXT     = sum(TXXT[VALID]),
      TOT_REF      = sum(TXXT_REF[VALID]),
      TOT_ADD_TIME = sum(TXXT[VALID] - TXXT_REF[VALID]),
      .groups = "drop"
    )
}

read_gold <- function(y) {
  read_csv(golden_paths[[as.character(y)]], show_col_types = FALSE) |>
    mutate(DATE = as_date(DATE))
}
score <- function(daily, gold) {
  gold |>
    rename_with(~ paste0(.x, "_g"), -c(ICAO, PHASE, DATE)) |>
    left_join(daily, by = c("ICAO", "PHASE", "DATE")) |>
    summarise(
      rows = n(),
      valid_ok = sum(MVTS_VALID == MVTS_VALID_g, na.rm = TRUE),
      na_ok    = sum(MVTS_NA == MVTS_NA_g, na.rm = TRUE),
      txxt_ok  = sum(abs(TOT_TXXT - TOT_TXXT_g) < tol, na.rm = TRUE),
      ref_ok   = sum(abs(TOT_REF  - TOT_REF_g)  < tol, na.rm = TRUE)
    )
}

# =============================================================================
# 1) Testa o recorte do periodo de referencia (2024)
# =============================================================================
message("Lendo dsTaxi", ref_year, "...")
mov2024 <- prep(read_harmonised(ref_year))
gold2024 <- read_gold(ref_year)

cat("\n---- Quantos movimentos-referencia caem FORA de 2024? ----\n")
rs <- mov2024 |> filter(VALID_TXXT, if_all(all_of(ref_key), ~ !is.na(.x)))
cat("por DATE:      ", sum(year(rs$DATE) != ref_year, na.rm = TRUE), "\n")
cat("por MVT_TIME:  ", sum(rs$MVT_TIME < ref_start | rs$MVT_TIME > ref_end, na.rm = TRUE), "\n")
cat("por BLOCK_TIME:", sum(rs$BLOCK_TIME < ref_start | rs$BLOCK_TIME > ref_end, na.rm = TRUE), "\n")

cat("\n==== Scoreboard 2024 por recorte de referencia ====\n")
sb <- map_dfr(names(ref_filters), function(rf) {
  ref_tbl <- build_reference(mov2024, rf)
  score(build_daily(ref_tbl, mov2024), gold2024) |>
    mutate(ref_filter = rf, .before = 1)
}) |>
  arrange(desc(ref_ok))
print(as.data.frame(sb))

best_rf <- sb$ref_filter[1]
cat(sprintf("\nMELHOR recorte: %s\n", best_rf))

# =============================================================================
# 2) Diagnostico do TOT_REF que ainda nao bate (melhor recorte)
# =============================================================================
ref_tbl_best <- build_reference(mov2024, best_rf)
diag <- gold2024 |>
  rename_with(~ paste0(.x, "_g"), -c(ICAO, PHASE, DATE)) |>
  left_join(build_daily(ref_tbl_best, mov2024), by = c("ICAO", "PHASE", "DATE")) |>
  mutate(d_ref = TOT_REF - TOT_REF_g)

cat("\n---- Residuo em TOT_REF ----\n")
cat("linhas com ref OK:", sum(abs(diag$d_ref) < tol, na.rm = TRUE),
    "de", nrow(diag), "\n")
cat("resumo de |d_ref| nas que NAO batem:\n")
print(summary(abs(diag$d_ref[abs(diag$d_ref) >= tol])))

cat("\n---- 12 exemplos onde TOT_REF nao bate ----\n")
diag |>
  filter(abs(d_ref) >= tol) |>
  transmute(ICAO, PHASE, DATE, MVTS_VALID,
            REF_our = round(TOT_REF, 4), REF_g = round(TOT_REF_g, 4),
            d_ref = round(d_ref, 4)) |>
  head(12) |>
  as.data.frame() |>
  print()

# =============================================================================
# 3) Validacao final nos 3 anos (ref sempre de 2024)
# =============================================================================
cat("\n==== Validacao por ano (recorte:", best_rf, ") ====\n")
for (y in data_years) {
  movy <- if (y == ref_year) mov2024 else prep(read_harmonised(y))
  s <- score(build_daily(ref_tbl_best, movy), read_gold(y))
  cat(sprintf("%d: rows=%d valid_ok=%d na_ok=%d txxt_ok=%d ref_ok=%d\n",
              y, s$rows, s$valid_ok, s$na_ok, s$txxt_ok, s$ref_ok))
}
