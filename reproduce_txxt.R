#!/usr/bin/env Rscript
# =============================================================================
# reproduce_txxt.R  (v2)
#
# Reproduz os analiticos diarios de taxi-time (PBWG-BRA-txxt-analytic-*) SEM o
# pacote PBWG, e VALIDA contra os CSVs de resultado (golden).
#
# v2: 4 parametros ja confirmados estao TRAVADOS (ver bloco LOCKED). O foco agora
# e' fechar (1) a contagem MVTS_NA e (2) a soma da referencia TOT_REF. O script
# testa poucas variacoes de DATE (com fallback) e do tratamento de chave NA,
# e imprime um DIAGNOSTICO detalhado apontando a direcao do erro.
#
# Rode na maquina com os dsTaxiYYYY.csv brutos. Ajuste os caminhos se preciso.
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

# ---- LOCKED (confirmado pelo sweep anterior) --------------------------------
max_txxt <- 120
min_n    <- 5L
p_ref    <- 0.20
qtype    <- 7L                              # tipo de quantile
ref_key  <- c("ICAO", "PHASE", "STND", "RWY")

# ---- helpers: leitura + harmonizacao ----------------------------------------
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

# taxi-time confirmado: ARR = block - mvt ; DEP = mvt - block
txxt_fn <- function(d) if_else(
  d$PHASE == "ARR",
  as.numeric(difftime(d$BLOCK_TIME, d$MVT_TIME, units = "mins")),
  as.numeric(difftime(d$MVT_TIME, d$BLOCK_TIME, units = "mins"))
)

# variacoes de DATE a testar (para fechar MVTS_NA)
date_variants <- list(
  "BLOCK"        = function(d) as_date(d$BLOCK_TIME),
  "MVT"          = function(d) as_date(d$MVT_TIME),
  "COALESCE_B_M" = function(d) as_date(coalesce(d$BLOCK_TIME, d$MVT_TIME)),
  "COALESCE_M_B" = function(d) as_date(coalesce(d$MVT_TIME, d$BLOCK_TIME))
)

prep <- function(df, date_key) {
  df |>
    mutate(
      TXXT = txxt_fn(pick(everything())),
      DATE = date_variants[[date_key]](pick(everything())),
      VALID_TXXT = !is.na(TXXT) & TXXT > 0 & TXXT <= max_txxt
    )
}

build_daily <- function(mov_ref, mov_all, exclude_na_key) {
  ref_src <- mov_ref |> filter(VALID_TXXT)
  if (exclude_na_key) {
    ref_src <- ref_src |> filter(if_all(all_of(ref_key), ~ !is.na(.x)))
  }
  ref_tbl <- ref_src |>
    group_by(across(all_of(ref_key))) |>
    summarise(N = n(),
              TXXT_REF = quantile(TXXT, p_ref, type = qtype, names = FALSE),
              .groups = "drop") |>
    filter(N >= min_n) |>
    select(all_of(ref_key), TXXT_REF)

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
      rows      = n(),
      missing   = sum(is.na(MVTS_VALID)),
      valid_ok  = sum(MVTS_VALID == MVTS_VALID_g, na.rm = TRUE),
      na_ok     = sum(MVTS_NA == MVTS_NA_g, na.rm = TRUE),
      txxt_ok   = sum(abs(TOT_TXXT - TOT_TXXT_g) < tol, na.rm = TRUE),
      ref_ok    = sum(abs(TOT_REF  - TOT_REF_g)  < tol, na.rm = TRUE)
    )
}

# =============================================================================
# 1) Mini-sweep sobre 2024: DATE x (excluir chave NA na referencia)
# =============================================================================
message("Lendo dsTaxi", ref_year, "...")
raw2024 <- read_harmonised(ref_year)
gold2024 <- read_gold(ref_year)

grid <- expand_grid(date_key = names(date_variants), exclude_na_key = c(FALSE, TRUE))
sb <- pmap_dfr(grid, function(date_key, exclude_na_key) {
  mov <- prep(raw2024, date_key)
  daily <- build_daily(mov, mov, exclude_na_key)
  score(daily, gold2024) |>
    mutate(date_key = date_key, exclude_na_key = exclude_na_key, .before = 1)
}) |>
  arrange(desc(na_ok + valid_ok + txxt_ok + ref_ok))

cat("\n==== Scoreboard 2024 (LOCKED: block-mvt, STND+RWY, qtype 7) ====\n")
print(as.data.frame(sb))

best <- sb |> slice(1)
cat(sprintf("\nMELHOR: date=%s  exclude_na_key=%s\n", best$date_key, best$exclude_na_key))

# =============================================================================
# 2) DIAGNOSTICO detalhado da melhor config (2024)
# =============================================================================
mov_best <- prep(raw2024, best$date_key)
daily_best <- build_daily(mov_best, mov_best, best$exclude_na_key)
diag <- gold2024 |>
  rename_with(~ paste0(.x, "_g"), -c(ICAO, PHASE, DATE)) |>
  left_join(daily_best, by = c("ICAO", "PHASE", "DATE")) |>
  mutate(
    tot_our  = MVTS_VALID + MVTS_NA,
    tot_gold = MVTS_VALID_g + MVTS_NA_g,
    d_total  = tot_our - tot_gold,
    d_valid  = MVTS_VALID - MVTS_VALID_g,
    d_na     = MVTS_NA - MVTS_NA_g
  )

cat("\n---- Direcao do erro no TOTAL de movimentos (our - gold) ----\n")
cat("linhas com total  MENOR  que o gold:", sum(diag$d_total < 0, na.rm = TRUE), "\n")
cat("linhas com total  IGUAL  ao gold  :", sum(diag$d_total == 0, na.rm = TRUE), "\n")
cat("linhas com total  MAIOR  que o gold:", sum(diag$d_total > 0, na.rm = TRUE), "\n")
cat("resumo de (our-gold) no total:\n"); print(summary(diag$d_total))
cat("resumo de (our-gold) em MVTS_VALID:\n"); print(summary(diag$d_valid))
cat("resumo de (our-gold) em MVTS_NA:\n"); print(summary(diag$d_na))

cat("\n---- Diferenca media por aeroporto (|d_total|, |d_na|, |d_ref|) ----\n")
diag |>
  mutate(d_ref = abs(TOT_REF - TOT_REF_g)) |>
  group_by(ICAO) |>
  summarise(n = n(),
            d_total = round(mean(abs(d_total), na.rm = TRUE), 2),
            d_na    = round(mean(abs(d_na), na.rm = TRUE), 2),
            d_ref   = round(mean(d_ref, na.rm = TRUE), 2),
            .groups = "drop") |>
  arrange(desc(d_total)) |>
  as.data.frame() |>
  print()

cat("\n---- 12 exemplos onde MVTS_NA nao bate ----\n")
diag |>
  filter(MVTS_NA != MVTS_NA_g) |>
  transmute(ICAO, PHASE, DATE,
            V_our = MVTS_VALID, V_g = MVTS_VALID_g,
            NA_our = MVTS_NA, NA_g = MVTS_NA_g,
            REF_our = round(TOT_REF, 2), REF_g = round(TOT_REF_g, 2)) |>
  head(12) |>
  as.data.frame() |>
  print()

# =============================================================================
# 3) Validacao final da melhor config nos 3 anos
# =============================================================================
cat("\n==== Validacao por ano (melhor config) ====\n")
mov_ref <- prep(raw2024, best$date_key)
for (y in data_years) {
  movy <- if (y == ref_year) mov_ref else prep(read_harmonised(y), best$date_key)
  daily <- build_daily(mov_ref, movy, best$exclude_na_key)
  s <- score(daily, read_gold(y))
  cat(sprintf("%d: rows=%d missing=%d valid_ok=%d na_ok=%d txxt_ok=%d ref_ok=%d\n",
              y, s$rows, s$missing, s$valid_ok, s$na_ok, s$txxt_ok, s$ref_ok))
}
