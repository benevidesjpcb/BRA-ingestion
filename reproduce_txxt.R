#!/usr/bin/env Rscript
# =============================================================================
# reproduce_txxt.R
#
# Reproduz os analiticos diarios de taxi-time (PBWG-BRA-txxt-analytic-*) SEM o
# pacote PBWG, e VALIDA contra os CSVs de resultado conhecidos.
#
# Ideia: nao sabemos de antemao (a) a chave de agrupamento da referencia,
# (b) o tipo de percentil, (c) de onde vem a DATE, nem (d) o sinal do taxi-time.
# Entao o script VARRE combinacoes candidatas sobre o ano de referencia (2024)
# e reporta qual delas reproduz EXATAMENTE o seu CSV de 2024. A combinacao
# vencedora e' entao aplicada a 2023/2024/2025 e validada nos tres anos.
#
# Rode na maquina que tem os dsTaxiYYYY.csv brutos. Ajuste os caminhos abaixo.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(data.table)
})

# ---- CONFIG: ajuste estes caminhos ------------------------------------------
raw_dir <- "data-raw"                       # onde estao os dsTaxiYYYY.csv
golden_paths <- c(                          # CSVs de resultado (golden)
  "2023" = "golden/PBWG-BRA-txxt-analytic-2023-ref2024-icao_ganp_p20.csv",
  "2024" = "golden/PBWG-BRA-txxt-analytic-2024-ref2024-icao_ganp_p20.csv",
  "2025" = "golden/PBWG-BRA-txxt-analytic-2025-ref2024-icao_ganp_p20.csv"
)

study_airports <- c("SBGR","SBGL","SBRJ","SBCF","SBBR","SBSV","SBKP","SBSP",
                    "SBCT","SBPA","SBRF","SBEG")
ref_year   <- 2024L
data_years <- 2023:2025
max_txxt   <- 120        # limite de taxi-time (min)
min_n      <- 5L         # amostras minimas por grupo p/ ter referencia
p_ref      <- 0.20       # percentil 20 (GANP p20)
tol        <- 1e-6       # tolerancia de comparacao dos totais

# ---- helpers: leitura + harmonizacao (espelha o .qmd) -----------------------
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
      FLTID = normalise_text(indicativo),
      REG   = normalise_text(matricula),
      ADEP  = normalise_text(adpartida),
      ADES  = normalise_text(addestino),
      PHASE = recode(normalise_text(mov), Arr = "ARR", Dep = "DEP",
                     .default = NA_character_),
      MVT_TIME   = parse_dt(dh_bimtra),
      BLOCK_TIME = parse_dt(dh_vra),
      ARCTYP = normalise_text(tipoaeronave),
      STND   = normalise_text(box),
      RWY    = normalise_text(pista),
      MATCH_VRA = normalise_text(match_vra),
      ICAO = case_when(PHASE == "ARR" ~ ADES, PHASE == "DEP" ~ ADEP,
                       TRUE ~ NA_character_)
    ) |>
    filter(ICAO %in% study_airports, !is.na(PHASE))
}

# ---- eixos do sweep (hipoteses a testar) ------------------------------------
# taxi-time por movimento (min). Duas hipoteses de sinal por fase.
txxt_variants <- list(
  # ARR = onblock - landing ; DEP = takeoff - offblock
  "block_minus_mvt_arr" = function(d) if_else(
    d$PHASE == "ARR",
    as.numeric(difftime(d$BLOCK_TIME, d$MVT_TIME, units = "mins")),
    as.numeric(difftime(d$MVT_TIME, d$BLOCK_TIME, units = "mins"))
  ),
  # valor absoluto (robusto a inversao de semantica)
  "abs_diff" = function(d) abs(
    as.numeric(difftime(d$BLOCK_TIME, d$MVT_TIME, units = "mins"))
  )
)
# de onde vem a data do movimento
date_variants <- list(
  "MVT"   = function(d) as_date(d$MVT_TIME),
  "BLOCK" = function(d) as_date(d$BLOCK_TIME)
)
# chave de agrupamento da referencia (alem de ICAO + PHASE)
key_variants <- list(
  "ICAO_PHASE"          = c("ICAO", "PHASE"),
  "ICAO_PHASE_STND"     = c("ICAO", "PHASE", "STND"),
  "ICAO_PHASE_RWY"      = c("ICAO", "PHASE", "RWY"),
  "ICAO_PHASE_STND_RWY" = c("ICAO", "PHASE", "STND", "RWY")
)
# tipo de quantile (R tem 9). GANP costuma ser 7 (default), mas testamos todos.
qtypes <- 1:9

# ---- nucleo: build reference + apply + daily --------------------------------
build_daily <- function(mov_ref, mov_all, key, qtype) {
  # referencia p20 por grupo, so grupos com n >= min_n (calculada em 2024)
  ref_tbl <- mov_ref |>
    filter(VALID_TXXT) |>
    group_by(across(all_of(key))) |>
    summarise(N = n(),
              TXXT_REF = quantile(TXXT, p_ref, type = qtype, names = FALSE),
              .groups = "drop") |>
    filter(N >= min_n) |>
    select(all_of(key), TXXT_REF)

  # aplica a referencia; valido = tem txxt ok E referencia do grupo existe
  applied <- mov_all |>
    left_join(ref_tbl, by = key) |>
    mutate(VALID = VALID_TXXT & !is.na(TXXT_REF))

  applied |>
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

compare <- function(daily, gold) {
  j <- gold |>
    rename_with(~ paste0(.x, "_g"), -c(ICAO, PHASE, DATE)) |>
    left_join(daily, by = c("ICAO", "PHASE", "DATE"))
  j |>
    summarise(
      n_rows        = n(),
      n_missing     = sum(is.na(MVTS_VALID)),
      mvts_valid_ok = sum(MVTS_VALID == MVTS_VALID_g, na.rm = TRUE),
      mvts_na_ok    = sum(MVTS_NA == MVTS_NA_g, na.rm = TRUE),
      txxt_ok       = sum(abs(TOT_TXXT - TOT_TXXT_g) < tol, na.rm = TRUE),
      ref_ok        = sum(abs(TOT_REF  - TOT_REF_g)  < tol, na.rm = TRUE),
      max_err_txxt  = max(abs(TOT_TXXT - TOT_TXXT_g), na.rm = TRUE),
      max_err_ref   = max(abs(TOT_REF  - TOT_REF_g),  na.rm = TRUE)
    )
}

# =============================================================================
# 1) SWEEP no ano de referencia (2024) para descobrir a config
# =============================================================================
message("Lendo dsTaxi", ref_year, " (harmonizado)...")
mov2024_raw <- read_harmonised(ref_year)
gold2024 <- read_csv(golden_paths[[as.character(ref_year)]], show_col_types = FALSE) |>
  mutate(DATE = as_date(DATE))

results <- list()
for (tx in names(txxt_variants)) {
  for (dt in names(date_variants)) {
    mov <- mov2024_raw |>
      mutate(
        TXXT = txxt_variants[[tx]](pick(everything())),
        DATE = date_variants[[dt]](pick(everything())),
        VALID_TXXT = !is.na(TXXT) & TXXT > 0 & TXXT <= max_txxt
      )
    for (kn in names(key_variants)) {
      for (qt in qtypes) {
        daily <- build_daily(mov, mov, key_variants[[kn]], qt)
        cmp <- compare(daily, gold2024)
        results[[length(results) + 1]] <- cmp |>
          mutate(txxt = tx, date = dt, key = kn, qtype = qt, .before = 1)
      }
    }
  }
}
res <- bind_rows(results) |>
  arrange(desc(txxt_ok + ref_ok + mvts_valid_ok + mvts_na_ok))

cat("\n==== TOP 10 combinacoes (2024) ====\n")
res |>
  transmute(txxt, date, key, qtype,
            rows = n_rows, valid_ok = mvts_valid_ok, na_ok = mvts_na_ok,
            txxt_ok, ref_ok, max_err_txxt = round(max_err_txxt, 4)) |>
  head(10) |>
  print(n = 10)

best <- res |> slice(1)
cat("\n==== MELHOR combinacao ====\n"); print(as.data.frame(best))

# =============================================================================
# 2) Aplica a melhor config a 2023/2024/2025 e valida
# =============================================================================
tx <- best$txxt; dt <- best$date; kn <- best$key; qt <- best$qtype
key <- key_variants[[kn]]

prep <- function(df) df |>
  mutate(
    TXXT = txxt_variants[[tx]](pick(everything())),
    DATE = date_variants[[dt]](pick(everything())),
    VALID_TXXT = !is.na(TXXT) & TXXT > 0 & TXXT <= max_txxt
  )

mov_ref <- prep(mov2024_raw)

cat("\n==== Validacao por ano ====\n")
for (y in data_years) {
  movy <- if (y == ref_year) mov_ref else prep(read_harmonised(y))
  daily <- build_daily(mov_ref, movy, key, qt)     # ref sempre de 2024
  gold  <- read_csv(golden_paths[[as.character(y)]], show_col_types = FALSE) |>
    mutate(DATE = as_date(DATE))
  cmp <- compare(daily, gold)
  cat(sprintf("%d: linhas=%d  valid_ok=%d  na_ok=%d  txxt_ok=%d  ref_ok=%d  max_err_txxt=%.4g\n",
              y, cmp$n_rows, cmp$mvts_valid_ok, cmp$mvts_na_ok,
              cmp$txxt_ok, cmp$ref_ok, cmp$max_err_txxt))
}
cat("\nSe valid_ok/na_ok/txxt_ok/ref_ok == linhas nos 3 anos, a reproducao bate exato.\n")
