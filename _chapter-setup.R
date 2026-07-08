################## SETUP ######################################################
# Quarto renders each chapter in a separate session.
# To save loading the same libraries in every chapter, we define defaults here.
# This script/definitions are sourced() at the beginning of every chapter.
###############################################################################

# load required libraries for each chapter ====================================
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(ggforce)
#-------- supporting packages
library(flextable)
library(zoo)
library(magrittr)
library(purrr)
library(glue)
library(pdftools)
library(devtools)
# library(readr)     # included in tidyverse
# library(tinytex)
library(arrow)
library(rnaturalearth)
library(ggplot2)
library(treemapify)
library(ggbump)

library(gt)
library(kableExtra)


# ============== DEFAULTS and DEFINITIONS =====================================
# study year
this_year <- 2025

# max_date
max_date <- lubridate::ymd("2025-12-31")

# set ggplot2 default theme
ggplot2::theme_set(theme_minimal())

#============== flextable stuff ===============================================
# set flextable font to surpress warning about used Latex engine
flextable::set_flextable_defaults(
  fonts_ignore = TRUE, # ignore waring of Latex engine
  font.size = 10, # set some default size and family
  font.family = "Helvetica"
)

# set flextable border properties
ft_border = flextable::fp_border_default(width = 0.5)

# study airports and names ====================================================
bra_apts <- c(
  "SBGR",
  "SBGL",
  "SBRJ",
  "SBCF",
  "SBBR",
  "SBSV",
  "SBKP",
  "SBSP",
  "SBCT",
  "SBPA",
  "SBRF",
  "SBEG"
)
eur_apts <- c(
  "EGLL",
  "EGKK",
  "EHAM",
  "EDDF",
  "EDDM",
  "LSZH",
  "LFPG",
  "LEMD",
  "LEBL",
  "LPPT",
  "LGAV",
  "LTFM"
) # ,"LIRF"

bra_apts_names <- tibble::tribble(
  ~ICAO , ~NAME , "SBGR" , "Guarulhos" , "SBGL" , "Galeão" , "SBRJ" , "Santos Dumont" , "SBCF" , "Belo Horizonte" , "SBBR" , "Brasília" , "SBSV" , "Salvador" , "SBKP" , "Campinas" , "SBSP" , "Congonhas" , "SBCT" , "Curitiba" , "SBPA" , "Porto Alegre" , "SBRF" , "Recife" , "SBEG" , "Eduardo Gomes"
)

eur_apts_names <- tibble::tribble(
  ~ICAO , ~NAME , "EGLL" , "Heathrow" , "EGKK" , "Gatwick" , "EHAM" , "Amsterdam" , "EDDF" , "Frankfurt" , "EDDM" , "Munich" , "LSZH" , "Zurich" , "LIRF" , "Rome" , "LFPG" , "Paris" , "LEMD" , "Madrid" , "LEBL" , "Barcelona" , "LPPT" , "Lisbon" , "LGAV" , "Athens" , "LTFM" , "Istanbul"
)


# define standard theme aspects for Brazil and Europe =========================
bra_eur_colours <- c(BRA = "#52854C", EUR = "#4E84C4")
bra_col <- getElement(bra_eur_colours, "BRA")
eur_col <- getElement(bra_eur_colours, "EUR")
YEAR_COLORS <- c("2023" = "#E74C3C", "2024" = "#2ECC71", "2025" = "#5DADE2")

# theme setting - tbd or replaced
bra_eur_theme_minimal <- theme_minimal() +
  theme(axis.title = element_text(size = 9))
bra_eur_theme_bw <- theme_bw() + theme(axis.title = element_text(size = 9))


# table BRA-EUR

table_bra_eur <- tibble::tribble(
  ~KPA                                         , ~Brazil_2019 , ~Brazil_2020 , ~Brazil_2021 , ~Brazil_2022 , ~Brazil_2023 , ~Brazil_2024 , ~Brazil_2025 , ~Europe_2023 , ~Europe_2024 , ~Europe_2025 ,
  "geographic area (non-oceanic million km²)¹" , "8.5"        , "8.5"        , "8.5"        , "8.5"        , "8.5"        , "8.5"        , "8.5"        , "10.9"       , "10.9"       , "10.9"       ,
  "number of en-route ANSPs²"                  , "1"          , "1"          , "1"          , "1"          , "1"          , "1"          , "1"          , "37"         , "37"         , "37"         ,
  "number of TWR¹"                             , "59 TWR"     , "60 TWR"     , "57+1 DTWR"  , "57+1 DTWR"  , "57+1 DTWR"  , "57+1 DTWR"  , "59+1 DTWR"  , "374"        , "373"        , "n/a"        ,
  "number of APP¹"                             , "43"         , "43"         , "42"         , "42"         , "41"         , "41"         , "42"         , "268"        , "266"        , "n/a"        ,
  "number of ACC¹"                             , "5"          , "5"          , "5"          , "1"          , "5"          , "5"          , "5"          , "57"         , "57"         , "57"         ,
  "number of ATCOs in OPS¹"                    , "3606"       , "3376"       , "3549"       , "3754"       , "3677"       , "3890"       , "3893"       , "16973"      , "17186"      , "n/a"        ,
  "controlled flights³"                        , "1594442"    , "1018181"    , "1286224"    , "1677760"    , "1801109"    , "1995139"    , "2109588"    , "10144258"   , "10633991"   , "11046028"   ,
  "flights ATCO"                               , "362"        , "302"        , "362"        , "447"        , "490"        , "497"        , "542"        , "598"        , "619"        , "n/a"        ,
  "traffic density (non-oceanic flights/km²)"  , "0.22"       , "0.13"       , "0.12"       , "0.16"       , "0.18"       , "0.19"       , "0.21"       , "0.93"       , "0.976"      , "n/a"
)
