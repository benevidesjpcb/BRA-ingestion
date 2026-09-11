#!/usr/bin/env Rscript
# =============================================================================
# render_totalbr_panel.R -- the panel as a file you can regenerate
#
#   source(here::here("TOTALBR", "render_totalbr_panel.R"))
#   totalbr_panel_render(2026, 2025)          # -> outputs/totalbr/painel-*.html
#   browseURL(totalbr_panel_render(2026, 2025))
#
# WHY THIS EXISTS. The panel was first written by hand, with the figures typed
# into the HTML. That version cannot be re-run: next month's numbers mean
# retyping forty of them, and a typo in any one is invisible -- it looks exactly
# like a number. Everything here is rendered FROM totalbr_panel(), so the page
# and the data cannot disagree, and regenerating is the same command every time.
#
# It writes a single self-contained .html: no server, no build step, no network
# except the Google Fonts link, which degrades to the fallback stack offline.
#
# THE EUROPEAN COLUMN IS A HOLE ON PURPOSE. Every block that will hold
# EUROCONTROL figures renders a marked placeholder instead, so the layout is
# already the two-column one and the data can be dropped in without redesigning
# anything. Pass europe = a list of the same shape to fill it.
# =============================================================================

source(here::here("TOTALBR", "panel_totalbr.R"))

# ---- formatting, Brazilian ---------------------------------------------------
# 1059722 -> "1,059,722" and 88.2 -> "88.2%". THE PAGE IS ENGLISH, so the marks
# are the English ones -- comma for thousands, point for the decimal. They are
# set here rather than at each call site so every figure on the page is spelled
# the same way; a panel that mixes 1,059,722 and 1.059.722 reads as two
# documents stapled together. Switching the page to Portuguese is these two
# functions and nothing else.
# decimal.mark is declared even though format="d" never emits a decimal: without
# it formatC warns that the thousands and decimal marks are both "." on every
# single call, and forty of those per page buries a warning that would matter.
.tb_n <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")
.tb_pct <- function(x, dp = 1) paste0(formatC(as.numeric(x), format = "f",
                                              digits = dp), "%")

# A signed change as its own chip. The CLASS carries the sign, not just the
# colour: "flat" for anything under 2% either way, so a rounding wobble does not
# get painted as a trend.
.tb_delta <- function(new, old, small = 2) {
  if (is.na(old) || is.na(new) || old == 0) return("")
  v   <- 100 * (new - old) / old
  cls <- if (abs(v) < small) "flat" else if (v > 0) "up" else "down"
  sig <- if (v > 0) "+" else "−"                       # a real minus sign
  sprintf('<span class="delta %s">%s%s</span>', cls, sig,
          .tb_pct(abs(v)))
}

.tb_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# ---- the aerodrome map ------------------------------------------------------
# NOT A TRACED OUTLINE. The aerodromes are plotted at their own coordinates and
# sized by their movements, so the country draws itself out of the data: the
# coast, the empty centre-west and the crowded south-east are all real. A
# silhouette would carry none of that, and would be a drawing rather than a
# measurement.
#
# Equirectangular, which is honest at this scale and needs no projection
# library. The bounds are Brazil's; pass your own for another region.
totalbr_panel_dots <- function(d, bounds = c(lon0 = -74.5, lon1 = -33.5,
                                             lat0 = 6, lat1 = -34.5),
                               w = 300, h = 330, n = 420,
                               file = here::here("data-raw", "airports.csv")) {
  D <- data.table::as.data.table(d)
  home <- c(D[ADEP_CNTRY == "BR", ADEP], D[ADES_CNTRY == "BR", ADES])
  tb <- data.table::as.data.table(table(home))
  data.table::setnames(tb, c("ICAO", "N"))

  a <- data.table::fread(file = file, colClasses = "character",
         select = c("ident", "icao_code", "gps_code",
                    "latitude_deg", "longitude_deg"),
         na.strings = NULL, showProgress = FALSE)
  # the same three keys totalbr_country_lookup() uses, for the same reason: a
  # third of the file carries its code in ident or gps_code, not icao_code
  k <- data.table::rbindlist(lapply(c("icao_code", "ident", "gps_code"),
        function(cl) a[nzchar(get(cl)), .(K = get(cl), latitude_deg, longitude_deg)]))
  k <- k[!duplicated(K)]

  m <- merge(tb, k, by.x = "ICAO", by.y = "K")
  m[, `:=`(lat = suppressWarnings(as.numeric(latitude_deg)),
           lon = suppressWarnings(as.numeric(longitude_deg)))]
  m <- m[!is.na(lat) & !is.na(lon) &
         lon >= bounds[["lon0"]] & lon <= bounds[["lon1"]] &
         lat <= bounds[["lat0"]] & lat >= bounds[["lat1"]]][order(-N)]
  if (nrow(m) == 0) return("")
  m <- utils::head(m, n)

  mx <- max(m$N)
  x  <- (m$lon - bounds[["lon0"]]) / (bounds[["lon1"]] - bounds[["lon0"]]) * w
  y  <- (bounds[["lat0"]] - m$lat) / (bounds[["lat0"]] - bounds[["lat1"]]) * h
  r  <- 0.9 + 6.6 * sqrt(m$N / mx)
  cls <- ifelse(m$N > 0.17 * mx, "p-hub", ifelse(m$N > 0.02 * mx, "p-mid", "p-sm"))
  # smallest first, so the hubs are not buried under the strips around them
  o <- order(m$N)
  paste(sprintf('<circle class="%s" cx="%.1f" cy="%.1f" r="%.2f"/>',
                cls[o], x[o], y[o], r[o]), collapse = "\n")
}

# ---- pieces -----------------------------------------------------------------
# One ranked row: the 2026 bar, the 2025 bar under it, the value and the change.
# Widths are shares of the largest row, so the longest bar always fills its
# track and the rest are read against it.
.tb_row <- function(label, now, before, top, klass = "") {
  sprintf(
    '<div class="row"><span class="rname">%s</span><span class="rval"><b class="rpct mono">%s</b>%s</span><span class="track"><i class="bar %s" style="width:%.1f%%"></i><i class="bar r" style="width:%.1f%%"></i></span></div>',
    .tb_esc(label), .tb_n(now), .tb_delta(now, before), klass,
    100 * now / top, if (is.na(before)) 0 else 100 * before / top)
}

.tb_rows <- function(df, label_col, klass = "") {
  if (is.null(df) || nrow(df) == 0) return("")
  top <- max(df$FLIGHTS)
  paste(vapply(seq_len(nrow(df)), function(i)
    .tb_row(df[[label_col]][i], df$FLIGHTS[i], df$BEFORE[i], top, klass),
    character(1)), collapse = "\n")
}

# ---- the flags ---------------------------------------------------------------
# THE FIRST VERSION OF THIS PANEL DREW NEITHER FLAG. It used a three-stripe bar
# -- green/yellow/blue for Brazil, blue/yellow/blue for Europe -- which is not
# what either flag looks like: Brazil is a yellow rhombus on green with a blue
# globe, and the European flag is a ring of twelve gold stars on blue. A flag is
# an identity, not a colour scheme, and getting it wrong is the kind of error a
# reader notices before they read a single number.
#
# Both are drawn to their official proportions (Brazil 20:14, Europe 3:2) and
# simplified only where the detail would be invisible at 20 pixels wide: the
# globe carries no constellation and no banner. That is a legible reduction of
# the real flag, not an invention.
TOTALBR_FLAG_BR <- paste0(
  '<svg class="flag" viewBox="0 0 20 14" role="img" aria-label="Brazil">',
  '<rect width="20" height="14" fill="#009B3A"/>',
  '<path d="M10 1.6 18.4 7 10 12.4 1.6 7Z" fill="#FEDF00"/>',
  '<circle cx="10" cy="7" r="3.1" fill="#002776"/>',
  '</svg>')

# Twelve stars, evenly spaced on a circle -- the number is fixed and has nothing
# to do with the number of member states, so it does not change.
TOTALBR_FLAG_EU <- local({
  pts <- vapply(0:11, function(i) {
    a <- pi / 2 + i * pi / 6              # first star at twelve o'clock
    sprintf('<circle cx="%.2f" cy="%.2f" r="0.62" fill="#FFCC00"/>',
            10.5 + 4.1 * cos(a), 7 - 4.1 * sin(a))
  }, character(1))
  paste0('<svg class="flag" viewBox="0 0 21 14" role="img" aria-label="Europe">',
         '<rect width="21" height="14" fill="#003399"/>',
         paste(pts, collapse = ""), '</svg>')
})

# ---- the logos ---------------------------------------------------------------
# Embedded as data URIs so the page stays one file. NOT hand-drawn: an official
# emblem approximated in SVG is wrong in a way that misrepresents the
# organisation, so when no file is given the header renders a marked slot and
# the page says plainly that the logo is missing rather than showing something
# almost right.
.tb_logo <- function(path, alt) {
  if (is.null(path) || !nzchar(path) || !file.exists(path))
    return(sprintf('<span class="logoslot" title="%s">%s</span>',
                   .tb_esc(alt), .tb_esc(alt)))
  ext  <- tolower(tools::file_ext(path))
  mime <- switch(ext, svg = "image/svg+xml", png = "image/png",
                 jpg = , jpeg = "image/jpeg", gif = "image/gif", NULL)
  if (is.null(mime)) {
    warning("Unsupported logo format '", ext, "' for ", path, "; slot left empty.")
    return(sprintf('<span class="logoslot">%s</span>', .tb_esc(alt)))
  }
  b64 <- base64enc::base64encode(path)
  sprintf('<img class="logo" src="data:%s;base64,%s" alt="%s">', mime, b64,
          .tb_esc(alt))
}

.tb_placeholder <- function(title, sub, tall = FALSE) sprintf(
  '<div class="empty"%s><span class="mark">EU</span><b>%s</b><span>%s</span></div>',
  if (tall) "" else ' style="min-height:126px"', .tb_esc(title), .tb_esc(sub))

# The donut, drawn from the shares rather than from four hand-set dasharrays.
# r = 54 on a 126 box; the circumference is what every segment is a fraction of.
.tb_donut <- function(pcts, colors) {
  circ <- 2 * pi * 54
  off  <- 0
  seg  <- character(0)
  for (i in seq_along(pcts)) {
    len <- circ * pcts[i] / 100
    seg <- c(seg, sprintf(
      '<circle cx="63" cy="63" r="54" stroke="%s" stroke-dasharray="%.2f %.2f" stroke-dashoffset="%.2f"/>',
      colors[i], len, circ - len, -off))
    off <- off + len
  }
  sprintf('<svg viewBox="0 0 126 126" role="img" aria-label="DAIO distribution"><g transform="rotate(-90 63 63)" fill="none" stroke-width="17">%s</g></svg>',
          paste(seg, collapse = ""))
}

# =============================================================================
# totalbr_panel_year(year, months) -- a year's rows, from wherever they live
#
# The month parts only exist for the years this project has downloaded; earlier
# years are in the parquet archive. Which one holds a given year is not
# something the caller should have to know, so this tries the parts and falls
# back to the archive, and SAYS which it used -- a panel whose reference year
# came from a different file than you assumed is the kind of thing that has to
# be visible, not inferred.
#
# The archive is sliced on its own year/month columns only to avoid reading 5.9
# million rows; the period is then trimmed on dt_dia by totalbr_panel(), which
# is what makes it comparable. See the note there.
# =============================================================================
totalbr_panel_year <- function(year, months = 1:6, feed = "cgna",
                               archive = NULL, quiet = FALSE) {
  parts <- vapply(months, function(m) totalbr_daio_part(year, m, feed = feed),
                  character(1))
  if (all(file.exists(parts))) {
    if (!quiet) message("  ", year, ": month parts (", toupper(feed), ")")
    return(totalbr_panel_load(year, months, feed, quiet = TRUE))
  }

  # totalbr_daio_source() is the project's own answer to "where is the archive":
  # the BRA_TOTALBR_PARQUET env var, then data-src/, then data-raw/totalbr/, then
  # data-raw/. Searching for it here separately is how this reported "no archive"
  # on a machine that had one, in a directory the narrower search did not know.
  if (is.null(archive)) archive <- totalbr_daio_source()
  if (!file.exists(archive))
    stop("No month part for ", year, ", and no .parquet archive found.",
         "\n  missing part(s): ",
         paste(basename(parts[!file.exists(parts)]), collapse = ", "),
         "\n  looked in: ", paste(c(here::here("data-src"),
                                    here::here("data-raw", "totalbr"),
                                    here::here("data-raw")), collapse = ", "),
         "\nPoint BRA_TOTALBR_PARQUET at the file, or pass archive = \"<path>\".")
  if (!quiet) message("  ", year, ": ", basename(archive))

  for (pkg in c("arrow", "dplyr"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Reading the archive needs the '", pkg, "' package.")

  ds  <- arrow::open_dataset(archive)
  nm  <- names(ds)
  if (!all(c("year", "month") %in% nm))
    stop("The archive has no year/month columns to slice on: ", archive)
  want <- intersect(c("co_indicativo", "co_addep", "co_addes", "co_modelo",
                      "dt_dia", "li_tipovoo", "co_tipo_voo"), nm)
  # THE SLICE IS PADDED BY A MONTH ON EACH SIDE, AND THE TRIM DOES THE REST.
  # The archive's own year/month columns do not agree with dt_dia at the
  # boundaries, so a row whose dt_dia falls in June can be filed under month 5.
  # Slicing exactly the wanted months fetches a set the later DATE trim can only
  # ever shrink -- it cannot recover a row that was never read. Measured: June
  # 2025 alone came to 173,839 sliced exactly, against 174,029 when it arrived
  # inside a January-June slice. 190 flights, lost silently, and only visible
  # because the two ways of asking disagreed.
  #
  # Padding costs one extra month of rows at each end and makes the answer
  # independent of how the archive filed its boundaries.
  span  <- seq(min(months) - 1L, max(months) + 1L)
  yrs   <- year + ifelse(span < 1, -1L, ifelse(span > 12, 1L, 0L))
  mons  <- ((span - 1L) %% 12L) + 1L
  keyed <- paste(yrs, mons)
  raw <- dplyr::collect(dplyr::select(
           dplyr::filter(ds, paste(year, month) %in% !!keyed),
           dplyr::all_of(want)))
  if (nrow(raw) == 0)
    stop("The archive holds no rows for ", year, " month(s) ",
         paste(range(months), collapse = "-"), ": ", archive)
  totalbr_daio(src = as.data.frame(raw), feed = feed, quiet = TRUE)
}

# ---- the stylesheet ---------------------------------------------------------
# Kept as one string so the whole page is one file with no external CSS. THE
# PALETTE IS ALL AT THE TOP AND ALL IN TOKENS: change --br / --eu / --ref /
# --signal and the bars, the donut, the map dots and the chips all follow. Each
# token is declared three times -- the light :root, the prefers-color-scheme
# block and the [data-theme] block -- and changing only the first leaves the
# dark theme on the old palette.
#
# Two of them carry meaning rather than taste and should stay distinguishable:
# --signal is what separates a fall from a rise, and --ref has to read as
# clearly fainter than --br or the two years compete instead of one sitting
# behind the other.
TOTALBR_PANEL_CSS <- '<style>
  :root{
    --ground:#EDF0EF; --surface:#FFFFFF; --surface-2:#F5F8F7; --inset:#F0F4F3;
    --ink:#14201E; --ink-2:#41524F; --ink-3:#6D807B;
    --rule:#D5DCDA; --rule-2:#E5EBE9;
    --br:#0B5F63; --br-soft:#D3E4E3; --br-2:#3E8F86; --br-3:#7FB8AE;
    --eu:#1B4E7A; --eu-soft:#D8E3ED;
    --ref:#98A7A3; --ref-soft:#DCE3E1;
    --signal:#B33771; --signal-soft:#F1DBE6;
    --shadow:0 1px 2px rgba(20,32,30,.06);
  }
  @media (prefers-color-scheme:dark){
    :root:not([data-theme="light"]){
      --ground:#0E1412; --surface:#171F1D; --surface-2:#1D2624; --inset:#141C1A;
      --ink:#E6ECE9; --ink-2:#ADBCB8; --ink-3:#7D8E89;
      --rule:#2B3734; --rule-2:#222C2A;
      --br:#4FB3A9; --br-soft:#1B3A38; --br-2:#3E8F86; --br-3:#2C6C68;
      --eu:#6FA8D8; --eu-soft:#1A2C3E;
      --ref:#6A7975; --ref-soft:#252F2D;
      --signal:#E8709F; --signal-soft:#39202A;
      --shadow:0 1px 2px rgba(0,0,0,.3);
    }
  }
  :root[data-theme="dark"]{
    --ground:#0E1412; --surface:#171F1D; --surface-2:#1D2624; --inset:#141C1A;
    --ink:#E6ECE9; --ink-2:#ADBCB8; --ink-3:#7D8E89;
    --rule:#2B3734; --rule-2:#222C2A;
    --br:#4FB3A9; --br-soft:#1B3A38; --br-2:#3E8F86; --br-3:#2C6C68;
    --eu:#6FA8D8; --eu-soft:#1A2C3E;
    --ref:#6A7975; --ref-soft:#252F2D;
    --signal:#E8709F; --signal-soft:#39202A;
    --shadow:0 1px 2px rgba(0,0,0,.3);
  }
  *{box-sizing:border-box}
  body{
    background:var(--ground); color:var(--ink); margin:0;
    font-family:"Source Sans 3",system-ui,-apple-system,sans-serif;
    font-size:14.5px; line-height:1.5;
    padding:clamp(14px,3vw,34px) clamp(10px,3vw,26px);
  }
  .wrap{max-width:1060px; margin:0 auto; display:flex; flex-direction:column; gap:18px}
  h1,h2,h3{font-family:Archivo,system-ui,sans-serif; margin:0; text-wrap:balance}
  .num{font-family:Archivo,sans-serif; font-variant-numeric:tabular-nums}
  .mono{font-family:"IBM Plex Mono",ui-monospace,monospace; font-variant-numeric:tabular-nums}
  .lbl{font-family:Archivo,sans-serif; font-size:10.5px; font-weight:600;
       letter-spacing:.13em; text-transform:uppercase; color:var(--ink-3)}

  .masthead{display:flex; flex-wrap:wrap; align-items:baseline; justify-content:space-between; gap:10px; padding:0 2px}
  h1{font-size:clamp(19px,2.7vw,25px); font-weight:700; letter-spacing:-.02em}
  .masthead .lbl{font-size:11px}

  .duo{display:grid; grid-template-columns:1fr 1fr; gap:16px}
  @media (max-width:720px){ .duo{grid-template-columns:1fr} }

  .card{background:var(--surface); border:1px solid var(--rule); border-radius:9px;
        box-shadow:var(--shadow); padding:16px clamp(12px,2vw,18px);
        display:flex; flex-direction:column; gap:14px}
  .card.pending{background:repeating-linear-gradient(135deg,var(--surface),var(--surface) 9px,var(--surface-2) 9px,var(--surface-2) 18px);
                border-style:dashed; box-shadow:none}
  .badge{align-self:flex-start; display:inline-flex; align-items:center; gap:7px;
         background:var(--inset); border:1px solid var(--rule-2); border-radius:6px;
         padding:5px 10px; font-family:Archivo,sans-serif; font-size:11.5px;
         font-weight:700; letter-spacing:.1em; text-transform:uppercase; color:var(--ink)}
  .flag{width:19px; height:13px; border-radius:2px; flex-shrink:0; display:block;
        box-shadow:0 0 0 1px rgba(20,32,30,.16)}

  /* institutional header, three columns: owner / title / partner */
  .masthead-bar{background:var(--surface); border:1px solid var(--rule);
    border-radius:9px; box-shadow:var(--shadow); padding:13px clamp(12px,2.4vw,22px);
    display:grid; grid-template-columns:1fr auto 1fr; gap:14px; align-items:center}
  .org{display:flex; align-items:center; gap:10px; min-width:0}
  .org.right{justify-self:end; flex-direction:row-reverse; text-align:right}
  .org .name{font-family:Archivo,sans-serif; font-size:13.5px; font-weight:700;
    letter-spacing:.02em; color:var(--ink); line-height:1.15}
  .org .tag{font-size:11px; color:var(--ink-3); line-height:1.25}
  .org .stack{display:flex; flex-direction:column; gap:1px; min-width:0}
  .logo{height:34px; width:auto; max-width:112px; object-fit:contain; display:block}
  .logoslot{display:flex; align-items:center; justify-content:center; height:34px;
    min-width:52px; padding:0 8px; border:1.5px dashed var(--rule); border-radius:5px;
    font-family:Archivo,sans-serif; font-size:8.5px; font-weight:700;
    letter-spacing:.09em; text-transform:uppercase; color:var(--ink-3);
    text-align:center; line-height:1.1}
  .centre{text-align:center; min-width:0}
  .centre h1{font-size:clamp(15px,2.3vw,20px); font-weight:700; letter-spacing:.01em;
    color:var(--eu); text-transform:uppercase; line-height:1.15}
  .centre p{margin:3px 0 0; font-size:11.5px; color:var(--ink-3)}
  @media (max-width:700px){
    .masthead-bar{grid-template-columns:1fr; text-align:center; gap:12px}
    .org, .org.right{justify-self:center; flex-direction:row; text-align:left}
  }
  .periodline{display:flex; justify-content:space-between; flex-wrap:wrap; gap:8px;
    padding:0 4px}

  .metric{background:var(--inset); border:1px solid var(--rule-2); border-radius:7px;
          padding:12px 14px; display:flex; flex-direction:column; gap:3px}
  .metric .big{font-family:Archivo,sans-serif; font-size:clamp(26px,4vw,34px);
               font-weight:700; letter-spacing:-.025em; line-height:1;
               font-variant-numeric:tabular-nums}
  .metric .foot{font-size:12.5px; color:var(--ink-3)}
  .delta{display:inline-flex; align-items:baseline; gap:2px; font-family:Archivo,sans-serif;
         font-weight:600; font-size:12.5px; font-variant-numeric:tabular-nums;
         padding:1px 6px; border-radius:4px; white-space:nowrap}
  .up{color:var(--br); background:var(--br-soft)}
  .down{color:var(--signal); background:var(--signal-soft)}
  .flat{color:var(--ink-3); background:var(--surface-2)}

  .mapbox{background:var(--inset); border:1px solid var(--rule-2); border-radius:7px;
          padding:8px; display:flex; align-items:center; justify-content:center; min-height:210px}
  .mapbox svg{width:100%; height:auto; max-height:250px; display:block}
  .p-hub{fill:var(--br); opacity:.95}
  .p-mid{fill:var(--br-2); opacity:.72}
  .p-sm{fill:var(--br-3); opacity:.42}

  .rows{display:flex; flex-direction:column; gap:7px}
  .row{display:grid; grid-template-columns:1fr auto; gap:4px 10px; align-items:center}
  .rname{font-size:13px; color:var(--ink-2)}
  .rval{display:flex; align-items:baseline; gap:7px; justify-content:flex-end;
        font-family:Archivo,sans-serif; font-variant-numeric:tabular-nums}
  .rpct{font-size:13.5px; font-weight:700; min-width:42px; text-align:right}
  .track{grid-column:1 / -1; display:flex; flex-direction:column; gap:2px}
  .bar{height:9px; border-radius:2px; background:var(--br); min-width:2px}
  .bar.eu{background:var(--eu)}
  .bar.r{height:4px; background:var(--ref-soft); border-left:2px solid var(--ref)}

  .empty{flex:1; display:flex; flex-direction:column; align-items:center; justify-content:center;
         gap:7px; text-align:center; padding:26px 16px; color:var(--ink-3); min-height:150px}
  .empty .mark{width:34px; height:34px; border-radius:50%; border:1.5px dashed var(--rule);
               display:flex; align-items:center; justify-content:center;
               font-family:Archivo,sans-serif; font-weight:700; color:var(--ink-3); font-size:15px}
  .empty b{font-family:Archivo,sans-serif; font-size:13px; font-weight:600; color:var(--ink-2)}
  .empty span{font-size:12.5px; max-width:30ch}

  .band{background:var(--surface); border:1px solid var(--rule); border-radius:9px;
        box-shadow:var(--shadow); padding:16px clamp(12px,2vw,20px);
        display:flex; flex-direction:column; gap:14px}
  .bhead{display:flex; flex-wrap:wrap; justify-content:space-between; align-items:baseline; gap:8px;
         border-bottom:1px solid var(--rule-2); padding-bottom:9px}
  h2{font-family:Archivo,sans-serif; font-size:12px; font-weight:700;
     letter-spacing:.13em; text-transform:uppercase; color:var(--ink)}
  .note{font-size:12.5px; color:var(--ink-2); margin:0; max-width:80ch}

  .donutwrap{display:flex; align-items:center; gap:16px; flex-wrap:wrap}
  .donut{position:relative; flex-shrink:0; width:126px; height:126px}
  .donut svg{width:126px; height:126px; display:block}
  .donut .mid{position:absolute; inset:0; display:flex; flex-direction:column;
              align-items:center; justify-content:center; gap:0; pointer-events:none}
  .donut .mid b{font-family:Archivo,sans-serif; font-size:21px; font-weight:700;
                letter-spacing:-.02em; font-variant-numeric:tabular-nums}
  .donut .mid span{font-size:10px; color:var(--ink-3); font-family:Archivo,sans-serif;
                   letter-spacing:.09em; text-transform:uppercase}
  .dkey{display:flex; flex-direction:column; gap:5px; flex:1; min-width:180px}
  .dk{display:grid; grid-template-columns:auto 1fr auto auto; gap:8px; align-items:center; font-size:12.5px}
  .sw{width:9px; height:9px; border-radius:2px; flex-shrink:0}
  .dk .v{font-family:Archivo,sans-serif; font-weight:700; font-variant-numeric:tabular-nums}
  .dk .n{color:var(--ink-3); font-family:"IBM Plex Mono",monospace; font-size:11.5px}

  .tscroll{overflow-x:auto; margin:0 -3px; padding:0 3px}
  table{border-collapse:collapse; width:100%; font-size:13px; min-width:500px}
  th{font-family:Archivo,sans-serif; font-size:10px; font-weight:600; letter-spacing:.11em;
     text-transform:uppercase; color:var(--ink-3); text-align:left;
     padding:0 9px 6px; border-bottom:1px solid var(--rule); white-space:nowrap}
  td{padding:7px 9px; border-bottom:1px solid var(--rule-2)}
  tr:last-child td{border-bottom:none}
  .r-al{text-align:right}
  .code{font-size:12px; font-weight:500; letter-spacing:.02em}
  .place{font-size:11.5px; color:var(--ink-3)}

  .caveat{background:var(--surface-2); border:1px solid var(--rule-2);
          border-left:3px solid var(--signal); border-radius:0 6px 6px 0;
          padding:11px 13px; font-size:12.5px; color:var(--ink-2)}
  .caveat b{color:var(--ink)}
  footer{background:var(--ink); color:var(--ground); border-radius:9px;
         padding:14px 18px; font-size:12px; display:flex; flex-direction:column; gap:5px}
  footer b{font-family:Archivo,sans-serif; letter-spacing:.06em; text-transform:uppercase; font-size:11px}
  footer .mono{color:var(--ground); opacity:.82}
</style>'

# =============================================================================
# totalbr_panel_render(year, ref_year) -- build the page
#
#   totalbr_panel_render(2026, 2025)                  # jan-jun, both years
#   totalbr_panel_render(2026, 2025, months = 1:3)    # a quarter
#   totalbr_panel_render(2026, NULL)                  # one year, no comparison
#
# `ref_year` is the year the changes are measured against. NULL drops every
# comparison rather than showing zeroes: a panel with a "+0.0%" beside each
# figure reads as "no change measured", which is not the same as "not measured".
#
# The reference year is read from whatever holds it. The month parts only exist
# from 2026; earlier years live in the parquet archive, so `ref_src` takes a
# frame (or a path) when the parts are not there -- totalbr_daio() on the
# archive, sliced to the year, is the usual thing to hand it.
# =============================================================================
totalbr_panel_render <- function(year = 2026, ref_year = 2025, months = 1:6,
                                 feed = "cgna", d = NULL, ref_d = NULL,
                                 out_dir = TOTALBR_OUT_DIR, file = NULL,
                                 logo_left = getOption("totalbr.logo.decea"),
                                 logo_right = getOption("totalbr.logo.eurocontrol"),
                                 quiet = FALSE) {
  if (!is.null(logo_left) || !is.null(logo_right))
    if (!requireNamespace("base64enc", quietly = TRUE))
      stop("Embedding a logo needs the 'base64enc' package, or pass no logo and ",
           "the header renders a marked slot instead.")
  if (is.null(d)) d <- totalbr_panel_load(year, months, feed, quiet = TRUE)
  p <- totalbr_panel(year, months, feed, d = d, quiet = quiet)

  ref <- NULL
  if (!is.null(ref_year)) {
    if (is.null(ref_d))
      ref_d <- totalbr_panel_year(ref_year, months, feed, quiet = quiet)
    ref <- totalbr_panel(ref_year, months, feed, d = ref_d, quiet = TRUE)
  }

  # ---- the figures, each looked up rather than typed -----------------------
  tot  <- p$total$FLIGHTS[p$total$MONTH == "TOTAL"]
  rtot <- if (is.null(ref)) NA_real_ else
            ref$total$FLIGHTS[ref$total$MONTH == "TOTAL"]

  pick <- function(tbl, key, col, val, out = "FLIGHTS") {
    hit <- tbl[[out]][tbl[[col]] == val]
    if (length(hit) == 0) NA_real_ else hit[1]
  }
  dom  <- pick(p$daio, , "CLASS", "Regional")
  rdom <- if (is.null(ref)) NA_real_ else pick(ref$daio, , "CLASS", "Regional")
  intl  <- tot  - dom
  rintl <- if (is.null(ref)) NA_real_ else rtot - rdom

  # regions and countries, each joined to the reference year by name
  reg <- data.table::as.data.table(p$regions)[, .(REGION, FLIGHTS, PCT)]
  reg[, BEFORE := if (is.null(ref)) NA_real_ else
        vapply(REGION, function(r) pick(ref$regions, , "REGION", r), numeric(1))]
  reg <- utils::head(reg[order(-FLIGHTS)], 6)
  reg[, REGION := totalbr_region_name(REGION)]

  ctry_of <- function(pp, rg, n = 6) {
    x <- data.table::as.data.table(totalbr_panel_countries(
           data.table::as.data.table(if (identical(pp, p)) d else ref_d), region = rg))
    utils::head(x[order(-FLIGHTS)], n)
  }
  sa  <- ctry_of(p, "South America"); eu <- ctry_of(p, "Europe")
  join_ref <- function(x, rg) {
    if (is.null(ref)) { x[, BEFORE := NA_real_]; return(x) }
    r <- data.table::as.data.table(totalbr_panel_countries(
           data.table::as.data.table(ref_d), region = rg))
    x[, BEFORE := r$FLIGHTS[match(ISO, r$ISO)]][]
  }
  sa <- join_ref(sa, "South America"); eu <- join_ref(eu, "Europe")
  sa[, NAME := totalbr_country_name(ISO)]; eu[, NAME := totalbr_country_name(ISO)]

  # routes to Europe, both years
  rt <- data.table::as.data.table(totalbr_panel_citypairs(
          data.table::as.data.table(d), region = "Europe", n = 8))
  if (!is.null(ref)) {
    rr <- data.table::as.data.table(totalbr_panel_citypairs(
            data.table::as.data.table(ref_d), region = "Europe", n = 400))
    rt[, BEFORE := rr$FLIGHTS[match(paste(BR, FAR), paste(rr$BR, rr$FAR))]]
  } else rt[, BEFORE := NA_real_]

  route_rows <- paste(vapply(seq_len(nrow(rt)), function(i) sprintf(
    '<tr><td><span class="mono code">%s</span> <span class="place">%s</span></td><td><span class="mono code">%s</span> <span class="place">%s</span></td><td class="r-al mono">%s</td><td class="r-al mono">%s</td><td class="r-al">%s</td></tr>',
    rt$BR[i], .tb_esc(totalbr_aerodrome_name(rt$BR[i])),
    rt$FAR[i], .tb_esc(totalbr_aerodrome_name(rt$FAR[i])),
    .tb_n(rt$FLIGHTS[i]),
    if (is.na(rt$BEFORE[i])) "&mdash;" else .tb_n(rt$BEFORE[i]),
    .tb_delta(rt$FLIGHTS[i], rt$BEFORE[i])), character(1)), collapse = "\n")

  # the four DAIO classes, in the panel's order
  dl  <- c("Regional", "Departures", "Arrivals", "Overflights")
  dpt <- c("Regional", "Departures", "Arrivals", "Overflights")
  cols <- c("var(--br)", "var(--br-2)", "var(--br-3)", "var(--signal)")
  dn  <- vapply(dl, function(k) pick(p$daio, , "CLASS", k), numeric(1))
  dp  <- vapply(dl, function(k) pick(p$daio, , "CLASS", k, out = "PCT"), numeric(1))
  dkey <- paste(sprintf(
    '<div class="dk"><i class="sw" style="background:%s"></i><span>%s</span><span class="n">%s</span><span class="v">%s</span></div>',
    cols, dpt, .tb_n(dn), .tb_pct(dp)), collapse = "\n")

  per <- if (length(months) == 1) sprintf("%s %d", toupper(month.abb[months]), year)
         else sprintf("%s–%s %d", toupper(month.abb[min(months)]),
                      toupper(month.abb[max(months)]), year)

  # TOKENS, NOT sprintf. The template is full of literal per-cent signs -- every
  # CSS width, every figure in the prose -- and sprintf would read each one as a
  # format specifier and fail, or worse, silently consume the wrong argument.
  # Fixed-string replacement has no such reading of the text.
  fill <- c(
    "{{TITLE}}"     = sprintf("Brazil and Europe Network %d%s", year,
                              ref_year_label(ref_year)),
    "{{PERIOD}}"    = per,
    "{{TOTAL}}"     = .tb_n(tot),
    "{{TOTAL_SUB}}" = if (is.null(ref)) "in the period" else
                        sprintf("%s on %s in %d", .tb_delta(tot, rtot),
                                .tb_n(rtot), ref_year),
    "{{DOTS}}"      = totalbr_panel_dots(d),
    "{{REGIONS}}"   = .tb_rows(reg, "REGION"),
    "{{DONUT}}"     = .tb_donut(dp, cols),
    "{{DOM_PCT}}"   = .tb_pct(dp[1]),
    "{{DKEY}}"      = dkey,
    "{{INTL}}"      = .tb_n(intl),
    "{{INTL_DELTA}}"= .tb_delta(intl, rintl),
    "{{ROUTES}}"    = route_rows,
    "{{SA}}"        = .tb_rows(sa, "NAME"),
    "{{EU}}"        = .tb_rows(eu, "NAME", "eu"),
    "{{GRAND}}"     = .tb_n(tot + (if (is.null(ref)) 0 else rtot)),
    "{{YEAR}}"      = as.character(year),
    "{{REFYEAR}}"   = if (is.null(ref_year)) "&mdash;" else as.character(ref_year),
    "{{STAMP}}"     = format(Sys.Date(), "%d %b %Y"),
    "{{FLAG_BR}}"   = TOTALBR_FLAG_BR,
    "{{FLAG_EU}}"   = TOTALBR_FLAG_EU,
    "{{LOGO_L}}"    = .tb_logo(logo_left,  "DECEA logo"),
    "{{LOGO_R}}"    = .tb_logo(logo_right, "EUROCONTROL logo"),
    "{{CSS}}"       = TOTALBR_PANEL_CSS)

  html <- TOTALBR_PANEL_TEMPLATE
  for (k in names(fill)) html <- gsub(k, fill[[k]], html, fixed = TRUE)

  left <- regmatches(html, gregexpr("\\{\\{[A-Z_]+\\}\\}", html))[[1]]
  if (length(left) > 0)
    warning("Template token(s) never filled: ", paste(unique(left), collapse = ", "))

  # A SINGLE MONTH IS NAMED BY THAT MONTH. "painel-cgna-2026-07-07" reads as a
  # range whose ends happen to coincide, and a folder of monthly panels named
  # that way is harder to scan than one named for the months themselves.
  if (is.null(file)) {
    span <- if (length(months) == 1) sprintf("%d-%02d", year, months)
            else sprintf("%d-%02d-%02d", year, min(months), max(months))
    file <- file.path(out_dir, sprintf("painel-%s-%s.html", feed, span))
  }
  if (!dir.exists(dirname(file))) dir.create(dirname(file), recursive = TRUE)
  writeLines(html, file, useBytes = TRUE)
  if (!quiet) message("Wrote ", file)
  invisible(file)
}

ref_year_label <- function(y) if (is.null(y)) "" else sprintf(" vs %d", y)

# ---- names -------------------------------------------------------------------
# Only what the panel shows. A full ISO table is not the job of this file, and a
# code with no name here falls back to the code itself rather than to "NA".
# TOTALBR_REGIONS is already keyed in English, so the region names need no
# translation layer at all -- they are displayed as they are stored.
totalbr_region_name <- function(rg) rg

# Only the countries the panel actually shows. A full ISO table is not this
# file's job, and a code with no name here falls back to the CODE rather than to
# "NA" -- an unnamed country should still be identifiable.
TOTALBR_COUNTRY_EN <- c(
  AR="Argentina", CL="Chile", CO="Colombia", UY="Uruguay", PE="Peru",
  PY="Paraguay", BO="Bolivia", EC="Ecuador", VE="Venezuela", SR="Suriname",
  GY="Guyana", GF="French Guiana", US="United States", CA="Canada",
  PT="Portugal", ES="Spain", FR="France", IT="Italy", DE="Germany",
  GB="United Kingdom", NL="Netherlands", CH="Switzerland", PA="Panama",
  MX="Mexico", DO="Dominican Rep.", CV="Cape Verde", ZA="South Africa",
  AO="Angola", TR="Türkiye", QA="Qatar", AE="United Arab Emirates",
  BE="Belgium", IE="Ireland", AT="Austria", GR="Greece", MA="Morocco",
  NG="Nigeria", ET="Ethiopia", SN="Senegal", CU="Cuba", BS="Bahamas")
totalbr_country_name <- function(iso) {
  out <- unname(TOTALBR_COUNTRY_EN[iso]); ifelse(is.na(out), iso, out)
}

TOTALBR_AERODROME_PT <- c(
  SBGR="Guarulhos", SBSP="Congonhas", SBGL="Galeão", SBKP="Viracopos",
  SBBR="Brasília", SBCF="Confins", SBSV="Salvador", SBRF="Recife",
  SBFZ="Fortaleza", SBPA="Porto Alegre", SBCT="Curitiba", SBFL="Florianópolis",
  LEMD="Madri", LPPT="Lisboa", LFPG="Paris CDG", LIRF="Roma Fiumicino",
  EGLL="Londres Heathrow", EDDF="Frankfurt", EHAM="Amsterdã", LSZH="Zurique",
  LIMC="Milão", SABE="Aeroparque", SAEZ="Ezeiza", SCEL="Santiago",
  SKBO="Bogotá", SPJC="Lima", KMIA="Miami", MPTO="Tocumen", SUMU="Montevidéu")
totalbr_aerodrome_name <- function(icao) {
  out <- unname(TOTALBR_AERODROME_PT[icao]); ifelse(is.na(out), "", out)
}

# ---- the page ----------------------------------------------------------------
# Markers, not code: every figure arrives from totalbr_panel(). Editing the
# wording here is safe; the numbers cannot be edited here at all, which is the
# point.
TOTALBR_PANEL_TEMPLATE <- '<title>{{TITLE}}</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@500;600;700&family=Source+Sans+3:wght@400;600&family=IBM+Plex+Mono:wght@400;500&display=swap">
{{CSS}}
<div class="wrap">
  <div class="masthead-bar">
    <div class="org">
      {{LOGO_L}}
      <span class="stack">
        <span class="name">DECEA</span>
        <span class="tag">Department of Airspace Control &ndash; Brazil</span>
      </span>
    </div>
    <div class="centre">
      <h1>Brazil and Europe Network</h1>
      <p>How Brazil and Europe connect with global air traffic</p>
    </div>
    <div class="org right">
      {{LOGO_R}}
      <span class="stack">
        <span class="name">EUROCONTROL</span>
        <span class="tag">Supporting European Aviation</span>
      </span>
    </div>
  </div>
  <div class="periodline">
    <span class="lbl">{{PERIOD}} &middot; compared with {{REFYEAR}}</span>
    <span class="lbl">Source: CGNA</span>
  </div>

  <div class="duo">
    <div class="card">
      <span class="badge">{{FLAG_BR}}Brazil</span>
      <div class="metric">
        <span class="lbl">Total flights &middot; {{PERIOD}}</span>
        <span class="big">{{TOTAL}}</span>
        <span class="foot">{{TOTAL_SUB}}</span>
      </div>
      <div class="mapbox">
        <svg viewBox="0 0 300 330" role="img" aria-label="Brazilian aerodromes, sized by movements">
{{DOTS}}
        </svg>
      </div>
      <div>
        <span class="lbl">Share of mapped external departures</span>
        <div class="rows" style="margin-top:9px">
{{REGIONS}}
        </div>
      </div>
    </div>

    <div class="card pending">
      <span class="badge" style="opacity:.5">{{FLAG_EU}}Europe</span>
      <div class="empty"><span class="mark">EU</span><b>Reserved for EUROCONTROL</b><span>The same cut &mdash; total flights, aerodromes on the map and external departures by region &mdash; once the PRU figures arrive.</span></div>
    </div>
  </div>

  <div class="band">
    <div class="bhead">
      <h2>Traffic distribution within region</h2>
      <span class="lbl">DAIO classification &middot; {{YEAR}}</span>
    </div>
    <div class="duo">
      <div class="donutwrap">
        <div class="donut">{{DONUT}}<span class="mid"><b>{{DOM_PCT}}</b><span>Regional</span></span></div>
        <div class="dkey">
{{DKEY}}
          <div class="dk" style="border-top:1px solid var(--rule-2); padding-top:5px; margin-top:2px">
            <i class="sw" style="background:transparent"></i><span style="color:var(--ink-3)">International</span>
            <span class="n">{{INTL}}</span><span class="v">{{INTL_DELTA}}</span>
          </div>
        </div>
      </div>
      <div class="card pending" style="box-shadow:none; border-radius:7px; padding:12px">
        <div class="empty" style="min-height:126px"><span class="mark">EU</span><b>Same chart, PRU figures</b><span>Regional &middot; departures &middot; arrivals &middot; overflights</span></div>
      </div>
    </div>
  </div>

  <div class="band">
    <div class="bhead">
      <h2>Top city-pair connections Brazil &harr; Europe</h2>
      <span class="lbl">Aerodrome pair &middot; flights in the period</span>
    </div>
    <div class="tscroll">
      <table>
        <thead><tr><th>Brasil</th><th>Europa</th><th class="r-al">{{YEAR}}</th><th class="r-al">{{REFYEAR}}</th><th class="r-al">Change</th></tr></thead>
        <tbody>
{{ROUTES}}
        </tbody>
      </table>
    </div>
  </div>

  <div class="band">
    <div class="bhead">
      <h2>Main country-level connections</h2>
      <span class="lbl">Arrivals + departures &middot; {{YEAR}}</span>
    </div>
    <div class="duo">
      <div>
        <span class="lbl">Brazil to South America</span>
        <div class="rows" style="margin-top:10px">
{{SA}}
        </div>
      </div>
      <div>
        <span class="lbl">Brazil to Europe</span>
        <div class="rows" style="margin-top:10px">
{{EU}}
        </div>
      </div>
    </div>
  </div>

  <div class="band">
    <div class="bhead"><h2>Method</h2></div>
    <p class="note"><b>The DAIO classification</b> reads each flight by its two ends: <b>regional</b> with both in Brazil, <b>arrival</b> or <b>departure</b> with one, <b>overflight</b> with neither. Countries come from the OurAirports database plus a list this project keeps for what it lacks. The region and country blocks describe the <b>foreign end</b>, so an arrival and a departure to the same country count together; overflights have two foreign ends and no basis for picking one, so they are in the international total but not in the per-country counts.</p>
    <p class="note">The faint bar behind each value is the same period of the reference year. Changes under 2% are rendered grey: at that size the difference does not separate from rounding. A &ldquo;city pair&rdquo; here is an <b>aerodrome pair</b> &mdash; SBGR and SBSP are both S&atilde;o Paulo &mdash; because merging them is the reader&rsquo;s decision, not this page&rsquo;s.</p>
  </div>

  <footer>
    <div><b>Source</b></div>
    <div>CGNA &mdash; the same source for both periods. The period is defined by each flight&rsquo;s <span class="mono">dt_dia</span>, not by how the source file was sliced.</div>
    <div>{{GRAND}} flights classified &middot; generated {{STAMP}} by <span class="mono">TOTALBR/render_totalbr_panel.R</span> &middot; the European column awaits EUROCONTROL PRU figures.</div>
  </footer>
</div>
'

# ---- run as a script ---------------------------------------------------------
# Rscript TOTALBR/render_totalbr_panel.R                # 2026 vs 2025, jan-jun
# Rscript TOTALBR/render_totalbr_panel.R 2026 2025      # the same, said out loud
# Rscript TOTALBR/render_totalbr_panel.R 2026 2025 7    # JULY ALONE, vs July 2025
# Rscript TOTALBR/render_totalbr_panel.R 2026 2025 1-6  # a range
# Rscript TOTALBR/render_totalbr_panel.R 2026 none 7    # one month, no comparison
#
# A bare number is THAT MONTH, not "the first N months". The earlier reading
# made a monthly panel impossible to ask for from here, and "7" meaning
# January-to-July is not what anyone types it for.
#
# sys.nframe() is 0 only when this file is the script being run, so sourcing it
# from an R session does nothing here.
# "7" -> 7; "1-6" -> 1:6. Anything else stops rather than guessing, because a
# misread month argument produces a panel that is wrong about its own period and
# says so nowhere.
.tb_months <- function(x) {
  if (grepl("^[0-9]{1,2}$", x)) {
    m <- as.integer(x)
  } else if (grepl("^[0-9]{1,2}-[0-9]{1,2}$", x)) {
    p <- as.integer(strsplit(x, "-", fixed = TRUE)[[1]]); m <- p[1]:p[2]
  } else {
    stop("Month argument must be a month (7) or a range (1-6), not '", x, "'.")
  }
  if (any(m < 1 | m > 12)) stop("Month out of range in '", x, "'.")
  m
}

if (sys.nframe() == 0L) {
  a  <- commandArgs(trailingOnly = TRUE)
  yr <- if (length(a) >= 1) as.integer(a[1]) else 2026
  rf <- if (length(a) >= 2) (if (tolower(a[2]) %in% c("none", "na", "-")) NULL
                             else as.integer(a[2])) else yr - 1L
  mo <- if (length(a) >= 3) .tb_months(a[3]) else 1:6
  f  <- totalbr_panel_render(yr, rf, months = mo)
  message("Open it with:  open ", f)
}
