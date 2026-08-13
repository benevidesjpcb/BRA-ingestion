#!/usr/bin/env Rscript
# =============================================================================
# setup_renviron.R
#
# Creates the project's .Renviron on a new machine, with every variable this
# repository reads already written out — so the only thing left to do is paste
# the TATIC token.
#
#   source(here::here("setup_renviron.R"))
#   setup_renviron()        # write/update .Renviron with all known variables
#   set_token()             # paste the TATIC token, without it going anywhere else
#   env_status()            # what is set right now (never prints a token)
#
# .Renviron is git-ignored and stays that way: it is the one file in this
# project that holds a secret, and the whole point of keeping it out of the
# repository is lost if a token is ever written into a script, a commit, or a
# chat window.
#
# WHY A PROJECT .Renviron AND NOT THE HOME ONE
# R reads the project's .Renviron when the project is opened, and when it exists
# it takes the place of ~/.Renviron entirely — the user-level one is NOT also
# read. Keeping everything in the project file means one place to look, and it
# travels with the project directory on the other machine.
#
# CHANGES ONLY TAKE EFFECT AFTER A RESTART. R reads .Renviron once, at startup.
# =============================================================================

# ---- the variables this repository reads ------------------------------------
# value = "" means "you must fill this in"; anything else is a working default.
# The comment is written above the line, so the file explains itself on the
# machine where it lives.
RENVIRON_VARS <- list(
  list(name = "TATIC_TOKEN", value = "",
       note = c("CGNA/TATIC API token. REQUIRED for download_tatic().",
                "Paste it after the '=' with no quotes and no spaces.",
                "Never commit this file; never paste this token into a script.")),
  list(name = "ODIN_PAGE_SIZE", value = "10000",
       note = c("Rows per ODIN request. Raise it (e.g. 50000) for a faster",
                "download; the engine reports if the server caps it lower.")),
  list(name = "ODIN_TOKEN", value = "",
       note = c("Not needed today: the ODIN API is open. Kept here for the day",
                "it is not.")),
  list(name = "BRA_PROXY", value = "",
       note = c("Corporate proxy for BOTH APIs, e.g. http://proxy.example.int:9512.",
                "On Windows, find it with: netsh winhttp show proxy. Only needed",
                "when the network has one AND https_proxy is not already set.",
                "Symptom of a missing/unauthenticated proxy: the API works in the",
                "browser but R fails with 'CONNECT tunnel failed, response 407'.")),
  list(name = "BRA_PROXY_AUTH", value = "",
       note = c("How to authenticate to that proxy: basic, digest, negotiate,",
                "ntlm or any. On a Windows domain machine try ntlm first.")),
  list(name = "BRA_PROXY_USERPWD", value = "",
       note = c("'user:password' for the proxy, or just ':' on Windows to reuse",
                "the logged-in account's credentials - which keeps any password",
                "out of this file entirely. Prefer ':'.")),
  list(name = "BRA_TAXI_ZIP", value = "",
       note = c("Optional: path to a zip holding dsTaxiYYYY.csv, when reading",
                "the taxi source from an archive instead of data-raw/dstaxi/.")),
  list(name = "BRA_TOTALBR_PARQUET", value = "",
       note = c("Optional: path to the ~1 GB TOTALBR parquet archive when it is",
                "kept outside the repository.")),
  list(name = "BRA_REPORT_DATA", value = "",
       note = c("Optional: data directory of the sibling report project that",
                "receives the combined analytic CSVs. Defaults to data/."))
)

renviron_path <- function() here::here(".Renviron")

# ---- read an existing file into name -> line ---------------------------------
.renviron_read <- function(path) {
  if (!file.exists(path)) return(character(0))
  ln <- readLines(path, warn = FALSE)
  keep <- grepl("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*=", ln)
  nm <- sub("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*=.*$", "\\1", ln[keep])
  stats::setNames(ln[keep], nm)
}

# =============================================================================
# setup_renviron(path, overwrite)
#
# Writes the variables above into .Renviron. Existing entries are LEFT ALONE:
# only the missing ones are added, so running this twice never destroys a token
# or a path you already set. overwrite = TRUE rewrites the file from the
# template, after saving the current one alongside with a timestamp.
# =============================================================================
setup_renviron <- function(path = renviron_path(), overwrite = FALSE) {

  existing <- .renviron_read(path)

  if (file.exists(path) && overwrite) {
    backup <- paste0(path, ".bak-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    file.copy(path, backup)
    message("Saved the previous file as ", basename(backup))
    existing <- character(0)
  }

  add <- Filter(function(v) !(v$name %in% names(existing)), RENVIRON_VARS)

  if (length(add) == 0) {
    message("Nothing to add: ", path, " already has all ",
            length(RENVIRON_VARS), " variables.")
    return(invisible(env_status(quiet = TRUE)))
  }

  header <- if (file.exists(path) && !overwrite) character(0) else c(
    "# .Renviron - environment for the BRA-ingestion project.",
    "# Git-ignored on purpose: it holds the TATIC token.",
    "# R reads this file ONCE, at startup: restart R after editing it.",
    "")

  lines <- unlist(lapply(add, function(v) c(
    paste0("# ", v$note),
    paste0(v$name, "=", v$value),
    ""
  )))

  con <- file(path, open = if (file.exists(path) && !overwrite) "a" else "w")
  on.exit(close(con))
  writeLines(c(header, lines), con)

  message(if (length(existing) > 0) "Updated " else "Created ", path)
  message("Added: ", paste(vapply(add, function(v) v$name, character(1)),
                           collapse = ", "))
  message("\nNext: put the token in, then restart R.",
          "\n  set_token()        # prompts for it, writes it here, shows nothing",
          "\nor edit the file by hand: the TATIC_TOKEN= line.")

  invisible(path)
}

# =============================================================================
# set_token(name, path)
#
# Asks for the token at the console and writes it into .Renviron. Interactive on
# purpose: a token typed at a prompt does not end up in .Rhistory, in a script,
# or in a commit — which is exactly what happens when it is assigned in code.
# The value is never printed back.
# =============================================================================
set_token <- function(name = "TATIC_TOKEN", path = renviron_path()) {
  if (!interactive())
    stop("set_token() needs an interactive session. Edit ", path, " by hand instead.")

  token <- trimws(readline(paste0("Paste the value for ", name, " (it will not be shown): ")))
  if (!nzchar(token)) { message("Nothing entered; ", name, " unchanged."); return(invisible(NULL)) }
  if (grepl("[\"' ]", token))
    warning("The value contains a quote or a space. .Renviron takes the raw value, ",
            "with no quotes.", call. = FALSE, immediate. = TRUE)

  ln <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
  hit <- grepl(paste0("^\\s*", name, "\\s*="), ln)
  new <- paste0(name, "=", token)
  if (any(hit)) ln[which(hit)[1]] <- new else ln <- c(ln, new)
  writeLines(ln, path)

  # 0600 where the OS supports it: the file holds a secret and does not need to
  # be readable by other accounts on a shared machine.
  try(Sys.chmod(path, mode = "0600"), silent = TRUE)

  message(name, " written to ", path, " (", nchar(token), " characters).")
  message("RESTART R for it to take effect - .Renviron is read at startup only.")
  invisible(TRUE)
}

# =============================================================================
# env_status() — what is set in THIS session
#
# Reports whether each variable has a value and how long it is. It never prints
# a token: the length is enough to tell "set" from "set to something truncated",
# which is the only question worth asking without exposing the value.
# =============================================================================
env_status <- function(quiet = FALSE) {
  out <- do.call(rbind, lapply(RENVIRON_VARS, function(v) {
    val <- Sys.getenv(v$name, unset = "")
    secret <- grepl("TOKEN", v$name)
    data.frame(
      VARIABLE = v$name,
      SET      = nzchar(val),
      VALUE    = if (!nzchar(val)) "" else if (secret) paste0("<", nchar(val), " chars>") else val,
      stringsAsFactors = FALSE
    )
  }))
  if (!quiet) {
    print(out, row.names = FALSE)
    if (!nzchar(Sys.getenv("TATIC_TOKEN", unset = "")))
      message("\nTATIC_TOKEN is empty in this session. If you just edited ",
              ".Renviron, restart R - it is read at startup only.")
  }
  invisible(out)
}

# ---- keep the secret out of git ---------------------------------------------
# Belt and braces: the repository already ignores .Renviron, but a fresh clone
# somewhere else, or a copied project folder, may not.
ensure_gitignored <- function(path = here::here(".gitignore")) {
  ln <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
  if (any(trimws(ln) == ".Renviron")) return(invisible(FALSE))
  writeLines(c(ln, "", "# Secrets - holds TATIC_TOKEN; must never be committed",
               ".Renviron"), path)
  message("Added .Renviron to ", path)
  invisible(TRUE)
}

# ---- run only when executed as a script (not when sourced) ------------------
if (sys.nframe() == 0L) {
  setup_renviron()
  ensure_gitignored()
}
