# =============================================================================
# proxy.R
#
# Corporate proxy settings for every outbound request this project makes — the
# ODIN API and the CGNA/TATIC API alike. Sourced by both downloaders.
#
# THE SYMPTOM
#   The API works in a browser, and R fails with
#     CONNECT tunnel failed, response 407
#   or the less obvious
#     Failure when receiving data from the peer
#
# THE CAUSE
#   On a network where everything external goes through a proxy, an HTTPS
#   request starts by asking the proxy for a tunnel (CONNECT). The proxy answers
#   407 — "identify yourself". A browser does that automatically with the
#   logged-in account and never mentions it; curl, under httr2, does not try at
#   all unless told how. The failure then surfaces as a transport error, which
#   looks like the API being down rather than the proxy refusing.
#
# THE SETTINGS
#   BRA_PROXY          proxy URL, e.g. http://proxy.example.int:9512
#                      (find it on Windows with: netsh winhttp show proxy)
#   BRA_PROXY_AUTH     basic | digest | negotiate | ntlm | any
#   BRA_PROXY_USERPWD  "user:password", or ":" to authenticate as the logged-in
#                      Windows account through SSPI — no password stored anywhere
#
#   ODIN_PROXY / ODIN_PROXY_AUTH / ODIN_PROXY_USERPWD are accepted as aliases,
#   because that is what they were called when this was ODIN-only.
#
# Nothing set = no proxy options are added, and curl still honours the standard
# https_proxy / http_proxy variables on its own. A proxy that needs no
# authentication therefore needs nothing here.
# =============================================================================

.bra_env <- function(primary, alias) {
  v <- Sys.getenv(primary, unset = "")
  if (nzchar(v)) return(v)
  Sys.getenv(alias, unset = "")
}

bra_proxy <- function(req) {
  px   <- .bra_env("BRA_PROXY",         "ODIN_PROXY")
  auth <- tolower(.bra_env("BRA_PROXY_AUTH", "ODIN_PROXY_AUTH"))
  pwd  <- .bra_env("BRA_PROXY_USERPWD", "ODIN_PROXY_USERPWD")
  if (!nzchar(px) && !nzchar(auth) && !nzchar(pwd)) return(req)

  opts <- list()
  if (nzchar(px))  opts$proxy        <- px
  if (nzchar(pwd)) opts$proxyuserpwd <- pwd
  # CURLAUTH_* bit values, as libcurl defines them
  code <- switch(auth,
                 basic     = 1L,
                 digest    = 2L,
                 negotiate = 4L,
                 ntlm      = 8L,
                 any       = -1L,     # let curl pick whatever the proxy offers
                 NULL)
  if (!is.null(code)) opts$proxyauth <- code
  if (length(opts) == 0) return(req)
  do.call(httr2::req_options, c(list(req), opts))
}
