# ==============================================================================
# ARCHIVO: api_client.R — Cliente HTTP para API-Football (v3)
# ==============================================================================
library(httr)
library(jsonlite)
source("config.R")

api_get <- function(endpoint, query = list(), max_retries = 3) {
  if (API_FOOTBALL_KEY == "") stop("API_FOOTBALL_KEY no configurada. Ver config.R.")

  url <- paste0(API_FOOTBALL_BASE_URL, "/", endpoint)

  intento <- 0
  repeat {
    intento <- intento + 1
    resp <- tryCatch({
      httr::GET(
        url,
        query = query,
        httr::add_headers(`x-apisports-key` = API_FOOTBALL_KEY)
      )
    }, error = function(e) NULL)

    Sys.sleep(API_FOOTBALL_DELAY_SEC)

    if (is.null(resp)) {
      if (intento >= max_retries) { warning("❌ Fallo de red tras reintentos: ", endpoint); return(NULL) }
      Sys.sleep(1); next
    }

    status <- httr::status_code(resp)

    if (status == 429) {
      warning("⏳ Rate limit alcanzado. Esperando 60s...")
      Sys.sleep(60)
      if (intento >= max_retries) return(NULL)
      next
    }

    if (status != 200) {
      warning(sprintf("❌ Error HTTP %d en %s", status, endpoint))
      return(NULL)
    }

    parsed <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
    if (is.null(parsed)) return(NULL)

    if (!is.null(parsed$errors) && length(parsed$errors) > 0) {
      warning(paste("❌ API-Football devolvió error:", paste(unlist(parsed$errors), collapse = "; ")))
      return(NULL)
    }

    remaining <- httr::headers(resp)[["x-ratelimit-requests-remaining"]]
    if (!is.null(remaining) && as.numeric(remaining) < 20) {
      message(sprintf("ℹ️ Quedan %s requests hoy.", remaining))
    }

    return(parsed)
  }
}
