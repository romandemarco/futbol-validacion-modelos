# ==============================================================================
# ARCHIVO: utils.R — Mensajería
# VERSIÓN: 4.3
#
# Eliminadas en esta versión: calcular_sd_empirico() y calcular_sd_partido().
# Devolvían el SD INCONDICIONAL de la columna, que mezcla la varianza del
# partido dado λ con la variación de λ entre partidos. Meter las dos en la
# binomial negativa inflaba la varianza y aplastaba las probabilidades ~2-3
# puntos hacia 0.5 — el tamaño exacto del edge buscado.
# Reemplazadas por prob_bajo() + k_mercado() en math_core.R, con k estimado
# por máxima verosimilitud sobre residuos walk-forward.
# ==============================================================================
library(dplyr)
source("config.R")

# Telegram opcional: si el paquete no está, el pipeline sigue igual
.tg_ok <- requireNamespace("telegram.bot", quietly = TRUE)
bot <- if (.tg_ok && TELEGRAM_TOKEN != "") telegram.bot::Bot(token = TELEGRAM_TOKEN) else NULL

enviar_telegram <- function(msg) {
  if (is.null(bot) || TELEGRAM_CHAT_ID == "") {
    message("ℹ️ Telegram no configurado. Mensaje no enviado.")
    return(invisible(NULL))
  }
  tryCatch({
    bot$sendMessage(chat_id = TELEGRAM_CHAT_ID, text = msg, parse_mode = "Markdown")
    cat("🚀 Enviado a Telegram.\n")
  }, error = function(e) cat("❌ Error Telegram:", conditionMessage(e), "\n"))
}

# ==============================================================================
# ligas_pais(): busca los IDs de liga de un país. Cuesta 1 request.
# Útil para el fútbol argentino, donde hay torneos separados
# (Liga Profesional, Copa de la Liga, Primera Nacional) con IDs distintos.
# ==============================================================================
ligas_pais <- function(pais = "Argentina") {
  resp <- api_get("leagues", list(country = pais))
  if (is.null(resp) || length(resp$response) == 0) {
    message("❌ Sin resultados para ", pais); return(invisible(NULL))
  }
  out <- do.call(rbind, lapply(resp$response, function(x) {
    temps <- x$seasons
    anios <- if (length(temps)) paste(range(sapply(temps, function(s) s$year)), collapse = "-") else NA
    data.frame(id = x$league$id, nombre = x$league$name,
               tipo = x$league$type, temporadas = anios, stringsAsFactors = FALSE)
  }))
  print(out, row.names = FALSE)
  invisible(out)
}