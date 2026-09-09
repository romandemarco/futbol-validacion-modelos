# ==============================================================================
# ARCHIVO: market_registry.R — Definición única de todos los mercados
# VERSIÓN: 4.0
#
# CAMPOS:
#   label          -> nombre mostrado
#   col_L / col_V  -> columnas del dataset (local / visita)
#   quality_adj    -> aplica calcular_factor_calidad()
#   keywords       -> regex para matchear el string del usuario
#   base_stability -> peso base del score
#   ajuste_ref     -> "FALTA"/"TARJETA" o NULL
#   poisson_weight -> mezcla Poisson vs Lineal
#   filtrar_rojas  -> NUEVO. Si TRUE, entrena excluyendo partidos con expulsión.
#                     DEBE ser FALSE en TARJETAS y FALTAS: HomeCards = HY + HR,
#                     así que filtrar rojas te saca justo la cola alta y
#                     sub-predice sistemáticamente.
#   k_disp_key     -> clave en K_DISPERSION (config.R)
# ==============================================================================

market_registry <- list(
  GOLES = list(
    label = "GOLES", col_L = "FTHG", col_V = "FTAG",
    quality_adj = FALSE, keywords = "gol",
    base_stability = NA,
    poisson_weight = 0.90,
    filtrar_rojas = FALSE
  ),
  REMATES = list(
    label = "REMATES", col_L = "HS", col_V = "AS",
    quality_adj = TRUE, keywords = "remate|tiro total|disparo",
    base_stability = 1.05,
    poisson_weight = 0.50,
    filtrar_rojas = FALSE
  ),
  ARCO = list(
    label = "TIROS ARCO", col_L = "HST", col_V = "AST",
    quality_adj = TRUE, keywords = "arco|puerta",
    base_stability = 1.00,
    poisson_weight = 0.60,
    filtrar_rojas = FALSE
  ),
  CORNERS = list(
    label = "CÓRNERS", col_L = "HC", col_V = "AC",
    quality_adj = TRUE, keywords = "corner|córner|esquina",
    base_stability = 0.98,
    poisson_weight = 0.60,
    filtrar_rojas = FALSE
  ),
  FALTAS = list(
    label = "FALTAS", col_L = "HF", col_V = "AF",
    quality_adj = FALSE, keywords = "falta",
    base_stability = 0.90, ajuste_ref = "FALTA",
    poisson_weight = 0.70,
    filtrar_rojas = FALSE   # <-- CRÍTICO: era TRUE de facto y sesgaba a la baja
  ),
  TARJETAS = list(
    label = "TARJETAS", col_L = "HomeCards", col_V = "AwayCards",
    quality_adj = FALSE, keywords = "tarjeta|amarilla|card",
    base_stability = 0.85, ajuste_ref = "TARJETA",
    poisson_weight = 0.85,
    filtrar_rojas = FALSE   # <-- CRÍTICO: HomeCards incluye HR
  ),
  ATAJADAS = list(
    label = "ATAJADAS", col_L = "HSAVES", col_V = "ASAVES",
    quality_adj = FALSE, keywords = "atajad|save|paradas",
    base_stability = 0.90,
    poisson_weight = 0.80,
    filtrar_rojas = FALSE
  ),
  OFFSIDE = list(
    label = "FUERA DE JUEGO", col_L = "HOFF", col_V = "AOFF",
    quality_adj = FALSE, keywords = "fuera de juego|offside|orsai",
    base_stability = 0.80,
    poisson_weight = 0.85,
    filtrar_rojas = FALSE
  ),
  PASES = list(
    label = "PASES", col_L = "HPASS", col_V = "APASS",
    quality_adj = FALSE, keywords = "pase",
    base_stability = 0.95,
    poisson_weight = 0.10,
    filtrar_rojas = FALSE
  )
)

# Helper: encontrar el mercado a partir del texto del usuario
match_mercado <- function(txt) {
  txt <- tolower(txt)
  for (mkey in names(market_registry)) {
    if (grepl(market_registry[[mkey]]$keywords, txt)) return(mkey)
  }
  NULL
}