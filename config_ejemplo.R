# ==============================================================================
# ARCHIVO: config_ejemplo.R — Plantilla de configuración
#
# Copiar como config.R y completar según la liga y los mercados que se quieran
# evaluar. Los parámetros de dispersión y calibración se obtienen corriendo el
# protocolo de validación (ver README).
#
# Claves de parámetros: LIGA_MERCADO_LADO
#   Ej: PL_REMATES_L = Premier League, remates, equipo local
# ==============================================================================

# ==============================================================================
# CREDENCIALES
# Guardar en un archivo .Renviron en la raíz del proyecto:
#   API_FOOTBALL_KEY=tu_clave
# El .Renviron NO debe subirse a control de versiones.
# ==============================================================================
API_FOOTBALL_KEY  <- Sys.getenv("API_FOOTBALL_KEY")
TELEGRAM_TOKEN    <- Sys.getenv("TELEGRAM_TOKEN")
TELEGRAM_CHAT_ID  <- Sys.getenv("TELEGRAM_CHAT_ID")

if (API_FOOTBALL_KEY == "") {
  warning("API_FOOTBALL_KEY no configurada. Crear .Renviron con la clave.")
}

# ==============================================================================
# API Y LIGAS
# IDs de api-football.com. Agregar o quitar según necesidad.
# ==============================================================================
API_FOOTBALL_BASE_URL   <- "https://v3.football.api-sports.io"
API_FOOTBALL_DELAY_SEC  <- 0.3
TEMPORADAS_ACTIVAS      <- c(2024, 2025, 2026)

PREMIER_LEAGUE_ID <- 39   ; CACHE_PATH       <- "premier_cache.rds"
CHAMPIONSHIP_ID   <- 40   ; CACHE_PATH_CHAMP <- "champ_cache.rds"
LALIGA_ID         <- 140  ; CACHE_PATH_ESP   <- "espana_cache.rds"
BUNDESLIGA_ID     <- 78   ; CACHE_PATH_GER   <- "alemania_cache.rds"
BRASILEIRAO_ID    <- 71   ; CACHE_PATH_BRA   <- "brasil_cache.rds"
ARGENTINA_ID      <- 128  ; CACHE_PATH_ARG   <- "argentina_cache.rds"

# Liga sobre la que operan aplicar_calibracion() y k_mercado()
LIGA_ACTIVA <- "PL"

# ==============================================================================
# PARÁMETROS DEL MOTOR
#
# rho_global      corrección Dixon-Coles para marcadores bajos
# WEIGHT_EXPONENT decaimiento temporal por día. -0.004 => vida media ~173 días
# PRIOR_WEIGHT    fuerza del shrinkage hacia la media de liga
#
# Los tres se pueden optimizar con tunear_decay() y tunear_prior() (backtests.R),
# minimizando log loss fuera de muestra.
# ==============================================================================
rho_global      <- -0.13
WEIGHT_EXPONENT <- -0.004
PRIOR_WEIGHT    <- 4.0

# ==============================================================================
# DISPERSIÓN — parámetro `size` de la binomial negativa.
#
# Se obtiene con:
#   estimar_dispersion("REMATES", datos = df_liga, lado = "L")
#
# La función compara la verosimilitud de Poisson vs binomial negativa e indica
# cuál usar. Si Poisson alcanza, no agregar la clave (el motor usa Poisson por
# defecto cuando la clave está ausente).
# ==============================================================================
K_DISPERSION <- list(
  # Ejemplo:
  # PL_REMATES_L = 25.24
)

# ==============================================================================
# CALIBRACIÓN DE LAMBDA — c(intercepto, pendiente)
#
# Se obtiene con:
#   d <- generar_predicciones("REMATES", 400, df_liga, "L")
#   coef(lm(Real ~ Lambda, d))
#
# Corrige sesgo de nivel sin tocar el motor.
#
# REGLA EMPÍRICA: no cargar si la pendiente cae fuera de 0.80-1.20. En los casos
# medidos, calibrar con pendientes lejanas a 1 empeoró el resultado — esas
# pendientes suelen ser ruido de estimación, no señal.
# ==============================================================================
CALIBRACION_LAMBDA <- list(
  # Ejemplo:
  # PL_REMATES_L = c(2.384, 0.866)
)

# ==============================================================================
# UMBRALES DE DECISIÓN
# ==============================================================================
MIN_PROB      <- 0.50
MIN_SCORE     <- 0.60
MIN_EV_PCT    <- 3.0    # por debajo, la diferencia contra el precio es ruido
EV_SOSPECHOSO <- 15.0   # por encima, revisar el modelo antes de confiar
KELLY_DIVISOR <- 8
STAKE_MAX_PCT <- 0.03
STAKE_MIN_PCT <- 0.005

# ==============================================================================
# INTERRUPTORES DE FEATURES
# Cada uno debe validarse midiendo log loss antes de activarlo.
# ==============================================================================
PACE_ACTIVO        <- FALSE  # riesgo de doble conteo: el λ ya incorpora volumen
PACE_INTENSIDAD    <- 0.70
FILTRAR_ROJAS      <- FALSE  # nunca activar en mercados de tarjetas o faltas
XG_BLEND_PESO      <- 0.60   # solo aplica al mercado GOLES
ARBITRO_SHRINK_K   <- 10     # partidos necesarios para confiar en un árbitro

# ==============================================================================
# Helper: construye la clave con prefijo de liga
# ==============================================================================
k_liga <- function(...) {
  pref <- if (exists("LIGA_ACTIVA")) LIGA_ACTIVA else "PL"
  paste(c(pref, ...), collapse = "_")
}
