# ==============================================================================
# ARCHIVO: math_core.R — Núcleo matemático compartido
# VERSIÓN: 4.0
# ==============================================================================
library(dplyr)

# ------------------------------------------------------------------------------
# Dixon-Coles: corrección para marcadores bajos (sin cambios)
# ------------------------------------------------------------------------------
dixon_coles_prob <- function(i, j, lambda_L, lambda_V, rho) {
  p_poisson  <- dpois(i, lambda_L) * dpois(j, lambda_V)
  correction <- 1
  if      (i == 0 && j == 0) correction <- 1 - (lambda_L * lambda_V * rho)
  else if (i == 0 && j == 1) correction <- 1 + (lambda_L * rho)
  else if (i == 1 && j == 0) correction <- 1 + (lambda_V * rho)
  else if (i == 1 && j == 1) correction <- 1 - rho
  max(p_poisson * correction, 0)
}

# ------------------------------------------------------------------------------
# asegurar_weight(): garantiza que exista la columna weight
# ------------------------------------------------------------------------------
asegurar_weight <- function(datos) {
  if (!"weight" %in% names(datos)) datos$weight <- 1
  datos$weight[is.na(datos$weight) | datos$weight <= 0] <- 1e-6
  datos
}

# ------------------------------------------------------------------------------
# medias_por_equipo(): media ponderada por equipo con shrinkage a la liga.
# Base R (tapply) para que sea rápido dentro de los loops de backtest.
# ------------------------------------------------------------------------------
medias_por_equipo <- function(datos, col, rol = c("home", "away"), prior_w = PRIOR_WEIGHT) {
  rol <- match.arg(rol)
  key <- if (rol == "home") as.character(datos$HomeTeam) else as.character(datos$AwayTeam)
  x   <- suppressWarnings(as.numeric(datos[[col]]))
  w   <- datos$weight
  
  ok <- !is.na(x) & !is.na(w) & !is.na(key)
  if (!any(ok)) return(numeric(0))
  x <- x[ok]; w <- w[ok]; key <- key[ok]
  
  prom_liga <- sum(x * w) / sum(w)
  
  sw  <- tapply(w,     key, sum)
  sxw <- tapply(x * w, key, sum)
  n   <- tapply(rep(1, length(x)), key, sum)
  
  m <- as.numeric(sxw / sw)
  m[is.na(m)] <- prom_liga
  n <- as.numeric(n)
  
  out <- (n * m + prior_w * prom_liga) / (n + prior_w)
  stats::setNames(out, names(sw))
}

# ------------------------------------------------------------------------------
# MOTOR: calc_lambda_internal()
# ------------------------------------------------------------------------------
calc_lambda_internal <- function(col_local, col_visita, datos_func, local_tm, visita_tm,
                                 ajuste_ref = "NINGUNO", p_weight = 0.60,
                                 factor_ref_cards = 1.0, factor_ref_faltas = 1.0,
                                 filtrar_rojas = FALSE) {
  
  if (!all(c(col_local, col_visita) %in% names(datos_func))) {
    return(list(L = 0, V = 0, T = 0, status = "NO_COL"))
  }
  datos_clean <- asegurar_weight(datos_func)
  
  # [FIX 2] filtro de rojas opcional
  if (isTRUE(filtrar_rojas) && all(c("HR", "AR") %in% names(datos_clean))) {
    tmp <- datos_clean %>% filter(HR == 0 & AR == 0)
    if (nrow(tmp) >= 10) datos_clean <- tmp
  }
  
  prom_L <- weighted.mean(datos_clean[[col_local]],  w = datos_clean$weight, na.rm = TRUE)
  prom_V <- weighted.mean(datos_clean[[col_visita]], w = datos_clean$weight, na.rm = TRUE)
  if (is.na(prom_L) || is.na(prom_V) || prom_L < 0.01 || prom_V < 0.01) {
    return(list(L = 0, V = 0, T = 0, status = "LOW_DATA"))
  }
  
  df_L <- datos_clean %>% filter(HomeTeam == local_tm)
  df_V <- datos_clean %>% filter(AwayTeam == visita_tm)
  if (nrow(df_L) == 0 || nrow(df_V) == 0) {
    return(list(L = 0, V = 0, T = 0, status = "NO_GAMES"))
  }
  
  alpha_L <- nrow(df_L) / (nrow(df_L) + PRIOR_WEIGHT)
  alpha_V <- nrow(df_V) / (nrow(df_V) + PRIOR_WEIGHT)
  
  # [FIX 3] mapas de referencia ponderados + shrinkage, vectorizados
  map_vis_permiten_L <- medias_por_equipo(datos_clean, col_local,  "away")
  map_loc_generan_L  <- medias_por_equipo(datos_clean, col_local,  "home")
  map_loc_permiten_V <- medias_por_equipo(datos_clean, col_visita, "home")
  map_vis_generan_V  <- medias_por_equipo(datos_clean, col_visita, "away")
  
  pick <- function(mapa, claves, fallback) {
    v <- unname(mapa[as.character(claves)])
    v[is.na(v) | v < 0.1] <- fallback
    v
  }
  
  # --- Lado LOCAL ---
  opp_allow_L  <- pick(map_vis_permiten_L, df_L$AwayTeam, prom_L)
  raw_L_ataque <- weighted.mean((df_L[[col_local]] / opp_allow_L) * prom_L,
                                w = df_L$weight, na.rm = TRUE)
  val_L_ataque <- alpha_L * raw_L_ataque + (1 - alpha_L) * prom_L
  
  opp_gen_V  <- pick(map_loc_generan_L, df_V$HomeTeam, prom_L)
  raw_V_conc <- weighted.mean((df_V[[col_local]] / opp_gen_V) * prom_L,
                              w = df_V$weight, na.rm = TRUE)
  val_V_conc <- alpha_V * raw_V_conc + (1 - alpha_V) * prom_L
  
  lambda_L_poisson <- (val_L_ataque / prom_L) * (val_V_conc / prom_L) * prom_L
  lambda_L_linear  <- (val_L_ataque + val_V_conc) / 2
  lambda_L <- lambda_L_poisson * p_weight + lambda_L_linear * (1 - p_weight)
  
  # --- Lado VISITA ---
  opp_allow_V  <- pick(map_loc_permiten_V, df_V$HomeTeam, prom_V)
  raw_V_ataque <- weighted.mean((df_V[[col_visita]] / opp_allow_V) * prom_V,
                                w = df_V$weight, na.rm = TRUE)
  val_V_ataque <- alpha_V * raw_V_ataque + (1 - alpha_V) * prom_V
  
  opp_gen_L  <- pick(map_vis_generan_V, df_L$AwayTeam, prom_V)
  raw_L_conc <- weighted.mean((df_L[[col_visita]] / opp_gen_L) * prom_V,
                              w = df_L$weight, na.rm = TRUE)
  val_L_conc <- alpha_L * raw_L_conc + (1 - alpha_L) * prom_V
  
  lambda_V_poisson <- (val_V_ataque / prom_V) * (val_L_conc / prom_V) * prom_V
  lambda_V_linear  <- (val_V_ataque + val_L_conc) / 2
  lambda_V <- lambda_V_poisson * p_weight + lambda_V_linear * (1 - p_weight)
  
  # [FIX 1] árbitro por argumento (antes era código muerto)
  if (ajuste_ref == "TARJETA") {
    lambda_L <- lambda_L * factor_ref_cards
    lambda_V <- lambda_V * factor_ref_cards
  }
  if (ajuste_ref == "FALTA") {
    lambda_L <- lambda_L * factor_ref_faltas
    lambda_V <- lambda_V * factor_ref_faltas
  }
  
  if (!is.finite(lambda_L) || !is.finite(lambda_V) || lambda_L <= 0 || lambda_V <= 0) {
    return(list(L = 0, V = 0, T = 0, status = "NUM_ERROR"))
  }
  
  list(L = lambda_L, V = lambda_V, T = lambda_L + lambda_V, status = "OK")
}

# ------------------------------------------------------------------------------
# factor_arbitro(): [FIX 5] con shrinkage y topes duros
# ------------------------------------------------------------------------------
factor_arbitro <- function(datos, arbitro_nombre, k = ARBITRO_SHRINK_K, verbose = TRUE) {
  out <- list(faltas = 1.0, cards = 1.0, n = 0)
  if (is.null(arbitro_nombre) || all(is.na(arbitro_nombre)) ||
      !"Referee" %in% names(datos)) return(out)
  
  datos <- asegurar_weight(datos)
  apellido <- tail(strsplit(trimws(arbitro_nombre), " ")[[1]], 1)
  if (nchar(apellido) < 4) return(out)   # evita matches espurios
  
  ref <- datos %>% filter(!is.na(Referee) & grepl(apellido, Referee, ignore.case = TRUE))
  n <- nrow(ref)
  if (n < 5) {
    if (verbose) message(sprintf("🦓 Árbitro '%s': solo %d partidos, sin ajuste.", apellido, n))
    return(out)
  }
  
  liga_f <- weighted.mean(datos$HF + datos$AF, w = datos$weight, na.rm = TRUE)
  liga_c <- weighted.mean(datos$TotalCards,    w = datos$weight, na.rm = TRUE)
  ref_f  <- weighted.mean(ref$HF + ref$AF,     w = ref$weight,   na.rm = TRUE)
  ref_c  <- weighted.mean(ref$TotalCards,      w = ref$weight,   na.rm = TRUE)
  
  peso <- n / (n + k)   # shrinkage hacia 1
  if (is.finite(liga_f) && liga_f > 0.1) out$faltas <- 1 + (ref_f / liga_f - 1) * peso
  if (is.finite(liga_c) && liga_c > 0.1) out$cards  <- 1 + (ref_c / liga_c - 1) * peso
  
  out$faltas <- max(0.80, min(1.25, out$faltas))
  out$cards  <- max(0.75, min(1.30, out$cards))
  out$n <- n
  
  if (verbose) {
    message(sprintf("🦓 Árbitro (%s, n=%d): Faltas x%.2f | Tarjetas x%.2f",
                    arbitro_nombre, n, out$faltas, out$cards))
  }
  out
}

# ------------------------------------------------------------------------------
# calcular_factor_calidad(): sin cambios de lógica
# ------------------------------------------------------------------------------
calcular_factor_calidad <- function(datos_filtrados, equipo, is_home, mercado_label, verbose = FALSE) {
  df <- datos_filtrados %>%
    filter(if (is_home) HomeTeam == equipo else AwayTeam == equipo) %>%
    arrange(desc(Date)) %>%
    head(10)
  
  if (nrow(df) < 5) return(1.0)
  
  avg_goals  <- mean(if (is_home) df$FTHG else df$FTAG, na.rm = TRUE)
  avg_shots  <- mean(if (is_home) df$HS   else df$AS,   na.rm = TRUE)
  avg_target <- mean(if (is_home) df$HST  else df$AST,  na.rm = TRUE)
  avg_corner <- mean(if (is_home) df$HC   else df$AC,   na.rm = TRUE)
  
  if (is.na(avg_shots)  || avg_shots  < 1)   avg_shots  <- 1
  if (is.na(avg_goals)  || avg_goals  < 0.1) avg_goals  <- 0.1
  if (is.na(avg_target) || avg_target < 0.5) avg_target <- 0.5
  if (is.na(avg_corner) || avg_corner < 0.5) avg_corner <- 0.5
  
  prom_shots_liga  <- mean(if (is_home) datos_filtrados$HS   else datos_filtrados$AS,   na.rm = TRUE)
  prom_target_liga <- mean(if (is_home) datos_filtrados$HST  else datos_filtrados$AST,  na.rm = TRUE)
  prom_goals_liga  <- mean(if (is_home) datos_filtrados$FTHG else datos_filtrados$FTAG, na.rm = TRUE)
  prom_corner_liga <- mean(if (is_home) datos_filtrados$HC   else datos_filtrados$AC,   na.rm = TRUE)
  
  if (is.na(prom_shots_liga)  || prom_shots_liga  < 1)   prom_shots_liga  <- 10
  if (is.na(prom_target_liga) || prom_target_liga < 1)   prom_target_liga <- 3.5
  if (is.na(prom_goals_liga)  || prom_goals_liga  < 0.1) prom_goals_liga  <- 1.2
  if (is.na(prom_corner_liga) || prom_corner_liga < 1)   prom_corner_liga <- 4.5
  
  factor_ajuste <- 1.0
  
  if (mercado_label == "REMATES") {
    ratio_precision <- (avg_target / avg_shots) / (prom_target_liga / prom_shots_liga)
    ratio_efic      <- (avg_goals  / avg_shots) / (prom_goals_liga  / prom_shots_liga)
    factor_crudo  <- (ratio_precision * 0.55) + (ratio_efic * 0.45)
    factor_ajuste <- 1.0 + ((factor_crudo - 1.0) * 0.50)
    factor_ajuste <- max(0.82, min(1.08, factor_ajuste))
    
  } else if (mercado_label == "TIROS ARCO") {
    ratio_conv <- (avg_goals / avg_target) / (prom_goals_liga / prom_target_liga)
    factor_ajuste <- 1.0 + ((ratio_conv - 1.0) * 0.45)
    factor_ajuste <- max(0.83, min(1.07, factor_ajuste))
    
  } else if (mercado_label == "CÓRNERS") {
    ratio_corners <- avg_corner / prom_corner_liga
    factor_ajuste <- 1.0 + ((ratio_corners - 1.0) * 0.40)
    factor_ajuste <- max(0.85, min(1.08, factor_ajuste))
  }
  
  if (verbose) message(sprintf("📐 %s (%s) → Factor %.3f", equipo, mercado_label, factor_ajuste))
  if (!is.finite(factor_ajuste)) return(1.0)
  factor_ajuste
}

# ------------------------------------------------------------------------------
# aplicar_pace_factor(): [FIX 4] orden corregido + interruptor
# ------------------------------------------------------------------------------
aplicar_pace_factor <- function(lambda, mercado_label, col_L, col_V, datos,
                                local_clean, visita_clean, asimetria = 0,
                                activo = PACE_ACTIVO, intensidad = PACE_INTENSIDAD) {
  if (!isTRUE(activo)) return(lambda)
  
  prom_liga_T <- mean(datos[[col_L]] + datos[[col_V]], na.rm = TRUE)
  if (is.na(prom_liga_T) || prom_liga_T <= 0) return(lambda)
  
  ultimos <- function(eq) {
    datos %>%
      filter(HomeTeam == eq | AwayTeam == eq) %>%
      arrange(desc(Date)) %>%      # <-- [FIX 4] antes tomaba los MÁS VIEJOS
      head(15)
  }
  df_L <- ultimos(local_clean)
  df_V <- ultimos(visita_clean)
  if (nrow(df_L) < 5 || nrow(df_V) < 5) return(lambda)
  
  ritmo_L <- mean(df_L[[col_L]] + df_L[[col_V]], na.rm = TRUE) / prom_liga_T
  ritmo_V <- mean(df_V[[col_L]] + df_V[[col_V]], na.rm = TRUE) / prom_liga_T
  if (!is.finite(ritmo_L) || !is.finite(ritmo_V)) return(lambda)
  
  pace_crudo <- (ritmo_L + ritmo_V) / 2
  pace_crudo <- pace_crudo - if (asimetria > 0.8) (asimetria - 0.8) * 0.08 else 0
  if (pace_crudo <= 0.1) return(lambda)
  
  pace_factor <- if (mercado_label == "PASES") {
    1 + ((1 / pace_crudo) - 1) * 0.60
  } else {
    1 + (pace_crudo - 1) * intensidad
  }
  lambda * max(0.80, min(1.20, pace_factor))
}

# ------------------------------------------------------------------------------
# prob_bajo(): distribución centralizada.
# ------------------------------------------------------------------------------
prob_bajo <- function(limite, lambda, k = NA_real_) {
  if (!is.finite(lambda) || lambda <= 0) return(NA_real_)
  if (is.na(k) || !is.finite(k) || k <= 0) return(ppois(limite, lambda))
  pnbinom(limite, size = k, mu = lambda)
}

# ------------------------------------------------------------------------------
# k_mercado(): devuelve el k de dispersión de la liga activa.
# El motor divide por 2 en lados individuales (L/V): por eso PL_REMATES
# guarda 50.48, que es el doble del 25.24 estimado para el lado local.
# ------------------------------------------------------------------------------
k_mercado <- function(market_key, lado = "T") {
  if (!exists("K_DISPERSION")) return(NA_real_)
  k <- K_DISPERSION[[k_liga(market_key, lado)]]
  if (is.null(k) || is.na(k)) return(NA_real_)
  k
}

# ------------------------------------------------------------------------------
# aplicar_calibracion(): corrige lambda según liga, mercado y lado.
# ------------------------------------------------------------------------------
aplicar_calibracion <- function(lambda, market_key, lado = "T") {
  if (!exists("CALIBRACION_LAMBDA")) return(lambda)
  cal <- CALIBRACION_LAMBDA[[k_liga(market_key, lado)]]
  if (is.null(cal)) return(lambda)
  max(cal[1] + cal[2] * lambda, 0.05)
}

linea_a_limite <- function(linea) {
  if (linea %% 1 == 0) as.integer(linea) - 1L else as.integer(floor(linea))
}