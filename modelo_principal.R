# ==============================================================================
# ARCHIVO: modelo_principal.R — Predictor Analítico Cuantitativo
# VERSIÓN: 4.0
# ==============================================================================
source("config.R")
source("utils.R")
source("math_core.R")
source("market_registry.R")
source("data_pipeline.R")

print_safe <- function(val) ifelse(is.nan(val) | is.na(val), 0.0, val)

analizar <- function(local_user, visita_user, mercado = NULL,
                     linea = 0, cuota_casa = 0, arbitro_nombre = NULL,
                     datos = NULL, fecha_partido = Sys.Date(),
                     enviar = TRUE, verbose = TRUE) {
  
  # ---------------------------------------------------------------- 1. Setup --
  local_clean   <- unificar_nombre(local_user)
  visita_clean  <- unificar_nombre(visita_user)
  fecha_partido <- as.Date(fecha_partido)
  
  if (is.null(datos)) {
    message("⬇️ Cargando datos de Premier League...")
    datos <- cargar_datos_premier()
  }
  if (is.null(datos) || nrow(datos) < 10) {
    message("❌ No se pudieron obtener datos suficientes.")
    return(invisible(NULL))
  }
  
  all_teams <- unique(c(datos$HomeTeam, datos$AwayTeam))
  if (!local_clean %in% all_teams || !visita_clean %in% all_teams) {
    message(sprintf("❌ Equipo no encontrado. Local: '%s' | Visita: '%s'", local_clean, visita_clean))
    return(invisible(NULL))
  }
  
  # Point-in-time + decaimiento relativo a la fecha del partido
  datos <- datos %>% filter(Date < fecha_partido)
  datos$weight <- exp(WEIGHT_EXPONENT * as.numeric(fecha_partido - datos$Date))
  datos <- datos %>% filter(weight > 0.0001)
  
  if (nrow(datos) < 20) {
    message("❌ No hay suficientes datos previos a la fecha del partido.")
    return(invisible(NULL))
  }
  
  # ------------------------------------------------------------- 2. Árbitro --
  ref <- factor_arbitro(datos, arbitro_nombre, verbose = verbose)
  
  # ------------------------------------------- 3. Lambdas de todos los mercados
  resultados <- list()
  
  for (mkey in names(market_registry)) {
    if (mkey == "GOLES") next
    md <- market_registry[[mkey]]
    ajuste_ref <- if (!is.null(md$ajuste_ref)) md$ajuste_ref else "NINGUNO"
    
    raw <- calc_lambda_internal(
      md$col_L, md$col_V, datos, local_clean, visita_clean,
      ajuste_ref        = ajuste_ref,
      p_weight          = md$poisson_weight,
      factor_ref_cards  = ref$cards,      # [FIX C]
      factor_ref_faltas = ref$faltas,     # [FIX C]
      filtrar_rojas     = isTRUE(md$filtrar_rojas)
    )
    
    if (isTRUE(md$quality_adj) && raw$status == "OK") {
      adj_L <- calcular_factor_calidad(datos, local_clean,  TRUE,  md$label)
      adj_V <- calcular_factor_calidad(datos, visita_clean, FALSE, md$label)
      resultados[[mkey]] <- list(L = raw$L * adj_L, V = raw$V * adj_V,
                                 T = raw$L * adj_L + raw$V * adj_V,
                                 adj_L = adj_L, adj_V = adj_V, status = "OK")
    } else {
      resultados[[mkey]] <- c(raw, list(adj_L = 1, adj_V = 1))
    }
  }
  
  # --------------------------------------------------- 4. GOLES (blend con xG) --
  peso_goles <- market_registry[["GOLES"]]$poisson_weight
  goles_raw  <- calc_lambda_internal("FTHG", "FTAG", datos, local_clean, visita_clean,
                                     p_weight = peso_goles)
  
  hay_xg <- all(c("HXG", "AXG") %in% names(datos)) && sum(datos$HXG, na.rm = TRUE) > 5
  
  if (hay_xg && goles_raw$status == "OK") {
    xg_raw <- calc_lambda_internal("HXG", "AXG", datos, local_clean, visita_clean,
                                   p_weight = peso_goles)
    if (xg_raw$status == "OK") {
      w <- XG_BLEND_PESO
      lam_L <- goles_raw$L * (1 - w) + xg_raw$L * w
      lam_V <- goles_raw$V * (1 - w) + xg_raw$V * w
      if (verbose) message(sprintf("🎯 Blend xG activo (%.0f%% xG / %.0f%% goles).", w * 100, (1 - w) * 100))
    } else {
      lam_L <- goles_raw$L; lam_V <- goles_raw$V
    }
  } else {
    lam_L <- goles_raw$L; lam_V <- goles_raw$V
    if (verbose) message("⚠️ xG no disponible, usando goles reales.")
  }
  
  resultados[["GOLES"]] <- list(L = lam_L, V = lam_V, T = lam_L + lam_V,
                                adj_L = 1, adj_V = 1, status = goles_raw$status)
  goles <- resultados[["GOLES"]]
  
  # ------------------------------------------------ 5. Matriz Dixon-Coles ------
  prob_btts <- NA_real_; p_matrix <- NULL; tot_p <- NA_real_
  if (goles$status == "OK") {
    max_goles <- 9
    p_matrix  <- matrix(0, nrow = max_goles + 1, ncol = max_goles + 1)
    for (i in 0:max_goles) for (j in 0:max_goles) {
      p_matrix[i + 1, j + 1] <- dixon_coles_prob(i, j, goles$L, goles$V, rho_global)
    }
    tot_p     <- sum(p_matrix)
    prob_btts <- sum(p_matrix[2:(max_goles + 1), 2:(max_goles + 1)]) / tot_p
  }
  
  # ------------------------------------------- 6. Reporte completo (sin mercado)
  if (is.null(mercado)) {
    cat(sprintf("\n⚽ REPORTE — %s vs %s  (%s)\n", local_clean, visita_clean, format(fecha_partido)))
    cat("──────────────────────────────────────────\n")
    for (mkey in names(market_registry)) {
      md <- market_registry[[mkey]]; r <- resultados[[mkey]]
      cat(sprintf("%-16s: %6.2f - %6.2f   (total %6.2f)\n",
                  md$label, print_safe(r$L), print_safe(r$V), print_safe(r$T)))
    }
    if (!is.na(prob_btts)) cat(sprintf("%-16s: %6.1f%%\n", "AMBOS MARCAN", prob_btts * 100))
    cat("──────────────────────────────────────────\n")
    return(invisible(resultados))
  }
  
  # ------------------------------------------------ 7. Resolver el mercado -----
  mercado_lc <- tolower(mercado)
  es_under   <- grepl("menos|under|bajo", mercado_lc)
  matched    <- NULL
  lambda     <- NA_real_
  tipo       <- "T"
  label      <- ""
  prob_real  <- NA_real_
  
  if (grepl("ambos", mercado_lc)) {
    # ---- AMBOS MARCAN (antes nunca llegaba al scoring: [FIX A]) ----
    if (is.na(prob_btts)) { message("⛔ Sin datos para BTTS."); return(invisible(NULL)) }
    prob_real <- if (grepl("\\bno\\b", mercado_lc)) 1 - prob_btts else prob_btts
    label     <- "AMBOS MARCAN"
    etiqueta  <- if (grepl("\\bno\\b", mercado_lc)) "AMBOS MARCAN (NO)" else "AMBOS MARCAN (SÍ)"
    cv_match  <- 0.35
    base_stab <- 0.85
    
  } else {
    matched <- match_mercado(mercado_lc)
    if (is.null(matched)) { message("⛔ Mercado no reconocido: ", mercado); return(invisible(NULL)) }
    
    md  <- market_registry[[matched]]
    obj <- resultados[[matched]]
    label <- md$label
    if (is.null(obj) || obj$status != "OK") {
      message("⛔ Sin datos suficientes para: ", label); return(invisible(NULL))
    }
    
    if (grepl("local", mercado_lc))       { lambda <- obj$L; tipo <- "L" }
    else if (grepl("visita", mercado_lc)) { lambda <- obj$V; tipo <- "V" }
    else                                  { lambda <- obj$T; tipo <- "T" }
    
    lambda <- aplicar_calibracion(lambda, matched, tipo)
    
    # Pace factor (apagado por defecto en config.R hasta validarlo)
    if (label %in% c("REMATES", "TIROS ARCO", "CÓRNERS", "PASES")) {
      asimetria <- abs(resultados[["GOLES"]]$L - resultados[["GOLES"]]$V)
      lambda_0  <- lambda
      lambda    <- aplicar_pace_factor(lambda, label, md$col_L, md$col_V, datos,
                                       local_clean, visita_clean, asimetria)
      if (verbose && abs(lambda_0 - lambda) > 0.1) {
        message(sprintf("⏱️ Pace (%s): %.2f → %.2f (x%.3f)",
                        label, lambda_0, lambda, lambda / lambda_0))
      }
    }
    
    if (linea == 0) linea <- max(0.5, round(lambda) - 0.5)
    limite <- linea_a_limite(linea)
    
    # ---- Distribución [FIX D] ----
    if (label == "GOLES" && tipo == "T" && !is.null(p_matrix)) {
      prob_under <- 0
      for (i in 0:9) for (j in 0:9) if (i + j <= limite) prob_under <- prob_under + p_matrix[i + 1, j + 1] / tot_p
    } else if (label == "GOLES") {
      prob_under <- ppois(limite, lambda)
    } else {
      k_mkt <- k_mercado(matched, tipo)
      prob_under <- prob_bajo(limite, lambda, k_mkt)
      if (verbose) {
        message(sprintf("📏 %s | λ=%.2f | límite=%d | dist=%s",
                        label, lambda, limite,
                        if (is.na(k_mkt)) "Poisson" else sprintf("NegBin(k=%.1f)", k_mkt)))
      }
    }
    if (is.na(prob_under)) { message("⛔ Error numérico en la distribución."); return(invisible(NULL)) }
    prob_real <- if (es_under) prob_under else 1 - prob_under
    
    # ---- CV / estabilidad [FIX G] ----
    col_base <- if (tipo == "L") md$col_L else if (tipo == "V") md$col_V else md$col_L
    
    get_cv <- function(team_name, is_home, columnas) {
      df <- datos %>% filter(if (is_home) HomeTeam == team_name else AwayTeam == team_name) %>%
        arrange(desc(Date)) %>% head(10)
      if (nrow(df) < 4) return(0.40)
      vals <- if (length(columnas) == 2) df[[columnas[1]]] + df[[columnas[2]]] else df[[columnas[1]]]
      m <- mean(vals, na.rm = TRUE); s <- sd(vals, na.rm = TRUE)
      if (is.na(m) || m < 0.1 || is.na(s)) return(0.40)
      s / m
    }
    
    if (tipo == "L") {
      cv_match <- (get_cv(local_clean, TRUE, col_base) + get_cv(visita_clean, FALSE, col_base)) / 2
    } else if (tipo == "V") {
      cv_match <- (get_cv(visita_clean, FALSE, col_base) + get_cv(local_clean, TRUE, col_base)) / 2
    } else {
      cols <- c(md$col_L, md$col_V)
      cv_match <- (get_cv(local_clean, TRUE, cols) + get_cv(visita_clean, FALSE, cols)) / 2
    }
    
    base_stab <- if (label == "GOLES") {
      if (cv_match < 0.30) 1.10 else if (cv_match < 0.45) 1.00 else 0.90
    } else if (label == "FALTAS" && ref$n > 0) {
      md$base_stability + 0.15
    } else {
      md$base_stability
    }
    
    etiqueta <- sprintf("%s %s %s%s", label,
                        if (es_under) "MENOS DE" else "MÁS DE", linea,     # [FIX B]
                        if (tipo == "L") paste0(" (", local_clean, ")")
                        else if (tipo == "V") paste0(" (", visita_clean, ")") else " (TOTAL)")
  }
  
  # ------------------------------------------------------- 8. Scoring y EV -----
  if (is.na(prob_real) || prob_real <= 0 || prob_real >= 1) {
    message("⛔ Probabilidad fuera de rango."); return(invisible(NULL))
  }
  
  cuota_justa <- 1 / prob_real
  
  factor_cv <- dplyr::case_when(
    cv_match > 0.60 ~ 0.80,
    cv_match > 0.50 ~ 0.90,
    cv_match < 0.25 ~ 1.05,
    TRUE            ~ 1.00
  )
  score_pick <- prob_real * base_stab * factor_cv
  
  riesgo <- dplyr::case_when(
    cv_match < 0.30 ~ "💎 MUY ESTABLE",
    cv_match < 0.45 ~ "🟢 NORMAL",
    cv_match < 0.60 ~ "⚠️ VOLÁTIL",
    TRUE            ~ "☢️ CAÓTICO"
  )
  
  salida <- list(
    partido = paste(local_clean, "vs", visita_clean), fecha = fecha_partido,
    mercado = etiqueta, lambda = lambda, prob = prob_real,
    cuota_justa = cuota_justa, cuota_casa = cuota_casa,
    cv = cv_match, score = score_pick, riesgo = riesgo, ev_pct = NA_real_, stake_pct = 0
  )
  
  # [FIX E] sin cuota: informar igual, no devolver NULL
  if (is.null(cuota_casa) || is.na(cuota_casa) || cuota_casa <= 1) {
    cat(sprintf("\n📋 %s | %s\n", salida$partido, etiqueta))
    cat(sprintf("λ=%.2f | Prob: %.1f%% | Cuota justa: %.2f | CV %.2f (%s) | Score %.2f\n",
                print_safe(lambda), prob_real * 100, cuota_justa, cv_match, riesgo, score_pick))
    cat("ℹ️ Pasá cuota_casa para calcular EV y stake.\n")
    return(invisible(salida))
  }
  
  ev_pct <- (prob_real * cuota_casa - 1) * 100
  salida$ev_pct <- ev_pct
  
  kelly     <- (prob_real * cuota_casa - 1) / (cuota_casa - 1)
  stake_pct <- max(0, min(kelly / KELLY_DIVISOR, STAKE_MAX_PCT))
  salida$stake_pct <- stake_pct
  
  cat(sprintf("\n📋 %s | %s\n", salida$partido, etiqueta))
  cat(sprintf("λ=%.2f | Prob %.1f%% | Casa %.2f | Justa %.2f | EV %+.2f%% | CV %.2f (%s) | Score %.2f\n",
              print_safe(lambda), prob_real * 100, cuota_casa, cuota_justa, ev_pct,
              cv_match, riesgo, score_pick))
  
  # -------------------------------------------------------- 9. Filtros finales -
  motivos <- c()
  if (prob_real  < MIN_PROB)      motivos <- c(motivos, sprintf("prob %.2f < %.2f", prob_real, MIN_PROB))
  if (score_pick < MIN_SCORE)     motivos <- c(motivos, sprintf("score %.2f < %.2f", score_pick, MIN_SCORE))
  if (ev_pct     < MIN_EV_PCT)    motivos <- c(motivos, sprintf("EV %.2f%% < %.1f%%", ev_pct, MIN_EV_PCT))
  if (stake_pct  < STAKE_MIN_PCT) motivos <- c(motivos, "stake por debajo del mínimo")
  
  if (length(motivos) > 0) {
    cat(sprintf("⛔ NO PASA: %s\n", paste(motivos, collapse = " | ")))
    salida$pick <- FALSE
    return(invisible(salida))
  }
  
  salida$pick <- TRUE
  cat(sprintf("✅ PICK | Stake sugerido: %.2f%% del bank\n", stake_pct * 100))
  
  if (enviar) {
    msg <- paste0(
      "*ALERTA DE VALOR (EV+)*\n\n",
      "*PREMIER LEAGUE*\n",
      local_clean, " - ", visita_clean, "\n",
      "*Mercado:* ", etiqueta, "\n",
      "━━━━━━━━━━━━━━━━━━━\n",
      " - *Probabilidad:* ", sprintf("%.1f%%", prob_real * 100), "\n",
      " - *Cuota:* Casa ", cuota_casa, " | Justa ", sprintf("%.2f", cuota_justa), "\n",
      " - *EV:* ", sprintf("%+.2f%%", ev_pct), "\n",
      " - *Stake:* ", sprintf("%.2f%%", stake_pct * 100), " del bank\n",
      " - *Estabilidad:* ", sprintf("%.2f", cv_match), " (", riesgo, ")\n",
      "━━━━━━━━━━━━━━━━━━━\n",
      "_Exposición estadística. La decisión final es del usuario._"
    )
    enviar_telegram(msg)
  }
  
  registrar_pick(salida)
  invisible(salida)
}

# ==============================================================================
# registrar_pick(): [NUEVO] log de picks en CSV.
# Sin esto no podés medir CLV ni ROI real. Es la métrica que importa.
# ==============================================================================
registrar_pick <- function(p, archivo = "registro_picks.csv") {
  fila <- data.frame(
    ts = Sys.time(), fecha = p$fecha, partido = p$partido, mercado = p$mercado,
    lambda = round(p$lambda, 3), prob = round(p$prob, 4),
    cuota_casa = p$cuota_casa, cuota_justa = round(p$cuota_justa, 3),
    ev_pct = round(p$ev_pct, 2), stake_pct = round(p$stake_pct, 4),
    cv = round(p$cv, 3), score = round(p$score, 3),
    cuota_cierre = NA_real_, resultado = NA_integer_,   # <-- completar a mano después
    stringsAsFactors = FALSE
  )
  write.table(fila, archivo, sep = ",", row.names = FALSE,
              col.names = !file.exists(archivo), append = file.exists(archivo))
  invisible(NULL)
}