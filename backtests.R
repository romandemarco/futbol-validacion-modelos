# ==============================================================================
# ARCHIVO: backtests.R — Auditoría, calibración y tuneo
# VERSIÓN: 4.0
# ==============================================================================
source("config.R")
source("utils.R")
source("math_core.R")
source("market_registry.R")
source("data_pipeline.R")

# ------------------------------------------------------------------------------
# Helper interno: prepara el set de entrenamiento point-in-time
# ------------------------------------------------------------------------------
.train_hasta <- function(datos, fecha, min_n = 40) {
  tr <- datos %>% filter(Date < fecha)
  if (nrow(tr) < min_n) return(NULL)
  tr$weight <- exp(WEIGHT_EXPONENT * as.numeric(fecha - tr$Date))
  tr <- tr %>% filter(weight > 0.0001)
  if (nrow(tr) < min_n) return(NULL)
  tr
}

# ==============================================================================
# backtest_mercado(): bias y MAE por equipo, [FIX H] con el motor real
# ==============================================================================
backtest_mercado <- function(equipo, market_key, n_partidos = 20,
                             modo = c("realizado", "recibido"), datos = NULL) {
  modo <- match.arg(modo)
  if (!market_key %in% names(market_registry)) {
    message("❌ Mercado no reconocido. Opciones: ", paste(names(market_registry), collapse = ", "))
    return(invisible(NULL))
  }
  md <- market_registry[[market_key]]
  if (is.null(datos)) datos <- cargar_datos_premier(auto_update = FALSE)
  if (is.null(datos)) { message("❌ Sin datos."); return(invisible(NULL)) }
  
  target <- trimws(unificar_nombre(equipo))
  test <- datos %>% filter(HomeTeam == target | AwayTeam == target) %>%
    arrange(desc(Date)) %>% head(n_partidos) %>% arrange(Date)
  if (nrow(test) == 0) { message("⚠️ Sin partidos para ", target); return(invisible(NULL)) }
  
  message(sprintf("🎯 Auditando %s — %s (%s)", target, md$label, toupper(modo)))
  res <- data.frame()
  
  for (i in seq_len(nrow(test))) {
    m <- test[i, ]
    is_local <- m$HomeTeam == target
    rival    <- if (is_local) m$AwayTeam else m$HomeTeam
    
    train <- .train_hasta(datos, m$Date)
    if (is.null(train)) next
    
    ref <- factor_arbitro(train, if ("Referee" %in% names(m)) m$Referee else NULL, verbose = FALSE)
    
    raw <- calc_lambda_internal(md$col_L, md$col_V, train, m$HomeTeam, m$AwayTeam,
                                ajuste_ref = if (!is.null(md$ajuste_ref)) md$ajuste_ref else "NINGUNO",
                                p_weight = md$poisson_weight,
                                factor_ref_cards = ref$cards, factor_ref_faltas = ref$faltas,
                                filtrar_rojas = isTRUE(md$filtrar_rojas))
    if (raw$status != "OK") next
    
    if (isTRUE(md$quality_adj)) {
      raw$L <- raw$L * calcular_factor_calidad(train, m$HomeTeam,  TRUE,  md$label)
      raw$V <- raw$V * calcular_factor_calidad(train, m$AwayTeam, FALSE, md$label)
    }
    
    # "realizado" = lo que produce el target ; "recibido" = lo que produce el rival
    if (modo == "realizado") {
      esp  <- if (is_local) raw$L else raw$V
      real <- if (is_local) m[[md$col_L]] else m[[md$col_V]]
    } else {
      esp  <- if (is_local) raw$V else raw$L
      real <- if (is_local) m[[md$col_V]] else m[[md$col_L]]
    }
    if (is.na(real) || !is.finite(esp)) next
    
    res <- rbind(res, data.frame(
      Fecha = m$Date, Cond = if (is_local) "(L)" else "(V)", Rival = rival,
      Esp = round(esp, 1), Real = real, Diff = round(real - esp, 1),
      Estado = dplyr::case_when(real - esp > 2.5 ~ "🚀", real - esp < -2.5 ~ "🔻", TRUE ~ "✅")
    ))
  }
  
  if (nrow(res) == 0) { message("⚠️ Sin predicciones generadas."); return(invisible(NULL)) }
  print(res)
  
  cat(sprintf("\n📊 DIAGNÓSTICO — %s — %s (%s)\n", target, md$label, toupper(modo)))
  cat("------------------------------------------------\n")
  rep_bias <- function(d, et) {
    if (nrow(d) == 0) { cat(sprintf("%s: sin datos.\n", et)); return(invisible(NULL)) }
    cat(sprintf("%s: %d partidos | BIAS %+.2f | MAE %.2f\n", et, nrow(d),
                mean(d$Diff, na.rm = TRUE), mean(abs(d$Diff), na.rm = TRUE)))
  }
  rep_bias(res %>% filter(Cond == "(L)"), "🏠 EN CASA")
  rep_bias(res %>% filter(Cond == "(V)"), "✈️ DE VISITA")
  rep_bias(res, "📌 GLOBAL")
  cat("------------------------------------------------\n")
  invisible(res)
}

# ==============================================================================
# generar_predicciones(): corazón compartido de calibración y tuneo.
# Devuelve un data.frame con lambda y valor real para cada partido del test.
# ==============================================================================
generar_predicciones <- function(market_key, n_partidos = 300, datos = NULL, lado = "T") {
  if (is.null(datos)) datos <- cargar_datos_premier(auto_update = FALSE)
  md <- market_registry[[market_key]]
  
  test <- datos %>% arrange(desc(Date)) %>% head(n_partidos) %>% arrange(Date)
  out <- vector("list", nrow(test))
  
  for (i in seq_len(nrow(test))) {
    m <- test[i, ]
    train <- .train_hasta(datos, m$Date, min_n = 50)
    if (is.null(train)) next
    
    ref <- factor_arbitro(train, if ("Referee" %in% names(m)) m$Referee else NULL, verbose = FALSE)
    
    raw <- calc_lambda_internal(md$col_L, md$col_V, train, m$HomeTeam, m$AwayTeam,
                                ajuste_ref = if (!is.null(md$ajuste_ref)) md$ajuste_ref else "NINGUNO",
                                p_weight = md$poisson_weight,
                                factor_ref_cards = ref$cards, factor_ref_faltas = ref$faltas,
                                filtrar_rojas = isTRUE(md$filtrar_rojas))
    if (raw$status != "OK") next
    
    adj_L <- if (isTRUE(md$quality_adj)) calcular_factor_calidad(train, m$HomeTeam,  TRUE,  md$label) else 1
    adj_V <- if (isTRUE(md$quality_adj)) calcular_factor_calidad(train, m$AwayTeam, FALSE, md$label) else 1
    
    lam <- switch(lado,
                  "T" = raw$L * adj_L + raw$V * adj_V,
                  "L" = raw$L * adj_L,
                  "V" = raw$V * adj_V)
    
    lam <- aplicar_calibracion(lam, market_key, lado)
    
    if (lado == "T" && md$label %in% c("REMATES", "TIROS ARCO", "CÓRNERS", "PASES")) {
      g <- calc_lambda_internal("FTHG", "FTAG", train, m$HomeTeam, m$AwayTeam,
                                p_weight = market_registry[["GOLES"]]$poisson_weight)
      asim <- if (g$status == "OK") abs(g$L - g$V) else 0
      lam <- aplicar_pace_factor(lam, md$label, md$col_L, md$col_V, train,
                                 m$HomeTeam, m$AwayTeam, asim)
    }
    
    real <- switch(lado,
                   "T" = m[[md$col_L]] + m[[md$col_V]],
                   "L" = m[[md$col_L]], "V" = m[[md$col_V]])
    if (is.na(real) || !is.finite(lam) || lam <= 0) next
    
    out[[i]] <- data.frame(Fecha = m$Date,
                           Partido = paste(m$HomeTeam, "vs", m$AwayTeam),
                           Lambda = lam, Real = real)
    if (i %% 50 == 0) message(sprintf("   ...%d/%d", i, nrow(test)))
  }
  
  df <- bind_rows(out)
  if (nrow(df) == 0) stop("No se generaron predicciones.")
  df
}

# ==============================================================================
# estimar_dispersion(): k de la binomial negativa por máxima verosimilitud
#
# POR QUÉ IMPORTA: calcular_sd_empirico() devolvía el SD INCONDICIONAL, que
# mezcla la varianza del partido dado lambda (la que querés) con la variación
# de lambda entre partidos (que tu modelo ya explica). Eso inflaba la varianza
# y aplastaba todas las probabilidades hacia 0.5.
# ==============================================================================
estimar_dispersion <- function(market_key, datos = NULL, n_partidos = 400, lado = "T") {
  md <- market_registry[[market_key]]
  df <- generar_predicciones(market_key, n_partidos, datos, lado)
  mu <- df$Lambda; y <- df$Real
  if (length(y) < 50) stop("Muestra insuficiente.")
  
  nll <- function(log_k) -sum(dnbinom(round(y), mu = mu, size = exp(log_k), log = TRUE))
  opt <- stats::optimize(nll, c(-1, 10))
  k   <- exp(opt$minimum)
  
  ll_pois <- sum(dpois(round(y), mu, log = TRUE))
  ll_nb   <- -opt$objective
  usar_nb <- ll_nb > ll_pois + 2 && k < 500
  
  cat(sprintf("\n📐 DISPERSIÓN — %s (%s) | n=%d\n", md$label, lado, length(y)))
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat(sprintf("k estimado        : %.2f\n", k))
  cat(sprintf("λ medio           : %.2f\n", mean(mu)))
  cat(sprintf("SD condicional    : %.2f   <-- la correcta\n", sqrt(mean(mu) + mean(mu)^2 / k)))
  cat(sprintf("SD incondicional  : %.2f   <-- la que usabas antes\n", sd(y)))
  cat(sprintf("logLik Poisson    : %.1f\n", ll_pois))
  cat(sprintf("logLik NegBin     : %.1f\n", ll_nb))
  cat(sprintf("MAE %.2f | BIAS %+.2f\n", mean(abs(y - mu)), mean(y - mu)))
  cat(sprintf("\n👉 En config.R poné:  %s = %s\n", market_key,
              if (usar_nb) sprintf("%.2f", k) else "NA_real_  (Poisson alcanza)"))
  
  invisible(list(k = k, usar_nb = usar_nb, mu = mu, y = y))
}

# ==============================================================================
# evaluar_modelo(): la métrica honesta
# ==============================================================================
evaluar_modelo <- function(prob, resultado, cuotas = NULL, nombre = "modelo") {
  p <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
  y <- as.numeric(resultado)
  logloss <- function(p, y) -mean(y * log(p) + (1 - y) * log(1 - p))
  brier   <- function(p, y) mean((p - y)^2)
  base <- rep(mean(y), length(y))
  
  cat(sprintf("\n📊 EVALUACIÓN — %s | n=%d | tasa base=%.3f\n", nombre, length(y), mean(y)))
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat(sprintf("Log loss : %.4f | baseline %.4f | mejora %+.4f\n",
              logloss(p, y), logloss(base, y), logloss(base, y) - logloss(p, y)))
  cat(sprintf("Brier    : %.4f | baseline %.4f\n", brier(p, y), brier(base, y)))
  cat(sprintf("Sesgo    : %+.4f (predicho - real)\n", mean(p) - mean(y)))
  if (logloss(p, y) >= logloss(base, y)) {
    cat("⛔ El modelo NO le gana a la tasa base. No apuestes este mercado todavía.\n")
  }
  
  fit <- try(suppressWarnings(glm(y ~ qlogis(p), family = binomial)), silent = TRUE)
  if (!inherits(fit, "try-error")) {
    co <- coef(fit)
    cat(sprintf("Platt: intercept %+.3f | slope %.3f (ideal 0.000 / 1.000)\n", co[1], co[2]))
    if (!is.na(co[2]) && co[2] < 0.8) cat("   ⚠️ slope<0.8 → sobre-confiado: achicá el spread.\n")
    if (!is.na(co[2]) && co[2] > 1.2) cat("   ⚠️ slope>1.2 → sub-confiado.\n")
  }
  
  tabla <- data.frame(p = p, y = y) %>%
    mutate(bin = cut(p, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(n = dplyr::n(), pred = mean(p), real = mean(y),
              desvio = mean(p) - mean(y), .groups = "drop") %>%
    filter(n >= 10)
  print(as.data.frame(tabla))
  
  if (!is.null(cuotas)) {
    ev <- p * cuotas - 1
    sel <- ev > MIN_EV_PCT / 100
    if (sum(sel) > 0) {
      roi <- mean(ifelse(y[sel] == 1, cuotas[sel] - 1, -1))
      cat(sprintf("\n💰 Picks EV>%.1f%%: %d (%.1f%%) | ROI real %+.2f%% | EV prometido %+.2f%%\n",
                  MIN_EV_PCT, sum(sel), 100 * mean(sel), 100 * roi, 100 * mean(ev[sel])))
    }
  }
  invisible(tabla)
}

# ==============================================================================
# backtest_calibracion(): ahora delega la evaluación a evaluar_modelo()
# ==============================================================================
backtest_calibracion <- function(market_key, linea, es_under = FALSE,
                                 n_partidos = 300, datos = NULL, lado = "T") {
  if (!market_key %in% names(market_registry)) stop("❌ Mercado no reconocido.")
  md <- market_registry[[market_key]]
  
  df <- generar_predicciones(market_key, n_partidos, datos, lado)
  limite <- linea_a_limite(linea)
  k_mkt  <- k_mercado(market_key, lado)
  
  df$Prob_Pred <- sapply(df$Lambda, function(l) {
    pu <- prob_bajo(limite, l, k_mkt)
    if (es_under) pu else 1 - pu
  })
  df$Res_Binario <- if (es_under) as.integer(df$Real <= limite) else as.integer(df$Real > limite)
  df <- df %>% filter(!is.na(Prob_Pred))
  
  cat(sprintf("\n🎯 CALIBRACIÓN | %s (%s) | línea %.1f | %s\n",
              md$label, lado, linea, if (es_under) "UNDER" else "OVER"))
  evaluar_modelo(df$Prob_Pred, df$Res_Binario,
                 nombre = sprintf("%s %s %.1f", md$label, if (es_under) "U" else "O", linea))
  invisible(df)
}

# ==============================================================================
# tunear_decay(): grid search del decaimiento por log loss out-of-sample
# ==============================================================================
tunear_decay <- function(market_key, linea, grid = c(-0.001, -0.002, -0.004, -0.008, -0.015),
                         n_partidos = 250, datos = NULL, es_under = FALSE) {
  if (is.null(datos)) datos <- cargar_datos_premier(auto_update = FALSE)
  original <- WEIGHT_EXPONENT
  limite <- linea_a_limite(linea)
  k_mkt  <- k_mercado(market_key, "T")
  res <- data.frame()
  
  for (g in grid) {
    assign("WEIGHT_EXPONENT", g, envir = globalenv())
    message(sprintf("🔧 Probando decay = %.4f (vida media %d días)...", g, round(log(2) / abs(g))))
    df <- try(generar_predicciones(market_key, n_partidos, datos, "T"), silent = TRUE)
    if (inherits(df, "try-error")) next
    p <- sapply(df$Lambda, function(l) { pu <- prob_bajo(limite, l, k_mkt); if (es_under) pu else 1 - pu })
    p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
    y <- if (es_under) as.integer(df$Real <= limite) else as.integer(df$Real > limite)
    res <- rbind(res, data.frame(decay = g, vida_media = round(log(2) / abs(g)),
                                 n = length(y),
                                 logloss = -mean(y * log(p) + (1 - y) * log(1 - p)),
                                 mae = mean(abs(df$Real - df$Lambda))))
  }
  assign("WEIGHT_EXPONENT", original, envir = globalenv())
  res <- res[order(res$logloss), ]
  print(res)
  cat(sprintf("\n👉 En config.R poné: WEIGHT_EXPONENT <- %.4f\n", res$decay[1]))
  invisible(res)
}

# ==============================================================================
# tunear_prior(): idem para PRIOR_WEIGHT
# ==============================================================================
tunear_prior <- function(market_key, linea, grid = c(2, 4, 6, 9, 14),
                         n_partidos = 250, datos = NULL, es_under = FALSE) {
  if (is.null(datos)) datos <- cargar_datos_premier(auto_update = FALSE)
  original <- PRIOR_WEIGHT
  limite <- linea_a_limite(linea)
  k_mkt  <- k_mercado(market_key, "T")
  res <- data.frame()
  
  for (g in grid) {
    assign("PRIOR_WEIGHT", g, envir = globalenv())
    message(sprintf("🔧 Probando PRIOR_WEIGHT = %.1f...", g))
    df <- try(generar_predicciones(market_key, n_partidos, datos, "T"), silent = TRUE)
    if (inherits(df, "try-error")) next
    p <- sapply(df$Lambda, function(l) { pu <- prob_bajo(limite, l, k_mkt); if (es_under) pu else 1 - pu })
    p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
    y <- if (es_under) as.integer(df$Real <= limite) else as.integer(df$Real > limite)
    res <- rbind(res, data.frame(prior = g, n = length(y),
                                 logloss = -mean(y * log(p) + (1 - y) * log(1 - p)),
                                 mae = mean(abs(df$Real - df$Lambda))))
  }
  assign("PRIOR_WEIGHT", original, envir = globalenv())
  res <- res[order(res$logloss), ]
  print(res)
  cat(sprintf("\n👉 En config.R poné: PRIOR_WEIGHT <- %.1f\n", res$prior[1]))
  invisible(res)
}