# ==============================================================================
# ARCHIVO: diagnostico_senal.R
# Mide cuánta señal REAL tiene el modelo en cada mercado, y cuánto habría que
# encoger las predicciones para que dejen de amplificar ruido.
#
# Uso:
#   source("diagnostico_senal.R")
#   diagnostico_senal("CORNERS", df_premier)
#   tabla_senal(df_premier)            # todos los mercados de una
# ==============================================================================
source("backtests.R")

# ------------------------------------------------------------------------------
# diagnostico_senal(): correlación, MAE vs baseline y encogimiento óptimo
#
# El encogimiento se estima en la PRIMERA mitad de la muestra y se testea en la
# SEGUNDA. Si lo estimás y testeás en la misma muestra siempre parece que mejora.
# ------------------------------------------------------------------------------
diagnostico_senal <- function(market_key, datos = NULL, n_partidos = 400,
                              lado = "T", verbose = TRUE) {
  md <- market_registry[[market_key]]
  d  <- generar_predicciones(market_key, n_partidos, datos, lado)
  d  <- d %>% arrange(Fecha)
  n  <- nrow(d)
  if (n < 100) { message("⚠️ Muestra chica: ", n); }

  corte <- floor(n / 2)
  tr <- d[1:corte, ]
  te <- d[(corte + 1):n, ]

  # Pendiente óptima estimada SOLO en train
  fit <- lm(Real ~ Lambda, data = tr)
  a <- coef(fit)[1]; b <- coef(fit)[2]

  # Aplicada a test
  te$Lambda_cal <- a + b * te$Lambda

  mae_modelo   <- mean(abs(te$Real - te$Lambda))
  mae_baseline <- mean(abs(te$Real - mean(tr$Real)))
  mae_calib    <- mean(abs(te$Real - te$Lambda_cal))
  r_test       <- suppressWarnings(cor(te$Lambda, te$Real))

  if (verbose) {
    cat(sprintf("\n🔬 SEÑAL — %s (%s) | n=%d (train %d / test %d)\n",
                md$label, lado, n, nrow(tr), nrow(te)))
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat(sprintf("Correlación λ~real (test) : %+.3f   (r² = %.1f%%)\n", r_test, 100 * r_test^2))
    cat(sprintf("sd(λ) = %.2f | sd(real) = %.2f | ratio = %.2f\n",
                sd(d$Lambda), sd(d$Real), sd(d$Lambda) / sd(d$Real)))
    cat(sprintf("Pendiente óptima          : %.3f\n", b))
    if (b < 0.6) cat(sprintf("   ⚠️ Estás amplificando %.1fx: la mayor parte del spread es ruido.\n", 1 / max(b, 0.05)))
    if (b > 1.3) cat("   ⚠️ Estás sub-extendiendo: el modelo es más informativo de lo que usa.\n")
    cat(sprintf("\nMAE baseline (media liga) : %.3f\n", mae_baseline))
    cat(sprintf("MAE modelo crudo          : %.3f  (%+.3f)\n", mae_modelo, mae_modelo - mae_baseline))
    cat(sprintf("MAE modelo calibrado      : %.3f  (%+.3f)\n", mae_calib, mae_calib - mae_baseline))

    veredicto <- if (mae_calib < mae_baseline - 0.02) {
      "✅ Hay señal aprovechable (calibrando)."
    } else if (mae_calib < mae_baseline) {
      "🟡 Señal marginal. No alcanza para superar el margen de la casa."
    } else {
      "⛔ Sin señal. Este mercado NO se opera."
    }
    cat(sprintf("\n%s\n", veredicto))
  }

  invisible(list(market = market_key, lado = lado, n = n, r = r_test,
                 slope = b, intercept = a,
                 mae_base = mae_baseline, mae_crudo = mae_modelo, mae_calib = mae_calib,
                 gana = mae_calib < mae_baseline - 0.02, datos = d))
}

# ------------------------------------------------------------------------------
# tabla_senal(): barrido por todos los mercados y los tres lados
# ------------------------------------------------------------------------------
tabla_senal <- function(datos = NULL, n_partidos = 300,
                        mercados = c("CORNERS", "REMATES", "ARCO", "TARJETAS",
                                     "FALTAS", "ATAJADAS", "GOLES"),
                        lados = c("T", "L", "V")) {
  out <- list()
  for (mk in mercados) {
    for (ld in lados) {
      r <- try(diagnostico_senal(mk, datos, n_partidos, ld, verbose = FALSE), silent = TRUE)
      if (inherits(r, "try-error")) next
      out[[paste(mk, ld)]] <- data.frame(
        Mercado = market_registry[[mk]]$label, Lado = ld, n = r$n,
        r = round(r$r, 3), pendiente = round(r$slope, 2),
        MAE_base = round(r$mae_base, 3), MAE_calib = round(r$mae_calib, 3),
        Mejora = round(r$mae_base - r$mae_calib, 3),
        Veredicto = if (r$gana) "✅" else if (r$mae_calib < r$mae_base) "🟡" else "⛔"
      )
      message(sprintf("   ✓ %s (%s)", mk, ld))
    }
  }
  tab <- bind_rows(out)
  tab <- tab[order(-tab$Mejora), ]
  cat("\n📋 SEÑAL POR MERCADO (ordenado por mejora sobre baseline)\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  print(tab, row.names = FALSE)
  cat("\n⚠️ Solo operá los ✅. Un 🟡 no cubre el margen de la casa (~5%).\n")
  invisible(tab)
}
