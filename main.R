# ==============================================================================
# ARCHIVO: main.R — Orquestador
# VERSIÓN: 5.0
#
# USO DIARIO:
#   reporte("ARG", "Banfield", "River Plate")             -> todos los mercados
#   pred("ARG", "Banfield", "River Plate", 10.5, 1.80)    -> una predicción
#   fecha_completa(partidos)                              -> una tanda entera
#   actualizar()                                          -> bajar partidos nuevos
#
# REGLA DE ORO: source("main.R") NUNCA toca la API. Lee de los .rds.
# ==============================================================================
source("config.R")
source("utils.R")
source("data_pipeline.R")
source("market_registry.R")
source("math_core.R")
source("modelo_principal.R")
source("backtests.R")

message("🚀 Pipeline v5.0")

# ------------------------------------------------------------------------------
# Carga desde caché local: 0 requests
# ------------------------------------------------------------------------------
.cargar_cache <- function(var_ruta) {
  if (!exists(var_ruta)) return(NULL)
  ruta <- get(var_ruta)
  if (!file.exists(ruta)) return(NULL)
  cargar_datos_premier(cache_path = ruta, auto_update = FALSE)
}

df_premier <- .cargar_cache("CACHE_PATH")
df_esp     <- .cargar_cache("CACHE_PATH_ESP")
df_ger     <- .cargar_cache("CACHE_PATH_GER")
df_arg     <- .cargar_cache("CACHE_PATH_ARG")
df_champ   <- NULL   # descartado: sin cobertura de cuotas
df_bra     <- NULL   # descartado: sin señal

.n <- function(d) if (is.null(d)) "—" else nrow(d)
message(sprintf("📦 PL: %s | ESP: %s | GER: %s | ARG: %s",
                .n(df_premier), .n(df_esp), .n(df_ger), .n(df_arg)))


# ==============================================================================
# MERCADOS VALIDADOS — clave LIGA_MERCADO_LADO
# Solo estos pasaron el embudo completo. El resto es informativo.
# ==============================================================================
MERCADOS_OK <- c("PL_REMATES_L", "ESP_REMATES_L", "GER_REMATES_L", "ARG_REMATES_L",
                 "ESP_ARCO_L", "ESP_FALTAS_L", "ESP_OFFSIDE_L",
                 "GER_FALTAS_L", "GER_FALTAS_V")

# En registro activo (los otros están validados pero no se anotan todavía)
MERCADOS_REGISTRO <- c("PL_REMATES_L", "ESP_REMATES_L", "GER_REMATES_L", "ARG_REMATES_L")


# ==============================================================================
# .df_liga(): setea LIGA_ACTIVA y devuelve el data frame correcto.
# Es la razón por la que ya no hace falta acordarse de LIGA_ACTIVA.
# ==============================================================================
.df_liga <- function(liga) {
  liga <- toupper(liga)
  LIGA_ACTIVA <<- liga
  df <- switch(liga, PL = df_premier, ESP = df_esp, GER = df_ger, ARG = df_arg,
               stop("Liga no reconocida. Usar: PL, ESP, GER, ARG"))
  if (is.null(df)) stop(sprintf("Sin datos para %s. Corré actualizar('%s').",
                                liga, tolower(liga)))
  df
}


# ==============================================================================
# reporte(): todos los mercados de un partido, con λ CALIBRADO.
#
#   reporte("ARG", "Banfield", "River Plate")
#   reporte("ESP", "Levante", "Real Betis", fecha = "2026-09-06")
#
# La columna de la derecha marca:
#   ✅ validado y en registro   ·   ✓ validado, sin registrar   ·   (vacío) informativo
# ==============================================================================
reporte <- function(liga, local, visita, fecha = Sys.Date(), arbitro = NULL) {
  df <- .df_liga(liga)
  invisible(capture.output(
    res <- analizar(local, visita, datos = df, fecha_partido = fecha,
                    arbitro_nombre = arbitro, enviar = FALSE, verbose = FALSE)
  ))
  if (is.null(res)) { message("⛔ Sin datos suficientes."); return(invisible(NULL)) }
  
  cat(sprintf("\n⚽ %s vs %s — %s (%s)\n", local, visita, liga, format(as.Date(fecha))))
  cat("────────────────────────────────────────────────────────\n")
  cat(sprintf("%-16s %8s %8s %8s\n", "MERCADO", "LOCAL", "VISITA", "TOTAL"))
  cat("────────────────────────────────────────────────────────\n")
  
  marca <- function(mk, lado) {
    clave <- paste0(liga, "_", mk, "_", lado)
    if (clave %in% MERCADOS_REGISTRO) "✅" else if (clave %in% MERCADOS_OK) "✓ " else "  "
  }
  
  for (mk in names(market_registry)) {
    r <- res[[mk]]
    if (is.null(r) || is.null(r$status) || r$status != "OK") next
    cL <- aplicar_calibracion(r$L, mk, "L")
    cV <- aplicar_calibracion(r$V, mk, "V")
    cat(sprintf("%-16s %6.2f %s %6.2f %s %8.2f\n",
                market_registry[[mk]]$label, cL, marca(mk, "L"), cV, marca(mk, "V"), cL + cV))
  }
  cat("────────────────────────────────────────────────────────\n")
  cat("✅ validado y en registro   ✓ validado, sin registrar   (resto: informativo)\n")
  invisible(res)
}


# ==============================================================================
# pred(): una predicción con línea y cuota. La que se usa para el registro.
#
#   pred("ARG", "Banfield", "River Plate", 10.5, 1.80)
#   pred("ESP", "Levante", "Real Betis", 12.5, 1.90,
#        mercado = "faltas local", arbitro = "Gil Manzano")
# ==============================================================================
pred <- function(liga, local, visita, linea, cuota = 0,
                 mercado = "remates local", arbitro = NULL,
                 fecha = Sys.Date(), verbose = TRUE) {
  df <- .df_liga(liga)
  analizar(local, visita, mercado = mercado, linea = linea, cuota_casa = cuota,
           arbitro_nombre = arbitro, datos = df, fecha_partido = fecha,
           enviar = FALSE, verbose = verbose)
}


# ==============================================================================
# fecha_completa(): procesa una tanda y devuelve la tabla lista para el Excel.
#
#   partidos <- data.frame(
#     liga   = c("ARG","ARG","ESP"),
#     local  = c("Banfield","Tigre","Levante"),
#     visita = c("River Plate","Barracas Central","Real Betis"),
#     linea  = c(10.5, 12.5, 11.5),
#     cuota  = c(1.80, 1.72, 1.72)
#   )
#   fecha_completa(partidos)
#
# Columnas opcionales: mercado, arbitro, fecha
# ==============================================================================
fecha_completa <- function(partidos, archivo = "salida_fecha.csv") {
  filas <- lapply(seq_len(nrow(partidos)), function(i) {
    f    <- partidos[i, ]
    merc <- if (!is.null(f$mercado) && !is.na(f$mercado)) as.character(f$mercado) else "remates local"
    arb  <- if (!is.null(f$arbitro) && !is.na(f$arbitro)) as.character(f$arbitro) else NULL
    fch  <- if (!is.null(f$fecha)   && !is.na(f$fecha))   as.character(f$fecha)   else Sys.Date()
    cta  <- if (!is.null(f$cuota)   && !is.na(f$cuota))   f$cuota else 0
    
    r <- try(suppressMessages(invisible(capture.output(
      out <- pred(f$liga, f$local, f$visita, f$linea, cta,
                  mercado = merc, arbitro = arb, fecha = fch, verbose = FALSE)
    ))), silent = TRUE)
    
    base <- data.frame(liga = as.character(f$liga), local = as.character(f$local),
                       visita = as.character(f$visita), linea = f$linea, cuota = cta,
                       stringsAsFactors = FALSE)
    
    if (inherits(r, "try-error") || !exists("out") || is.null(out)) {
      cbind(base, lambda = NA, prob = NA, ev_pct = NA, cv = NA, score = NA,
            pick = FALSE, nota = "equipo no encontrado / sin datos")
    } else {
      # aviso si algún equipo tiene pocos partidos
      df <- .df_liga(f$liga)
      nl <- sum(df$HomeTeam == f$local); nv <- sum(df$AwayTeam == f$visita)
      nota <- if (nl < 15 || nv < 15) sprintf("POCOS DATOS (L=%d V=%d)", nl, nv) else ""
      ev <- if (is.null(out$ev_pct) || is.na(out$ev_pct)) NA else round(out$ev_pct, 2)
      if (!is.na(ev) && ev > EV_SOSPECHOSO) nota <- paste(nota, "⚠️ EV>15% investigar")
      cbind(base, lambda = round(out$lambda, 2), prob = round(out$prob, 3),
            ev_pct = ev, cv = round(out$cv, 2), score = round(out$score, 2),
            pick = isTRUE(out$pick), nota = trimws(nota))
    }
  })
  
  res <- do.call(rbind, filas)
  write.csv(res, archivo, row.names = FALSE)
  cat(sprintf("\n📋 %d partidos | %d picks | guardado en %s\n",
              nrow(res), sum(res$pick, na.rm = TRUE), archivo))
  print(res[, c("liga","local","linea","lambda","prob","ev_pct","score","pick","nota")],
        row.names = FALSE)
  invisible(res)
}


# ==============================================================================
# Utilidades de apoyo
# ==============================================================================
chequear_datos <- function(liga, local, visita, fecha = Sys.Date()) {
  df <- .df_liga(liga)
  fecha <- as.Date(fecha)
  w <- exp(WEIGHT_EXPONENT * as.numeric(fecha - df$Date))
  nl <- sum(w[df$HomeTeam == local & df$Date < fecha])
  nv <- sum(w[df$AwayTeam == visita & df$Date < fecha])
  cat(sprintf("%s de local : %.1f partidos efectivos %s\n", local, nl, if (nl < 8) "⚠️ POCOS" else "✅"))
  cat(sprintf("%s de visita: %.1f partidos efectivos %s\n", visita, nv, if (nv < 8) "⚠️ POCOS" else "✅"))
  invisible(c(local = nl, visita = nv))
}

buscar_equipo <- function(liga, texto) {
  df <- .df_liga(liga)
  eq <- sort(unique(c(df$HomeTeam, df$AwayTeam)))
  res <- grep(texto, eq, value = TRUE, ignore.case = TRUE)
  if (length(res) == 0) { cat("Sin coincidencias:\n"); print(eq) } else print(res)
  invisible(res)
}


# ==============================================================================
# actualizar(): LA ÚNICA FUNCIÓN QUE TOCA LA API
# ==============================================================================
actualizar <- function(liga = c("todas", "pl", "esp", "ger", "arg"), confirmar = TRUE) {
  liga <- match.arg(liga)
  
  st <- tryCatch(api_get("status"), error = function(e) NULL)
  if (is.null(st)) { message("❌ No se pudo consultar la API."); return(invisible(NULL)) }
  usados <- st$response$requests$current
  tope   <- st$response$requests$limit_day
  message(sprintf("📊 API hoy: %d/%d usados | %d disponibles.", usados, tope, tope - usados))
  
  ligas <- list(
    pl  = list(lab = "Premier League", var = "df_premier", ruta = "CACHE_PATH",     id = "PREMIER_LEAGUE_ID"),
    esp = list(lab = "LaLiga",         var = "df_esp",     ruta = "CACHE_PATH_ESP", id = "LALIGA_ID"),
    ger = list(lab = "Bundesliga",     var = "df_ger",     ruta = "CACHE_PATH_GER", id = "BUNDESLIGA_ID"),
    arg = list(lab = "Argentina",      var = "df_arg",     ruta = "CACHE_PATH_ARG", id = "ARGENTINA_ID")
  )
  
  for (k in (if (liga == "todas") names(ligas) else liga)) {
    cfg <- ligas[[k]]
    if (!exists(cfg$ruta) || !exists(cfg$id)) next
    ruta <- get(cfg$ruta); id <- get(cfg$id)
    if (liga == "todas" && !file.exists(ruta)) next
    
    if (!file.exists(ruta) && confirmar) {
      message(sprintf("\n⚠️ Descarga INICIAL de %s (~1100 requests, 6 min).", cfg$lab))
      if (!tolower(trimws(readline("   ¿Continuar? (s/n): "))) %in% c("s","si","sí","y")) next
    }
    
    message(sprintf("\n⬇️ %s...", cfg$lab))
    df <- cargar_datos_premier(cache_path = ruta, league_id = id, auto_update = TRUE)
    assign(cfg$var, df, envir = globalenv())
    message(sprintf("✅ %s: %d partidos | última: %s", cfg$lab, nrow(df), format(max(df$Date))))
  }
  
  st2 <- tryCatch(api_get("status"), error = function(e) NULL)
  if (!is.null(st2)) {
    message(sprintf("\n📊 Gastados: %d | Quedan %d hoy.",
                    st2$response$requests$current - usados,
                    tope - st2$response$requests$current))
  }
  invisible(NULL)
}

aestado_api <- function() {
  st <- tryCatch(api_get("status"), error = function(e) NULL)
  if (is.null(st)) { message("❌ Sin respuesta."); return(invisible(NULL)) }
  r <- st$response$requests
  cat(sprintf("📊 %d/%d usados hoy | %d disponibles\n", r$current, r$limit_day,
              r$limit_day - r$current))
  invisible(r)
}


# ==============================================================================
# verificar_parches(): chequea que las correcciones estén activas
# ==============================================================================
verificar_parches <- function(datos = df_premier) {
  cat("\n🔍 VERIFICACIÓN\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  ok <- function(x) if (x) "✅" else "❌"
  
  arb <- names(sort(table(datos$Referee), decreasing = TRUE))[1]
  d <- datos; d$weight <- 1
  f <- factor_arbitro(d, arb, verbose = FALSE)
  cat(sprintf("%s [1] Árbitro → '%s' (n=%d): cards x%.3f\n", ok(f$n > 0), arb, f$n, f$cards))
  
  cat(sprintf("%s [2] filtrar_rojas OFF en TARJETAS y FALTAS\n",
              ok(!isTRUE(market_registry$TARJETAS$filtrar_rojas) &&
                   !isTRUE(market_registry$FALTAS$filtrar_rojas))))
  
  eq <- datos$HomeTeam[nrow(datos)]
  u <- datos %>% filter(HomeTeam == eq | AwayTeam == eq) %>% arrange(desc(Date)) %>% head(15)
  cat(sprintf("%s [3] Pace usa fechas recientes → %s\n",
              ok(max(u$Date) == max(datos$Date[datos$HomeTeam == eq | datos$AwayTeam == eq])),
              format(max(u$Date))))
  
  eqs <- unique(datos$HomeTeam)
  r <- try(analizar(eqs[1], eqs[2], mercado = "ambos marcan", cuota_casa = 1.9,
                    datos = datos, enviar = FALSE, verbose = FALSE), silent = TRUE)
  cat(sprintf("%s [4] AMBOS MARCAN devuelve resultado\n",
              ok(!inherits(r, "try-error") && !is.null(r))))
  
  r2 <- try(analizar(eqs[1], eqs[2], mercado = "menos de corners", linea = 9.5,
                     cuota_casa = 2.0, datos = datos, enviar = FALSE, verbose = FALSE), silent = TRUE)
  cat(sprintf("%s [5] Etiqueta de under → %s\n",
              ok(!inherits(r2, "try-error") && grepl("MENOS", r2$mercado)),
              if (!inherits(r2, "try-error")) r2$mercado else "error"))
  
  cat(sprintf("%s [6] Calibración aplicada en analizar()\n",
              ok(any(grepl("aplicar_calibracion", deparse(analizar))))))
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
}

message("✅ Listo.  Usar: reporte() · pred() · fecha_completa() · actualizar()")