# ==============================================================================
# ARCHIVO: data_pipeline.R — Descarga y cachea datos de Premier League (API-Football)
# ==============================================================================
library(dplyr)
source("config.R")
source("api_client.R")

# --- Normalización de nombres (API-Football devuelve nombres oficiales largos) ---
unificar_nombre <- function(n) {
  n <- as.character(n)
  case_when(
    n %in% c("Manchester United")                ~ "Man United",
    n %in% c("Manchester City")                  ~ "Man City",
    n %in% c("Tottenham Hotspur", "Tottenham")   ~ "Tottenham",
    n %in% c("Nottingham Forest")                ~ "Nott'm Forest",
    n %in% c("Wolverhampton Wanderers", "Wolves")~ "Wolves",
    n %in% c("West Ham United")                  ~ "West Ham",
    n %in% c("Brighton & Hove Albion", "Brighton")~ "Brighton",
    n %in% c("Newcastle United")                 ~ "Newcastle",
    n %in% c("Leeds United")                     ~ "Leeds",
    n %in% c("Ipswich Town")                     ~ "Ipswich",
    n %in% c("Luton Town")                       ~ "Luton",
    n %in% c("Sheffield Utd", "Sheffield United")~ "Sheffield United",
    TRUE ~ n
  )
}

# --- Extrae el valor de un "type" dentro de la lista statistics de un fixture ---
get_stat_value <- function(stats_list, type_name) {
  for (s in stats_list) {
    if (!is.null(s$type) && identical(s$type, type_name)) {
      v <- s$value
      if (is.null(v) || identical(v, "")) return(0)
      if (is.character(v) && grepl("%", v)) v <- gsub("%", "", v)
      val <- suppressWarnings(as.numeric(v))
      return(ifelse(is.na(val), 0, val))
    }
  }
  return(0)
}

# --- Trae todos los fixtures FINALIZADOS de una temporada ---
obtener_fixtures_temporada <- function(season, league_id = PREMIER_LEAGUE_ID) {
  resp <- api_get("fixtures", list(league = league_id, season = season))
  if (is.null(resp) || length(resp$response) == 0) return(NULL)

  filas <- lapply(resp$response, function(f) {
    if (is.null(f$fixture$status$short) || f$fixture$status$short != "FT") return(NULL)
    data.frame(
      fixture_id   = f$fixture$id,
      Date         = as.Date(substr(f$fixture$date, 1, 10)),
      HomeTeam     = unificar_nombre(f$teams$home$name),
      AwayTeam     = unificar_nombre(f$teams$away$name),
      home_team_id = f$teams$home$id,
      away_team_id = f$teams$away$id,
      FTHG         = ifelse(is.null(f$goals$home), 0, f$goals$home),
      FTAG         = ifelse(is.null(f$goals$away), 0, f$goals$away),
      Referee      = ifelse(is.null(f$fixture$referee), NA, f$fixture$referee), # <-- ESTA ES LA LÍNEA NUEVA
      stringsAsFactors = FALSE
    )
  })
  filas <- filas[!sapply(filas, is.null)]
  if (length(filas) == 0) return(NULL)
  bind_rows(filas)
}

# --- Trae y parsea las estadísticas de un fixture puntual ---
obtener_stats_fixture <- function(fixture_id, home_team_id, away_team_id) {
  resp <- api_get("fixtures/statistics", list(fixture = fixture_id))
  if (is.null(resp) || length(resp$response) < 2) return(NULL)
  
  bloque_home <- Filter(function(x) x$team$id == home_team_id, resp$response)
  bloque_away <- Filter(function(x) x$team$id == away_team_id, resp$response)
  if (length(bloque_home) == 0 || length(bloque_away) == 0) return(NULL)
  
  sh <- bloque_home[[1]]$statistics
  sa <- bloque_away[[1]]$statistics
  
  data.frame(
    fixture_id = fixture_id,
    HS      = get_stat_value(sh, "Total Shots"),
    AS      = get_stat_value(sa, "Total Shots"),
    HST     = get_stat_value(sh, "Shots on Goal"),
    AST     = get_stat_value(sa, "Shots on Goal"),
    HF      = get_stat_value(sh, "Fouls"),
    AF      = get_stat_value(sa, "Fouls"),
    HC      = get_stat_value(sh, "Corner Kicks"),
    AC      = get_stat_value(sa, "Corner Kicks"),
    HY      = get_stat_value(sh, "Yellow Cards"),
    AY      = get_stat_value(sa, "Yellow Cards"),
    HR      = get_stat_value(sh, "Red Cards"),
    AR      = get_stat_value(sa, "Red Cards"),
    HSAVES  = get_stat_value(sh, "Goalkeeper Saves"),
    ASAVES  = get_stat_value(sa, "Goalkeeper Saves"),
    HOFF    = get_stat_value(sh, "Offsides"),
    AOFF    = get_stat_value(sa, "Offsides"),
    HPASS   = get_stat_value(sh, "Total passes"),
    APASS   = get_stat_value(sa, "Total passes"),
    HXG     = get_stat_value(sh, "expected_goals"),
    AXG     = get_stat_value(sa, "expected_goals")
  )
}

# --- Pipeline principal: incremental, cachea localmente en .rds ---
actualizar_cache_premier <- function(seasons = TEMPORADAS_ACTIVAS, cache_path = CACHE_PATH,
                                     league_id = PREMIER_LEAGUE_ID) {
  cache <- if (file.exists(cache_path)) readRDS(cache_path) else data.frame()

  fixtures_meta <- lapply(seasons, function(s) obtener_fixtures_temporada(s, league_id))
  fixtures_meta <- bind_rows(fixtures_meta[!sapply(fixtures_meta, is.null)])
  if (nrow(fixtures_meta) == 0) { message("❌ No se pudieron traer fixtures."); return(invisible(cache)) }

  ya_cacheados <- if (nrow(cache) > 0) cache$fixture_id else c()
  pendientes <- fixtures_meta %>% filter(!fixture_id %in% ya_cacheados)

  if (nrow(pendientes) == 0) {
    message("✅ Cache al día, no hay fixtures nuevos.")
    return(invisible(cache))
  }

  message(sprintf("⬇️ Descargando estadísticas de %d partidos nuevos...", nrow(pendientes)))

  filas_nuevas <- vector("list", nrow(pendientes))
  for (i in seq_len(nrow(pendientes))) {
    fx <- pendientes[i, ]
    stats <- obtener_stats_fixture(fx$fixture_id, fx$home_team_id, fx$away_team_id)
    if (!is.null(stats)) {
      filas_nuevas[[i]] <- cbind(fx, stats[, -1, drop = FALSE])
    }
    if (i %% 25 == 0) message(sprintf("   ...%d/%d", i, nrow(pendientes)))
  }
  filas_nuevas <- bind_rows(filas_nuevas[!sapply(filas_nuevas, is.null)])

  cache_actualizado <- bind_rows(cache, filas_nuevas)
  saveRDS(cache_actualizado, cache_path)
  message(sprintf("✅ Cache actualizado: %d partidos totales.", nrow(cache_actualizado)))
  return(invisible(cache_actualizado))
}

# --- Carga el dataset listo para el modelo (weight, cards derivadas, limpieza) ---
cargar_datos_premier <- function(cache_path = CACHE_PATH, auto_update = TRUE,
                                 league_id = PREMIER_LEAGUE_ID) {
  if (auto_update || !file.exists(cache_path)) {
    datos <- actualizar_cache_premier(cache_path = cache_path, league_id = league_id)
  } else {
    datos <- readRDS(cache_path)
  }
  if (is.null(datos) || nrow(datos) < 10) return(NULL)
  
  cols_numericas <- c("FTHG","FTAG","HS","AS","HST","AST","HF","AF","HC","AC",
                      "HY","AY","HR","AR","HSAVES","ASAVES","HOFF","AOFF",
                      "HPASS","APASS","HXG","AXG")
  for (col in cols_numericas) {
    if (!col %in% names(datos)) datos[[col]] <- 0
    datos[[col]][is.na(datos[[col]])] <- 0
  }
  
  datos <- datos %>%
    mutate(
      HomeCards  = HY + HR,
      AwayCards  = AY + AR,
      TotalCards = HY + HR + AY + AR
    ) %>%
    arrange(Date)
  
  return(datos)
}