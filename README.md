# Sistema de modelado y validación de predicciones deportivas

Pipeline en R para modelar estadísticas de partidos de fútbol (remates, córners,
faltas, tarjetas) y validar si esos modelos sirven realmente para predecir.

El foco del proyecto no está en construir un modelo, sino en **decidir cuáles
funcionan y cuáles no**. De 115 combinaciones de liga, mercado y lado evaluadas,
9 pasaron el protocolo de validación.

---

## Qué hace

1. **Extracción** — consume la API de api-football.com con caché incremental en
   disco, para no repetir descargas. Actualmente cubre 6 ligas y ~4.500 partidos.

2. **Modelado** — estima el valor esperado (λ) de cada estadística por equipo,
   combinando un enfoque multiplicativo y uno aditivo, con:
   - ponderación temporal exponencial (los partidos viejos pesan menos)
   - contracción hacia la media de liga para equipos con pocos partidos
   - ajuste por la calidad de los rivales enfrentados

3. **Distribución** — convierte λ en probabilidades usando Poisson o binomial
   negativa, según cuánta sobredispersión tenga cada mercado. El parámetro de
   dispersión se estima por máxima verosimilitud sobre residuos.

4. **Validación** — evalúa el modelo con datos que no usó para entrenar,
   respetando el orden temporal.

---

## El protocolo de validación

Un mercado solo se considera utilizable si pasa las seis etapas. Si falla una,
se descarta sin seguir.

| # | Etapa | Qué mide | Criterio |
|---|---|---|---|
| 1 | Señal | ¿el modelo sabe algo? | correlación > 0.30 |
| 2 | Escala | ¿exagera o se queda corto? | pendiente de regresión > 0.85 |
| 3 | Distribución | ¿qué forma tiene el error? | test de sobredispersión |
| 4 | Log loss | ¿le gana a la tasa base? | mejora > 0.03 |
| 5 | Calibración | ¿un 70% ocurre el 70%? | Platt slope entre 0.90 y 1.10 |
| 6 | Estabilidad | ¿se sostiene? | en 2 umbrales y 2 ventanas |

### Resultados

**Pasaron 9 de 115 combinaciones.** El patrón es consistente: solo funcionan
estadísticas de conteo alto que reflejan una característica estable del equipo,
y casi siempre del lado local.

Lo que se descartó y por qué:

- **Totales** (suma de ambos equipos): 11 de 11 fallaron. El modelo predice bien
  el *reparto* entre equipos, pero al sumar los dos lados esa información se
  cancela.
- **Conteos bajos** (goles, tarjetas): poca información por partido, el azar
  domina.
- **Lado visitante**: falló en casi todas las ligas. El equipo local controla más
  el juego y su volumen tiene menos varianza.

Una regla que emergió de los datos: **ningún mercado con pendiente de regresión
menor a 0.85 en la etapa 2 pasó el protocolo completo** (10 de 10 casos). Sirve
como filtro rápido para descartar sin gastar las etapas siguientes.

---

## Estructura

```
api_client.R          Cliente HTTP con manejo de errores y rate limiting
data_pipeline.R       Descarga, caché incremental y normalización
math_core.R           Motor: λ, shrinkage, ajuste por rival, distribuciones
market_registry.R     Definición declarativa de cada mercado
modelo_principal.R    Predicción de un partido concreto
backtests.R           Validación walk-forward y métricas
diagnostico_senal.R   Etapa 1 del protocolo
utils.R               Utilidades
main.R                Orquestador
config_ejemplo.R      Plantilla de configuración
```

---

## Cómo usarlo

```r
# 1. Copiar config_ejemplo.R como config.R y completar la API key
# 2. Cargar
source("main.R")

# 3. Descargar datos (solo la primera vez, ~1100 requests por liga)
actualizar("pl")

# 4. Evaluar si un mercado sirve
diagnostico_senal("REMATES", df_premier, lado = "L")      # etapa 1
estimar_dispersion("REMATES", datos = df_premier, lado = "L")  # etapa 3
backtest_calibracion("REMATES", 12.5, lado = "L", datos = df_premier)  # etapas 4-6

# 5. Predecir un partido
pred("PL", "Arsenal", "Chelsea", linea = 13.5)
```

Requiere `dplyr`, `httr` y `jsonlite`, y una clave de
[api-football.com](https://www.api-football.com/) (tiene plan gratuito).

---

## Notas metodológicas

**Validación walk-forward.** Para predecir un partido del 15 de marzo, el modelo
solo usa partidos anteriores a esa fecha. Después avanza al siguiente y repite.
Sin esto, el modelo estaría usando información que en la práctica no tenía.

**Por qué binomial negativa.** Poisson asume que la varianza es igual a la media.
En remates no lo es: un partido que promedia 14 puede terminar en 8 o en 22. El
parámetro `k` absorbe esa dispersión extra. Se estima maximizando la
verosimilitud sobre los residuos de la validación, no sobre la distribución
marginal de los datos — esa distinción importa, porque el desvío marginal mezcla
la variabilidad del partido con la variabilidad de λ entre partidos, e infla la
varianza estimada.

**Calibración.** Un modelo puede ordenar bien los partidos y aun así estar mal
escalado: decir 70% donde ocurre el 55%. El Platt scaling mide exactamente eso.
Varios mercados pasaron las etapas de señal y log loss y se cayeron acá.

---

## Estado

El proyecto está en fase de registro prospectivo: las predicciones se anotan
antes de cada jornada y se comparan contra los resultados. Es la única forma de
saber si el modelo funciona fuera de la muestra con la que se construyó.

---

## Licencia

MIT
