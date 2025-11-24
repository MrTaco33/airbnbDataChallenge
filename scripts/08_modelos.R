# Modelo a nivel colonia usando el ranking de inversión

library(tidyverse)
library(janitor)

if (!requireNamespace("modelsummary", quietly = TRUE)) {
  install.packages("modelsummary")
}
library(modelsummary)

# 1. Cargar ranking de colonias
col_rank <- read_csv("output/ranking_colonias_inversion.csv") |>
  clean_names()

# 2. Construir logs y limpiar un poco
col_rank <- col_rank |>
  mutate(
    log_ingreso_prom  = log(ingreso_anual_promedio),
    log_precio_prom   = log(precio_promedio),
    log_n_listings    = log(n_listings),
    alcaldia_geo      = as.factor(alcaldia_geo)
  ) |>
  drop_na(log_ingreso_prom, log_precio_prom,
          ocupacion_promedio, dist_transporte_promedio,
          tasa_crimen)

# 3. Especificaciones de modelos

# Modelo 1: muy básico
m_col_1 <- lm(log_ingreso_prom ~ log_precio_prom + ocupacion_promedio,
  data = col_rank)

# Modelo 2: añade transporte y crimen
m_col_2 <- lm(log_ingreso_prom ~ log_precio_prom + ocupacion_promedio +
    dist_transporte_promedio + tasa_crimen,
  data = col_rank)

# Modelo 3: + interacción crimen x transporte
m_col_3 <- lm(
  log_ingreso_prom ~ log_precio_prom + ocupacion_promedio +
    dist_transporte_promedio * tasa_crimen,
  data = col_rank
)

# Modelo 4: + efectos fijos por alcaldía
m_col_4 <- lm(
  log_ingreso_prom ~ log_precio_prom + ocupacion_promedio +
    dist_transporte_promedio * tasa_crimen +
    factor(alcaldia_geo),
  data = col_rank
)

model_list <- list(
  "Básico"                  = m_col_1,
  "+ transporte y crimen"   = m_col_2,
  "+ interacción"           = m_col_3,
  "+ FE alcaldía"           = m_col_4
)

# ==============================
# 4. Tabla de resultados para el informe
# ==============================

# Ruta donde quieres guardar las tablas de regresiones
output_dir <- "output/tablas_regresiones2"

# Crear la carpeta si no existe
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Guardar la tabla HTML de los modelos en esa carpeta
modelsummary(
  model_list,
  output    = file.path(output_dir, "modelo_colonias_ingreso.html"),
  statistic = "({p.value})",
  stars = TRUE,
  gof_omit  = "IC|Log|Adj|F",
  title     = "Determinantes del ingreso anual promedio por colonia"
)
