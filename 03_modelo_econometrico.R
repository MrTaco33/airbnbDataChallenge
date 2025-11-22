
# Objetivo: Modelar el efecto del crimen sobre el precio de Airbnb
#           usando precio en nivel y log-precio

# ==============================
# 0. Librerías
# ==============================
library(tidyverse)
library(data.table)
library(janitor)

# Si no tienes modelsummary, se instala una vez:
if (!requireNamespace("modelsummary", quietly = TRUE)) {
  install.packages("modelsummary")
}
library(modelsummary)

# ==============================
# 1. Cargar datos procesados
# ==============================
listings <- fread(
  "data/data_dch/processed/listings_delitos_alto_impacto.csv",
  encoding = "UTF-8"
)

# ==============================
# 2. Limpieza de variables clave
# ==============================

# --- Precio: convertir a numérico limpio
# (ajusta "price" si tu columna tiene otro nombre)
listings <- listings |>
  mutate(
    price_num = price |>
      as.character() |>
      str_replace_all("\\$", "") |>
      str_replace_all(",", "") |>
      as.numeric()
  ) |>
  filter(!is.na(price_num), price_num > 0)

# --- Trimming de extremos de precio (1% y 99%)
q_low  <- quantile(listings$price_num, 0.01, na.rm = TRUE)
q_high <- quantile(listings$price_num, 0.99, na.rm = TRUE)

listings <- listings |>
  filter(price_num >= q_low, price_num <= q_high)

# --- Log-precio
listings <- listings |>
  mutate(log_price = log(price_num))

# --- Variable de alcaldía (para FE)
# Probamos distintos nombres posibles
if ("alcaldia" %in% names(listings)) {
  listings <- listings |> mutate(alcaldia = as.factor(alcaldia))
} else if ("neighbourhood_group_cleansed" %in% names(listings)) {
  listings <- listings |> mutate(alcaldia = as.factor(neighbourhood_group_cleansed))
} else if ("neighbourhood_clean" %in% names(listings)) {
  listings <- listings |> mutate(alcaldia = as.factor(neighbourhood_clean))
} else {
  stop("No encuentro columna de alcaldía / neighbourhood; ajusta esta parte del script.")
}

# --- Variable resumida de crimen: suma de todos los delitos por alcaldía
# columnas que terminan en _media_alcaldia_2023 (o en general en 'media_alcaldia')
crime_cols <- listings |>
  select(ends_with("_media_alcaldia_2023")) |>
  colnames()

if (length(crime_cols) == 0) {
  crime_cols <- listings |>
    select(contains("media_alcaldia")) |>
    colnames()
}

if (length(crime_cols) == 0) {
  stop("No encontré columnas de crimen *_media_alcaldia_*; revisa nombres de variables.")
}

listings <- listings |>
  mutate(
    crimen_total = rowSums(across(all_of(crime_cols)), na.rm = TRUE)
  )

# --- Controles (ajusta según lo que tenga tu base)
control_vars <- c("accommodates", "bedrooms", "bathrooms", "minimum_nights")
control_vars <- control_vars[control_vars %in% names(listings)]

# Armamos data final para el modelo
model_data <- listings |>
  select(price_num, log_price, crimen_total, alcaldia, all_of(control_vars)) |>
  drop_na(price_num, log_price, crimen_total)

# ==============================
# 3. Especificaciones de modelos
# ==============================

# Fórmulas dinámicas para no romper si falta algún control
controls_rhs <- if (length(control_vars) == 0) {
  ""
} else {
  paste("+", paste(control_vars, collapse = " + "))
}

# ---- Modelos con precio en nivel ----
f_nivel_1  <- as.formula("price_num ~ crimen_total")
f_nivel_2  <- as.formula(paste("price_num ~ crimen_total", controls_rhs))
f_nivel_3  <- as.formula(paste("price_num ~ crimen_total", controls_rhs, "+ factor(alcaldia)"))

m_nivel_1 <- lm(f_nivel_1, data = model_data)   # simple
m_nivel_2 <- lm(f_nivel_2, data = model_data)   # + controles
m_nivel_3 <- lm(f_nivel_3, data = model_data)   # + controles + FE alcaldía

# ---- Modelos con log-precio ----
f_log_1  <- as.formula("log_price ~ crimen_total")
f_log_2  <- as.formula(paste("log_price ~ crimen_total", controls_rhs))
f_log_3  <- as.formula(paste("log_price ~ crimen_total", controls_rhs, "+ factor(alcaldia)"))

m_log_1 <- lm(f_log_1, data = model_data)
m_log_2 <- lm(f_log_2, data = model_data)
m_log_3 <- lm(f_log_3, data = model_data)

# ==============================
# 4. Resumen de resultados
# ==============================

model_list <- list(
  "Nivel: simple"              = m_nivel_1,
  "Nivel: + controles"         = m_nivel_2,
  "Nivel: + controles + FE"    = m_nivel_3,
  "Log: simple"                = m_log_1,
  "Log: + controles"           = m_log_2,
  "Log: + controles + FE"      = m_log_3
)

# Tabla en HTML (para el informe)
modelsummary(
  model_list,
  output   = "reports/modelos_precio_crimen.html",
  statistic = "({p.value})",
  gof_omit  = "IC|Log|Adj|F"
)

# También una versión en texto en la consola
modelsummary(model_list)
