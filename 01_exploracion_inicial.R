# ====================================================
# 01_exploracion_inicial.R
# Proyecto: airbnbDataChallenge
# Objetivo: Exploración inicial de listings + crimen
# Autor: Max
# Fecha: Sys.Date()
# ====================================================

# ----------------------------
# Librerías
# ----------------------------
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(janitor)
library(ggplot2)
library(scales)

# ----------------------------
# Paths
# ----------------------------
path_listings <- "data/data_dch/raw/listings.csv"
path_crimen_alto <- "data/data_dch/processed/listings_delitos_alto_impacto.csv"
path_crimen_genero <- "data/data_dch/processed/listings_delitos_perspectiva_genero.csv"

# ----------------------------
# Cargar datos
# ----------------------------
listings <- fread(path_listings, encoding = "UTF-8") |> clean_names()

crimen_alto <- fread(path_crimen_alto, encoding = "UTF-8") |> clean_names()

crimen_genero <- fread(path_crimen_genero, encoding = "UTF-8") |> clean_names()

print("Datasets cargados correctamente.")

# ----------------------------
# Exploración general listings
# ----------------------------
cat("\nResumen de LISTINGS:\n")
print(glimpse(listings))

cat("\nNAs por variable:\n")
print(sapply(listings, function(x) sum(is.na(x))))

cat("\nDescriptivos de precio:\n")
print(summary(listings$price))

# ----------------------------
# Distribución del precio
# ----------------------------
ggplot(listings, aes(x = price)) +
  geom_histogram(bins = 60, fill = "#0072B2", alpha = 0.8) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "")) +
  labs(
    title = "Distribución de precios (Airbnb CDMX)",
    x = "Precio por noche",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggsave("reports/distribucion_precio.png", width = 7, height = 5)

# ----------------------------
# Densidad del precio
# ----------------------------
ggplot(listings, aes(x = price)) +
  geom_density(fill = "#D55E00", alpha = 0.7) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Densidad de precios",
    x = "Precio",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("reports/densidad_precio.png", width = 7, height = 5)

# ----------------------------
# Exploración básico crimen
# ----------------------------
cat("\nResumen DELITOS DE ALTO IMPACTO:\n")
print(glimpse(crimen_alto))

cat("\nResumen DELITOS - PERSPECTIVA DE GÉNERO:\n")
print(glimpse(crimen_genero))

# =============================
# Serie mensual de delitos de alto impacto (CDMX)
# =============================

# Partimos de crimen_gral (que ya habías creado en dch_merge.R)
# y agregamos al nivel mensual total CDMX:

crimen_mensual <- crimen_gral %>%
  group_by(mes) %>%
  summarise(
    incidencias = sum(incidencias, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(crimen_mensual, aes(x = mes, y = incidencias)) +
  geom_line(color = "#D55E00", linewidth = 1) +
  labs(
    title = "Serie mensual de delitos de alto impacto (CDMX)",
    x = "Mes",
    y = "Número de incidencias"
  ) +
  theme_minimal()

ggsave("reports/serie_crimen_alto.png", width = 8, height = 5)


# ----------------------------
# Merge inicial para ver consistencia (YA NO LO USAMOS)
# ----------------------------
# cat("\nRealizando merge preliminar...\n")

# merge_test <- listings |>
#  mutate(alcaldia = tolower(neighbourhood_group_cleansed)) |>
# left_join(crimen_alto, by = "alcaldia")

# cat("\nMerge exitoso. Vista rápida:\n")
# print(head(merge_test))

# ====================================================
# FIN DEL SCRIPT
# ====================================================
