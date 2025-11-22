
# Gráficas detalladas de precios y crimen
# -------------------------------
# Librerías
# -------------------------------
library(tidyverse)
library(data.table)
library(scales)

# -------------------------------
# 1. Cargar datos procesados
# -------------------------------

# Usamos la base con delitos de alto impacto (ya mergeada)
listings <- fread(
  file = "data/data_dch/processed/listings_delitos_alto_impacto.csv",
  encoding = "UTF-8"
)

# Aseguramos que price sea numérico
listings <- listings %>%
  mutate(
    price = readr::parse_number(as.character(price))
  )

# Quitamos precios faltantes o no positivos
listings <- listings %>%
  filter(!is.na(price), price > 0)

# -------------------------------
# 2. Trimming de outliers de precio
# -------------------------------

# Para que las gráficas no se “aplasten”
p99 <- quantile(listings$price, 0.99, na.rm = TRUE)

cat("Percentil 99 de precio:", round(p99, 2), "\n")

listings_trim <- listings %>%
  filter(price <= p99)

# -------------------------------
# 3. Histograma y densidad (sin outliers)
# -------------------------------

# Histograma
g_hist <- ggplot(listings_trim, aes(x = price)) +
  geom_histogram(binwidth = p99 / 50, fill = "#1f77b4", alpha = 0.7, color = "white") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Distribución de precios (Airbnb CDMX, sin outliers)",
    x = "Precio por noche",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggsave("reports/hist_precio_trim.png", g_hist, width = 8, height = 5)

# Densidad
g_dens <- ggplot(listings_trim, aes(x = price)) +
  geom_density(fill = "#ff7f0e", alpha = 0.7) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Densidad de precios (sin outliers)",
    x = "Precio",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("reports/densidad_precio_trim.png", g_dens, width = 8, height = 5)

# Versión en log-precio
listings_trim <- listings_trim %>%
  mutate(log_price = log(price))

g_dens_log <- ggplot(listings_trim, aes(x = log_price)) +
  geom_density(fill = "#2ca02c", alpha = 0.7) +
  labs(
    title = "Densidad de log(precio)",
    x = "log(Precio)",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("reports/densidad_log_precio.png", g_dens_log, width = 8, height = 5)

# -------------------------------
# 4. Densidad de precios por alcaldía
# -------------------------------

# Usamos la variable 'neighbourhood' como alcaldía
# Nos quedamos con las alcaldías con más observaciones
top_alcaldias <- listings_trim %>%
  count(neighbourhood, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(neighbourhood)

cat("Alcaldías más frecuentes:\n")
print(top_alcaldias)

listings_top <- listings_trim %>%
  filter(neighbourhood %in% top_alcaldias)

g_dens_alc <- ggplot(listings_top, aes(x = price)) +
  geom_density(fill = "#1f77b4", alpha = 0.7) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Densidad de precios por alcaldía (top 8, sin outliers)",
    x = "Precio por noche",
    y = "Densidad"
  ) +
  facet_wrap(~ neighbourhood, scales = "free_y") +
  theme_minimal()

ggsave("reports/densidad_precio_por_alcaldia.png", g_dens_alc, width = 10, height = 6)

# -------------------------------
# 5. Boxplot de precios por alcaldía
# -------------------------------

g_box_alc <- ggplot(listings_top, aes(x = reorder(neighbourhood, price, FUN = median),
                                      y = price)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Boxplot de precios por alcaldía (top 8, sin outliers)",
    x = "Alcaldía",
    y = "Precio por noche"
  ) +
  coord_flip() +
  theme_minimal()

ggsave("reports/boxplot_precio_por_alcaldia.png", g_box_alc, width = 8, height = 6)

# -------------------------------
# 6. Construir un indicador total de crimen 2023
# -------------------------------

# Columnas de crimen: terminan en "media_alcaldia_2023"
crimen_cols_2023 <- names(listings_trim)[grepl("_media_alcaldia_2023$", names(listings_trim))]

cat("Columnas de crimen 2023 usadas:\n")
print(crimen_cols_2023)

listings_trim <- listings_trim %>%
  mutate(
    crimen_total_2023 = rowSums(
      dplyr::across(all_of(crimen_cols_2023), ~ replace_na(., 0)),
      na.rm = TRUE
    )
  )

# -------------------------------
# 7. Scatter: crimen total vs precio
# -------------------------------

g_scatter_crimen <- ggplot(listings_trim,
                           aes(x = crimen_total_2023, y = price)) +
  geom_point(alpha = 0.25, size = 0.8) +
  scale_y_log10(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Crimen vs precio (log-precio, sin outliers)",
    x = "Crimen promedio (total, 2023, por alcaldía)",
    y = "Precio por noche (escala log)"
  ) +
  theme_minimal()

ggsave("reports/scatter_crimen_vs_precio.png", g_scatter_crimen, width = 8, height = 5)

# -------------------------------
# 8. Resumen por alcaldía (para el informe)
# -------------------------------

resumen_alc <- listings_trim %>%
  group_by(neighbourhood) %>%
  summarise(
    n_listings = n(),
    precio_medio = mean(price, na.rm = TRUE),
    precio_mediano = median(price, na.rm = TRUE),
    crimen_total_medio = mean(crimen_total_2023, na.rm = TRUE)
  ) %>%
  arrange(desc(n_listings))

print(head(resumen_alc, 10))

# Puedes exportar este resumen a CSV si quieres:
fwrite(resumen_alc, "reports/resumen_alcaldia_precio_crimen.csv")
