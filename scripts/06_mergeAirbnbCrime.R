library(tidyverse)
library(data.table)
library(lubridate)
library(stringi)
library(tidylog)
library(sf)

# ============================================================================
# PARÁMETROS
# ============================================================================
anio_inicial <- 2023

# ============================================================================
# CARGAR DATOS
# ============================================================================

# Airbnb - usando listings_scrapped.csv con más detalle
listings <- fread(file = "data/raw/listings_scrapped.csv", encoding = "UTF-8")

# Delitos - ahora desde las bases limpias en data/external
crimen_gral <- fread(file = "data/external/delitos_alto_impacto_clean.csv", encoding = "UTF-8")
crimen_genero <- fread(file = "data/external/delitos_genero_clean.csv", encoding = "UTF-8")

# GeoJSON de colonias
colonias_geo <- st_read("data/raw/09-Cdmx.geojson")

crimen_gral$fecha

# ============================================================================
# PROCESAR DATOS DE CRIMEN
# ============================================================================

# Función para procesar bases de crimen
procesar_crimen <- function(df, nombre_base) {
  
  # Convertir fecha y crear variables temporales
  df <- df %>%
    mutate(
      fecha = dmy(fecha),
      mes = floor_date(fecha, "month"),
      anio = year(fecha),
      # Limpiar nombres de colonia y alcaldía
      colonia_clean = tolower(colonia_hechos),
      colonia_clean = stri_trans_general(colonia_clean, "Latin-ASCII"),
      colonia_clean = str_remove_all(colonia_clean, "\\."),
      colonia_clean = str_trim(colonia_clean),
      alcaldia_clean = tolower(alcaldia_hechos),
      alcaldia_clean = stri_trans_general(alcaldia_clean, "Latin-ASCII"),
      alcaldia_clean = str_remove_all(alcaldia_clean, "\\."),
      alcaldia_clean = str_trim(alcaldia_clean),
      delito_clean = tolower(delito),
      delito_clean = str_replace_all(delito_clean, " ", "_")
    ) %>%
    # Filtrar registros sin colonia y años >= anio_inicial
    filter(
      colonia_clean != "sin registro",
      !is.na(colonia_clean),
      anio >= anio_inicial
    )
  
  return(df)
}

# Procesar ambas bases
crimen_gral_proc <- procesar_crimen(crimen_gral, "alto_impacto")
crimen_genero_proc <- procesar_crimen(crimen_genero, "genero")

# ============================================================================
# CALCULAR TASAS DE DELITO POR COLONIA
# ============================================================================

calcular_tasas_colonia <- function(df, prefijo) {
  
  # 1. Contar delitos por colonia y tipo de delito
  delitos_colonia <- df %>%
    group_by(anio, alcaldia_clean, colonia_clean, delito_clean) %>%
    summarise(
      incidencias_colonia = n(),
      .groups = "drop"
    )
  
  # 2. Contar total de cada delito en toda la ciudad
  delitos_total <- df %>%
    group_by(anio, delito_clean) %>%
    summarise(
      incidencias_total = n(),
      .groups = "drop"
    )
  
  # 3. Unir y calcular tasa
  tasas <- delitos_colonia %>%
    left_join(delitos_total, by = c("anio", "delito_clean")) %>%
    mutate(
      tasa_delito = incidencias_colonia / incidencias_total,
      # Crear nombre de variable para pivot
      var_name = paste0(prefijo, "_", delito_clean, "_tasa_", anio)
    )
  
  # 4. Calcular tasa total por colonia (todos los delitos agregados)
  tasa_total_colonia <- tasas %>%
    group_by(anio, alcaldia_clean, colonia_clean) %>%
    summarise(
      incidencias_colonia_total = sum(incidencias_colonia),
      .groups = "drop"
    )
  
  tasa_total_ciudad <- df %>%
    group_by(anio) %>%
    summarise(
      incidencias_total = n(),
      .groups = "drop"
    )
  
  tasa_total <- tasa_total_colonia %>%
    left_join(tasa_total_ciudad, by = "anio") %>%
    mutate(
      tasa_total = incidencias_colonia_total / incidencias_total,
      var_name = paste0(prefijo, "_total_tasa_", anio)
    )
  
  # 5. Pivot a formato wide para tasas por delito específico
  tasas_wide <- tasas %>%
    select(alcaldia_clean, colonia_clean, var_name, tasa_delito) %>%
    pivot_wider(
      names_from = var_name,
      values_from = tasa_delito,
      values_fill = 0
    )
  
  # 6. Pivot a formato wide para tasa total
  tasa_total_wide <- tasa_total %>%
    select(alcaldia_clean, colonia_clean, var_name, tasa_total) %>%
    pivot_wider(
      names_from = var_name,
      values_from = tasa_total,
      values_fill = 0
    )
  
  # 7. Combinar ambas
  resultado <- tasas_wide %>%
    left_join(tasa_total_wide, by = c("alcaldia_clean", "colonia_clean"))
  
  return(resultado)
}

# Calcular tasas para ambas bases
tasas_gral <- calcular_tasas_colonia(crimen_gral_proc, "alto_impacto")
tasas_genero <- calcular_tasas_colonia(crimen_genero_proc, "genero")

# ============================================================================
# PROCESAR LISTINGS Y HACER MATCH CON COLONIAS
# ============================================================================

# Limpiar y preparar listings
listings_clean <- listings %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  mutate(
    neighbourhood_clean = tolower(neighbourhood_cleansed),
    neighbourhood_clean = stri_trans_general(neighbourhood_clean, "Latin-ASCII"),
    neighbourhood_clean = str_remove_all(neighbourhood_clean, "\\."),
    neighbourhood_clean = str_trim(neighbourhood_clean)
  )

# Convertir listings a objeto espacial (sf)
listings_sf <- st_as_sf(
  listings_clean,
  coords = c("longitude", "latitude"),
  crs = 4326  # WGS84
)

# Asegurar que colonias_geo tenga el mismo CRS
colonias_geo <- st_transform(colonias_geo, crs = 4326)

# Hacer spatial join para asignar colonia a cada Airbnb
# Ajusta los nombres de columnas según tu GeoJSON
# Suponiendo que el GeoJSON tiene columnas 'nomcolonia' y 'nomalcaldia'
listings_con_colonia <- st_join(listings_sf, colonias_geo, join = st_within)

# Convertir de vuelta a dataframe y limpiar nombres de colonia
listings_con_colonia <- listings_con_colonia %>%
  st_drop_geometry() %>%
  mutate(
    # AJUSTA ESTOS NOMBRES según las columnas de tu GeoJSON
    colonia_geo = tolower(nomcolonia),  # Cambiar 'nomcolonia' por el nombre real
    colonia_geo = stri_trans_general(colonia_geo, "Latin-ASCII"),
    colonia_geo = str_remove_all(colonia_geo, "\\."),
    colonia_geo = str_trim(colonia_geo),
    alcaldia_geo = tolower(nomalcaldia),  # Cambiar 'nomalcaldia' por el nombre real
    alcaldia_geo = stri_trans_general(alcaldia_geo, "Latin-ASCII"),
    alcaldia_geo = str_remove_all(alcaldia_geo, "\\."),
    alcaldia_geo = str_trim(alcaldia_geo)
  )

# ============================================================================
# MERGE CON TASAS DE DELITO
# ============================================================================

# Merge con tasas de delitos de alto impacto
listings_final_gral <- listings_con_colonia %>%
  left_join(
    tasas_gral,
    by = c("alcaldia_geo" = "alcaldia_clean", "colonia_geo" = "colonia_clean")
  )

# Merge con tasas de delitos con perspectiva de género
listings_final_genero <- listings_con_colonia %>%
  left_join(
    tasas_genero,
    by = c("alcaldia_geo" = "alcaldia_clean", "colonia_geo" = "colonia_clean")
  )

# ============================================================================
# EXPORTAR RESULTADOS
# ============================================================================

# Crear directorio processed si no existe
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Exportar listings con tasas de delito
fwrite(
  listings_final_gral,
  "data/processed/listings_con_tasas_alto_impacto.csv",
  row.names = FALSE
)

fwrite(
  listings_final_genero,
  "data/processed/listings_con_tasas_genero.csv",
  row.names = FALSE
)

# Exportar también las tasas por colonia (sin listings) como referencia
fwrite(
  tasas_gral,
  "data/processed/tasas_delito_alto_impacto_por_colonia.csv",
  row.names = FALSE
)

fwrite(
  tasas_genero,
  "data/processed/tasas_delito_genero_por_colonia.csv",
  row.names = FALSE
)

# ============================================================================
# RESUMEN
# ============================================================================

cat("\n=== RESUMEN DE PROCESAMIENTO ===\n")
cat("Airbnbs procesados:", nrow(listings_clean), "\n")
cat("Airbnbs con colonia asignada:", sum(!is.na(listings_con_colonia$colonia_geo)), "\n")
cat("Colonias únicas en delitos alto impacto:", n_distinct(tasas_gral$colonia_clean), "\n")
cat("Colonias únicas en delitos género:", n_distinct(tasas_genero$colonia_clean), "\n")
cat("\nArchivos generados en data/processed/\n")