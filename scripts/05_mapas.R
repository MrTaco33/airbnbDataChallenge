#install.packages(c(
#  "tidyverse",  # dplyr, ggplot2, readr...
#  "sf",         # datos espaciales
#  "janitor",    # clean_names()
#  "ggrepel",    # etiquetas que no se enciman
#  "leaflet",    # mapa interactivo
#  "scales"      # rescale() para tamaños de burbujas
#))

library(tidyverse)
library(sf)
library(janitor)
library(ggrepel)
library(scales)
library(dplyr)


listings <- readr::read_csv("data/raw/listings_scrapped.csv") %>%
  janitor::clean_names()


# Tabla de colonias con número de anuncios
colonias_burbujas <- listings |>
  filter(
    !is.na(host_neighbourhood),
    !is.na(latitude),
    !is.na(longitude)
  ) |>
  group_by(host_neighbourhood) |>
  summarise(
    n_listings = n(),
    lat = mean(latitude, na.rm = TRUE),
    lon = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # para que las etiquetas se vean bonitas
  mutate(
    colonia = stringr::str_squish(stringr::str_to_title(host_neighbourhood))
  )

head(colonias_burbujas)


# Convertimos a objeto espacial de puntos
colonias_sf <- st_as_sf(
  colonias_burbujas,
  coords = c("lon", "lat"),
  crs = 4326  # WGS84, estándar
)


# Usando archivo del polígono de neighbourhood
neigh_poly <- st_read("data/raw/neighbourhoods.geojson") |>
  clean_names()

names(neigh_poly)
nrow(neigh_poly)   # te da idea de cuántos polígonos hay (16, 49, etc.)

#=================================================





ggplot() +
  # 1) Fondo: polígonos de neighbourhoods.geojson (delegaciones/barrios oficiales)
  geom_sf(
    data = neigh_poly,
    fill = "grey95",
    color = "white",
    linewidth = 0.2
  ) +
  # 2) Burbujas: colonias agregadas
  geom_sf(
    data = colonias_sf,
    aes(size = n_listings),
    alpha = 0.7
  ) +
  # 3) Escala de tamaño (área proporcional)
  scale_size_area(
    max_size = 18,                             # tamaño máximo en puntos
    breaks   = c(20, 50, 100, 200),            # ajusta a tus datos
    name     = "Número de Airbnbs"
  ) +
  coord_sf() +
  labs(
    title    = "Concentración de anuncios de Airbnb por colonia",
    subtitle = "Burbujas ubicadas en el centro de masa de los anuncios",
    caption  = "Fuente: Inside Airbnb / procesamiento propio"
  ) +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 11),
    plot.caption    = element_text(size = 8, hjust = 0),
    plot.margin     = margin(10, 10, 10, 10)
  )
# si guardas el último plot creado:
ggsave(
  filename = "figs/mapa_concentracion_airbnb_colonias_negro.png",
  width = 8,
  height = 6,
  dpi = 300
)





## Mapa dinámico
library(leaflet)

leaflet(neigh_poly) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    weight = 1,
    color = "#FFFFFF",
    fillOpacity = 0.1
  ) |>
  addCircleMarkers(
    data = colonias_sf,
    radius = ~rescale(n_listings, to = c(4, 20)),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<strong>", colonia, "</strong><br>",
      "Nº de Airbnbs: ", n_listings
    )
  )





  ### MAPA DENSIDAD AIRBNBS AZUL ###
  ggplot() +
  # 1) Fondo: polígonos de neighbourhoods.geojson (delegaciones/barrios oficiales)
  geom_sf(
    data = neigh_poly,
    fill = "grey95",
    color = "white",
    linewidth = 0.2
  ) +
  # 2) Burbujas: colonias agregadas
  geom_sf(
    data = colonias_sf,
    aes(size = n_listings),
    color = "steelblue",   # borde azul
    fill  = "steelblue",   # relleno azul (por si usa shapes con fill)
    alpha = 0.7
  ) +
  # 3) Escala de tamaño (área proporcional)
  scale_size_area(
    max_size = 18,
    breaks   = c(20, 50, 100, 200),
    name     = "Número de Airbnbs"
  ) +
  coord_sf() +
  labs(
    title    = "Concentración de anuncios de Airbnb por colonia",
    subtitle = "Burbujas ubicadas en el centro de masa de los anuncios",
    caption  = "Fuente: Inside Airbnb / procesamiento propio"
  ) +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 11),
    plot.caption    = element_text(size = 8, hjust = 0),
    plot.margin     = margin(10, 10, 10, 10)
  )

# si guardas el último plot creado:
ggsave(
  filename = "figs/mapa_concentracion_airbnb_colonias_AZUL.png",
  width = 8,
  height = 6,
  dpi = 300
)



## Mapa interactivo 

library(leaflet)
library(htmlwidgets)

# 1) Crear el mapa y guardarlo en un objeto
m <- leaflet(neigh_poly) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    weight = 1,
    color = "#FFFFFF",
    fillOpacity = 0.1
  ) |>
  addCircleMarkers(
    data = colonias_sf,
    radius = ~rescale(n_listings, to = c(4, 20)),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<strong>", colonia, "</strong><br>",
      "Nº de Airbnbs: ", n_listings
    )
  )

# 2) Asegurarte de que exista la carpeta figs
dir.create("figs", showWarnings = FALSE)

# 3) Guardar como archivo HTML interactivo
saveWidget(
  widget = m,
  file   = "figs/mapa_airbnb_interactivo.html",
  selfcontained = TRUE  # mete todo (css/js) en un solo archivo
)


############################
##### MAPAS DE PRECIOS #####
############################


library(dplyr)
library(stringr)

listings_clean <- listings |>
  mutate(
    price_num = price |>
      str_replace_all("[$,]", "") |>  # quita $ y comas
      as.numeric()
  ) |>
  filter(!is.na(price_num))


precios_colonia <- listings_clean |>
  group_by(host_neighbourhood) |>
  summarise(
    price_median = median(price_num, na.rm = TRUE),
    n_listings   = n(),
    .groups = "drop"
  )


library(dplyr)

colonias_sf_precio <- colonias_sf |>
  left_join(precios_colonia, by = "host_neighbourhood")


## Mapa de calor 

library(ggplot2)
library(sf)
library(viridis)  # para una escala bonita

ggplot() +
  # Fondo: polígonos grandes (delegaciones / barrios oficiales)
  geom_sf(
    data  = neigh_poly,
    fill  = "grey95",
    color = "white",
    linewidth = 0.2
  ) +
  # Colonias coloreadas por precio
  geom_sf(
    data = colonias_sf_precio,
    aes(fill = price_median),
    color = NA,        # sin borde para que se vea más “heatmap”
    alpha = 0.9
  ) +
  scale_fill_viridis_c(
    option = "magma",  # o "plasma", "viridis"
    direction = -1,
    name = "Precio mediano (MXN)"
  ) +
  coord_sf() +
  labs(
    title    = "Mapa de calor de precios de Airbnb por colonia",
    subtitle = "Color = precio mediano por noche",
    caption  = "Fuente: Inside Airbnb / procesamiento propio"
  ) +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 11),
    plot.caption    = element_text(size = 8, hjust = 0),
    plot.margin     = margin(10, 10, 10, 10)
  )

#=================================
## Intento 3



library(dplyr)
library(stringr)
library(readr)

# Cargar listings desde tu CSV
listings <- read_csv("data/processed/airbnb_panel.csv")  # ajusta la ruta si es otra

# Limpiar precios (de texto tipo "$1,234" a numérico)
listings_clean <- listings |>
  mutate(
    price_num = (price)  # lee números quitando $, comas, etc.
  ) |>
  filter(!is.na(price_num),
         !is.na(host_neighbourhood))

# Precio mediano por host_neighbourhood
precios_host_neigh <- listings_clean |>
  group_by(host_neighbourhood) |>
  summarise(
    price_median = mean(price_num, na.rm = TRUE),
    n_listings   = n(),
    .groups = "drop"
  )

# Unir esto a tus puntos de colonias
# OPCIÓN 1: Renombrar antes del join para evitar conflicto
colonias_sf_precio <- colonias_sf |>
  select(-n_listings) |>  # eliminar la columna anterior
  left_join(precios_host_neigh, by = "host_neighbourhood")

# OPCIÓN 2: Usar suffix para distinguir columnas duplicadas
# colonias_sf_precio <- colonias_sf |>
#   left_join(precios_host_neigh, by = "host_neighbourhood", suffix = c("_old", ""))

# Checar que sí haya precios
summary(colonias_sf_precio$price_median)
sum(is.na(colonias_sf_precio$price_median))



library(ggplot2)
library(sf)
library(viridis)
library(scales)

ggplot() +
  # Fondo: polígonos grandes (delegaciones / barrios oficiales)
  geom_sf(
    data  = neigh_poly,
    fill  = "grey95",
    color = "white",
    linewidth = 0.2
  ) +
  # Puntos (centroides) coloreados por precio mediano
  geom_sf(
    data = colonias_sf_precio,
    aes(color = price_median, size = n_listings),
    alpha = 0.9
  ) +
  scale_size_area(
    max_size = 18,
    breaks   = c(20, 50, 100, 200),
    name     = "Nº de Airbnbs"
  ) +
  scale_color_viridis_c(
    option    = "magma",   # o "plasma", "viridis"
    direction = -1,
    trans = "log",
    name      = "Precio medio (MXN)",
    labels    = scales::comma  # formato con comas para miles
  ) +
  coord_sf() +
  labs(
    title    = "Mapa de calor de precios de Airbnb por colonia",
    subtitle = "Color = precio mediano por noche; tamaño = nº de anuncios",
    caption  = "Fuente: Inside Airbnb / procesamiento propio"
  ) +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 11),
    plot.caption    = element_text(size = 8, hjust = 0),
    plot.margin     = margin(10, 10, 10, 10)
  )

# Guardar
ggsave(
  filename = "figs/mapa_precio_airbnb_colonias.png",
  width = 10,
  height = 8,
  dpi = 300
)


# ======================================

######################
##### MAPA DELITOS ###
######################



library(tidyverse)
library(data.table)
library(sf)
library(viridis)
library(scales)

# ============================================================================
# CARGAR DATOS
# ============================================================================

# Cargar tasas de delito por colonia
tasas_alto_impacto <- fread("data/processed/tasas_delito_alto_impacto_por_colonia.csv")
tasas_genero <- fread("data/processed/tasas_delito_genero_por_colonia.csv")

# Cargar GeoJSON de CDMX (para contexto de alcaldías)
alcaldias_geo <- st_read("data/raw/neighbourhoods.geojson")

# Cargar listings con colonias asignadas
listings_con_delitos <- fread("data/processed/listings_con_tasas_alto_impacto.csv")

# ============================================================================
# PREPARAR DATOS PARA MAPEO
# ============================================================================

# Convertir listings a objeto espacial
listings_sf <- st_as_sf(
  listings_con_delitos,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Asegurar mismo CRS
alcaldias_geo <- st_transform(alcaldias_geo, crs = 4326)

# ============================================================================
# FUNCIÓN PARA CREAR MAPAS
# ============================================================================

crear_mapa_delitos <- function(data, variable_tasa, titulo, 
                               paleta = "YlOrRd", direccion = 1) {
  
  # Filtrar datos válidos
  data_filtrada <- data %>%
    filter(!is.na(!!sym(variable_tasa)))
  
  if (nrow(data_filtrada) == 0) {
    warning("No hay datos para la variable: ", variable_tasa)
    return(NULL)
  }
  
  # Crear mapa
  mapa <- ggplot() +
    # Fondo: límites de alcaldías
    geom_sf(data = alcaldias_geo, 
            fill = "gray95", 
            color = "gray50", 
            linewidth = 0.5,
            alpha = 0.3) +
    # Puntos: colonias coloreadas por tasa de delito
    geom_sf(data = data_filtrada, 
            aes(color = !!sym(variable_tasa)),
            size = 2,
            alpha = 0.7) +
    scale_color_viridis_c(
      option = paleta,
      direction = direccion,
      name = "Tasa de delito",
      labels = percent_format(accuracy = 0.01)
    ) +
    labs(
      title = titulo,
      subtitle = "Cada punto representa la ubicación de un Airbnb",
      caption = "Fuente: Datos de crimen CDMX | Tasa = delitos en colonia / total delitos"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      legend.position = "right",
      panel.grid = element_line(color = "gray90", linewidth = 0.2),
      axis.text = element_text(size = 8)
    )
  
  return(mapa)
}

# ============================================================================
# CREAR MAPAS DE TASAS TOTALES
# ============================================================================

# Identificar columnas de tasas totales en los datos
columnas_disponibles <- names(listings_con_delitos)

# Buscar columnas de tasa total (terminan en _total_tasa_XXXX)
columnas_tasa_total <- grep("_total_tasa_\\d{4}$", columnas_disponibles, value = TRUE)

if (length(columnas_tasa_total) > 0) {
  cat("Columnas de tasa total encontradas:\n")
  print(columnas_tasa_total)
  cat("\n")
  
  # Crear mapa para la tasa total más reciente
  tasa_reciente <- columnas_tasa_total[length(columnas_tasa_total)]
  anio <- str_extract(tasa_reciente, "\\d{4}$")
  
  mapa_total <- crear_mapa_delitos(
    data = listings_sf,
    variable_tasa = tasa_reciente,
    titulo = paste0("Tasa Total de Delitos de Alto Impacto por Colonia (", anio, ")"),
    paleta = "YlOrRd"
  )
  
  print(mapa_total)
  
  # Guardar
  ggsave(
    filename = paste0("figs/mapa_delitos_total_", anio, ".png"),
    plot = mapa_total,
    width = 12,
    height = 10,
    dpi = 300
  )
  
} else {
  warning("No se encontraron columnas de tasa total")
}

# ============================================================================
# CREAR MAPAS DE DELITOS ESPECÍFICOS
# ============================================================================

# Buscar columnas de delitos específicos (no totales)
columnas_delitos_especificos <- grep("_tasa_\\d{4}$", columnas_disponibles, value = TRUE)
columnas_delitos_especificos <- setdiff(columnas_delitos_especificos, columnas_tasa_total)

if (length(columnas_delitos_especificos) > 0) {
  cat("\nDelitos específicos disponibles:\n")
  print(head(columnas_delitos_especificos, 10))
  cat("\n")
  
  # Ejemplo: Mapa de robo
  columnas_robo <- grep("robo.*_tasa_", columnas_delitos_especificos, 
                        value = TRUE, ignore.case = TRUE)
  
  if (length(columnas_robo) > 0) {
    robo_reciente <- columnas_robo[length(columnas_robo)]
    anio_robo <- str_extract(robo_reciente, "\\d{4}$")
    nombre_delito <- str_remove(robo_reciente, "_tasa_\\d{4}$")
    nombre_delito <- str_replace_all(nombre_delito, "_", " ")
    nombre_delito <- str_to_title(nombre_delito)
    
    mapa_robo <- crear_mapa_delitos(
      data = listings_sf,
      variable_tasa = robo_reciente,
      titulo = paste0("Tasa de ", nombre_delito, " por Colonia (", anio_robo, ")"),
      paleta = "Reds"
    )
    
    print(mapa_robo)
    
    # Guardar
    ggsave(
      filename = paste0("figs/mapa_", gsub(" ", "_", tolower(nombre_delito)), "_", anio_robo, ".png"),
      plot = mapa_robo,
      width = 12,
      height = 10,
      dpi = 300
    )
  }
  
  # Ejemplo: Mapa de homicidio
  columnas_homicidio <- grep("homicidio.*_tasa_", columnas_delitos_especificos, 
                             value = TRUE, ignore.case = TRUE)
  
  if (length(columnas_homicidio) > 0) {
    homicidio_reciente <- columnas_homicidio[length(columnas_homicidio)]
    anio_homicidio <- str_extract(homicidio_reciente, "\\d{4}$")
    nombre_delito <- str_remove(homicidio_reciente, "_tasa_\\d{4}$")
    nombre_delito <- str_replace_all(nombre_delito, "_", " ")
    nombre_delito <- str_to_title(nombre_delito)
    
    mapa_homicidio <- crear_mapa_delitos(
      data = listings_sf,
      variable_tasa = homicidio_reciente,
      titulo = paste0("Tasa de ", nombre_delito, " por Colonia (", anio_homicidio, ")"),
      paleta = "Purples"
    )
    
    print(mapa_homicidio)
    
    # Guardar
    ggsave(
      filename = paste0("figs/mapa_", gsub(" ", "_", tolower(nombre_delito)), "_", anio_homicidio, ".png"),
      plot = mapa_homicidio,
      width = 12,
      height = 10,
      dpi = 300
    )
  }
}

# ============================================================================
# MAPA COMPARATIVO: ALTO IMPACTO VS GÉNERO
# ============================================================================

# Cargar también los datos con perspectiva de género
listings_genero <- fread("data/processed/listings_con_tasas_genero.csv")
listings_genero_sf <- st_as_sf(
  listings_genero,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Buscar columnas de tasa total en género
columnas_genero_total <- grep("genero_total_tasa_\\d{4}$", 
                               names(listings_genero), 
                               value = TRUE)

if (length(columnas_genero_total) > 0 && length(columnas_tasa_total) > 0) {
  
  genero_reciente <- columnas_genero_total[length(columnas_genero_total)]
  anio_genero <- str_extract(genero_reciente, "\\d{4}$")
  
  # Mapa de género
  mapa_genero <- crear_mapa_delitos(
    data = listings_genero_sf,
    variable_tasa = genero_reciente,
    titulo = paste0("Tasa Total de Delitos con Perspectiva de Género (", anio_genero, ")"),
    paleta = "Magma"
  )
  
  print(mapa_genero)
  
  # Guardar
  ggsave(
    filename = paste0("figs/mapa_delitos_genero_total_", anio_genero, ".png"),
    plot = mapa_genero,
    width = 12,
    height = 10,
    dpi = 300
  )
}

# ============================================================================
# TABLA RESUMEN DE COLONIAS MÁS PELIGROSAS
# ============================================================================

if (length(columnas_tasa_total) > 0) {
  tasa_reciente <- columnas_tasa_total[length(columnas_tasa_total)]
  
  top_colonias <- listings_con_delitos %>%
    filter(!is.na(!!sym(tasa_reciente))) %>%
    group_by(colonia_geo, alcaldia_geo) %>%
    summarise(
      tasa_promedio = mean(!!sym(tasa_reciente), na.rm = TRUE),
      num_airbnbs = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(tasa_promedio)) %>%
    head(20)
  
  cat("\n=== TOP 20 COLONIAS CON MAYOR TASA DE DELITOS ===\n")
  print(top_colonias, n = 20)
  
  # Guardar tabla
  fwrite(top_colonias, "figs/top_colonias_peligrosas.csv")
}

cat("\n=== MAPAS GENERADOS ===\n")
cat("Los mapas se guardaron en la carpeta 'output/'\n")