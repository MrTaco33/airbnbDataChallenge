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
listings <- read_csv("data/raw/airbnb_panel.csv")  # ajusta la ruta si es otra

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
    price_median = median(price_num, na.rm = TRUE),
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
    name      = "Precio mediano (MXN)",
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