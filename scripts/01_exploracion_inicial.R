library(tidyverse)
library(janitor)
library(skimr)
library(dplyr)

 # Cargar datos 
airbnb_listings <- read_csv("data/raw/listings.csv") %>%
  clean_names()
airbnb_listings_scraped <- read_csv("data/raw/listings_scrapped.csv") %>%
  clean_names()
airbnb_reviews <- read_csv("data/raw/reviews.csv") %>%
  clean_names()

glimpse(airbnb_listings_scraped)
skimr::skim(airbnb_listings_scraped$price)


# Histograma de precios (listings_scrapped)
airbnb_listings_scraped %>%
  mutate(price_num = readr::parse_number(price)) %>%
  ggplot(aes(x = price_num)) +
  geom_histogram(bins = 60) +
  coord_cartesian(xlim = c(0, 5000)) +
  labs(
    title = "Distribución del precio por noche (Airbnb CDMX)",
    x = "Precio por noche (MXN)",
    y = "Frecuencia"
  )

# Top colonias por número de anuncios
airbnb_listings_scraped %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  slice_head(n = 20)


