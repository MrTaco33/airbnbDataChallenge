library(tidyverse)
library(janitor)
library(skimr)
library(dplyr)

 # Cargar datos 
#listings
airbnb_listings <- read_csv("data/raw/listings.csv") %>%
  clean_names()

#listings_scrapped
airbnb_listings_scraped <- read_csv("data/raw/listings_scrapped.csv") %>%
  clean_names() %>%
 airbnb_listings_scraped <- read_csv("data/raw/listings_scrapped.csv") %>%
  clean_names() %>%
  mutate(
    price_num = parse_number(price)) %>%
  filter(!is.na(price_num),
    price_num > 0,
    price_num < 10000)

summary(airbnb_listings_scraped$price_num)

#histograma de dist. del precio por noche
ggplot(airbnb_listings_scraped, aes(x = price_num)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "white") +  
  coord_cartesian(xlim = c(0, 5000)) +
  labs(title = "Distribución del precio por noche (Airbnb CDMX)",
    x = "Precio por noche (MXN)",
    y = "Frecuencia") +
  theme_minimal()

#visualización rápida
glimpse(airbnb_listings_scraped)
skimr::skim(airbnb_listings_scraped$price)

# Top colonias por número de anuncios
airbnb_listings_scraped %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  slice_head(n = 20)


