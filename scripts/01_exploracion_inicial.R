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
  mutate(
    price_num = parse_number(price)) %>%
  filter(!is.na(price_num),
    price_num > 0,
    price_num < quantile(price_num, 0.99, na.rm = TRUE)) #para quitar el 1% +caro

summary(airbnb_listings_scraped$price_num)
glimpse(airbnb_listings_scraped)
skimr::skim(airbnb_listings_scraped$price)

#Precio mediano por colonia
precio_colonia <- airbnb_listings_scraped %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(n=n(), mediana_precio=median(price_num, na.rm = TRUE)) %>%
  filter(n>=30) %>%
  arrange(desc(mediana_precio))

#gráfica
ggplot(precio_colonia,
       aes(x = reorder(neighbourhood_cleansed, mediana_precio),
           y = mediana_precio)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Precio mediano por colonia (n ≥ 30)",
    x = "Colonia",
    y = "Precio mediano por noche (MXN)"
  ) +
  theme_minimal()

#Histograma dist. del precio x noche
ggplot(airbnb_listings_scraped, aes(x = price_num)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "white") +  
  coord_cartesian(xlim = c(0, 5000)) +
  labs(
    title = "Distribución del precio por noche (Airbnb CDMX)",
    x = "Precio por noche (MXN)",
    y = "Frecuencia"
  ) +
  theme_minimal()


#Calidad percibida: score de airbnb y sentimientos LLM
#Tenemos que mergear review_sentiment_by_listing con listings_scrapped (en airbnb_panel)
