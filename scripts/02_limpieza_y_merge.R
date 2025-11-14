library(tidyverse)
library(readr)
library(janitor)

#Cargar anuncios (listings_scrapped)
anuncios <- read_csv("data/raw/listings_scrapped.csv") %>%
  clean_names()

#Limpiar precio y definir colonia
anuncios <- anuncios %>%
  mutate(
    # price viene como texto tipo "$1,234.00"
    price_num = parse_number(price),
    log_price = log(price_num),
    colonia = neighbourhood_cleansed)%>%
  # quitamos precios raros
  filter(!is.na(price_num),price_num > 0)

# quitar 1% +caro para evitar outliers muy extremos
p99 <- quantile(anuncios$price_num, 0.99, na.rm = TRUE)
anuncios <- anuncios %>%
  filter(price_num <= p99)

#Cargar sentimientos LLM por listing
sent_llm <- read_csv("data/processed/review_sentiment_by_listing.csv") %>%
  clean_names()

# asegurarnos que hay solo un registro por listing_id
sent_llm <- sent_llm %>%
  distinct(listing_id, .keep_all = TRUE)

# MERGE: anuncios + sentimientos (id de listings_scrapped = listing_id de sentimientos)
airbnb_panel <- anuncios |>
  left_join(
    sent_llm,
    by = c("id" = "listing_id"))

#seleccionar columnas más relevantes para análisis
airbnb_panel <- airbnb_panel |>
  select(
    # identificadores y ubicación
    id, host_id, colonia, neighbourhood, neighbourhood_cleansed,
    latitude, longitude,
    # tipo de propiedad
    property_type, room_type,
    accommodates, bedrooms, bathrooms, beds,
    # precio y rendimiento
    price_num, log_price,
    estimated_occupancy_l365d, estimated_revenue_l365d,
    # scores oficiales de Airbnb
    review_scores_rating, review_scores_accuracy,
    review_scores_cleanliness, review_scores_communication,
    review_scores_location, review_scores_value,
    # sentimientos LLM
    overall_rating, cleanliness_rating, communication_rating,
    location_rating, value_rating, safety_rating,
    # por si luego queremos fechas
    first_review, last_review, number_of_reviews)

#Guardar panel procesado
dir.create("data/processed", showWarnings = FALSE)

saveRDS(airbnb_panel, "data/processed/airbnb_panel.rds")
write_csv(airbnb_panel, "data/processed/airbnb_panel.csv")

cat("Guardado airbnb_panel en data/processed/airbnb_panel.rds y .csv\n")


