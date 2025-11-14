library(tidyverse)
library(janitor)
library(skimr)
library(dplyr)

 # Cargar datos 
airbnb_listings <- read_csv("data/raw/listings.csv") %>%
  clean_names()
airbnb_reviews <- read_csv("data/raw/reviews.csv") %>%
  clean_names()



