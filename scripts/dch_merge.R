library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(stringi)
library(tidylog)

# parametros
anio_inicial <- 2023

# datos
listings <- fread(file = "data/data_dch/raw/listings.csv",
                  encoding = "UTF-8")
crimen_gral <- read_excel(path = "data/data_dch/external/1A DELITOS DE ALTO IMPACTO.xlsx",
                          skip = 1)
crimen_genero <- read_excel(path = "data/data_dch/external/2a-delitos-con-perspectiva-de-genero.xlsx",
                            skip = 1)


# procesamos bases de crimen para tener totales por alcaldia y mes y media mensual anual
## todo alto impacto
crimen_gral <- select(crimen_gral, `FECHA DE LOS HECHOS`, `ALCALDÍA HECHOS`, `TIPO IMPACTO`,
                                    `DELITO`, `MODALIDAD - DELITO`)
crimen_gral <- mutate(crimen_gral, fecha = dmy(`FECHA DE LOS HECHOS`),
                                   mes = floor_date(fecha, "month"),
                                   alcaldia = tolower(`ALCALDÍA HECHOS`),
                                   delito = tolower(DELITO),
                                   modalidad = tolower(`MODALIDAD - DELITO`),
                                   impacto = tolower(`TIPO IMPACTO`))

crimen_gral <- crimen_gral %>% group_by(mes, alcaldia, delito) %>% summarise(impacto = first(impacto),
                                                                             modalidad = first(modalidad),
                                                                             incidencias = n()) %>% ungroup()
crimen_gral_anual <- mutate(crimen_gral, anio = year(mes))
crimen_gral_anual <- crimen_gral_anual %>% group_by(anio, alcaldia, delito) %>% summarise(impacto = first(impacto),
                                                                                          modalidad = first(modalidad),
                                                                                          incidencias = mean(incidencias, na.rm = T)) %>% 
                                                                                ungroup()



## perspectiva de genero
crimen_genero <- select(crimen_genero, `FECHA DE LOS HECHOS`, `ALCALDÍA HECHOS`, `TIPO IMPACTO`,
                        `DELITO`, `MODALIDAD - DELITO`)
crimen_genero <- mutate(crimen_genero, fecha = dmy(`FECHA DE LOS HECHOS`),
                                       mes = floor_date(fecha, "month"),
                                       alcaldia = tolower(`ALCALDÍA HECHOS`),
                                       delito = tolower(DELITO),
                                       modalidad = tolower(`MODALIDAD - DELITO`),
                                       impacto = tolower(`TIPO IMPACTO`))
  
crimen_genero <- crimen_genero %>% group_by(mes, alcaldia, delito) %>% summarise(impacto = first(impacto),
                                                                                 modalidad = first(modalidad),
                                                                                 incidencias = n()) %>% ungroup()
crimen_genero_anual <- mutate(crimen_genero, anio = year(mes))
crimen_genero_anual <- crimen_genero_anual %>% group_by(anio, alcaldia, delito) %>% summarise(impacto = first(impacto),
                                                                                              modalidad = first(modalidad),
                                                                                              incidencias = mean(incidencias, na.rm = T)) %>% ungroup()



# revisamos llaves para merge
unique(listings$neighbourhood)
unique(crimen_gral_anual$alcaldia)
unique(crimen_genero_anual$alcaldia)


# limpiamos caracteres especiales en todas las bases para facilitar merge
## todo alto impacto
crimen_gral_anual <- mutate(crimen_gral_anual, alcaldia_clean = stri_trans_general(alcaldia, "Latin-ASCII"),
                                               alcaldia_clean = str_remove_all(alcaldia_clean, "\\."))
crimen_gral_anual <- filter(crimen_gral_anual, alcaldia_clean != "sin registro")
crimen_gral_anual <- filter(crimen_gral_anual, anio >= anio_inicial)

## perspectiva de genero
crimen_genero_anual <- mutate(crimen_genero_anual, alcaldia_clean = stri_trans_general(alcaldia, "Latin-ASCII"),
                                                   alcaldia_clean = str_remove_all(alcaldia_clean, "\\."))
crimen_genero_anual <- filter(crimen_genero_anual, alcaldia_clean != "sin registro")
crimen_genero_anual <- filter(crimen_genero_anual, anio >= anio_inicial)


## listings
listings <- mutate(listings, neighbourhood_clean = stri_trans_general(neighbourhood, "Latin-ASCII"),
                             neighbourhood_clean = str_remove_all(neighbourhood_clean, "\\."),
                             neighbourhood_clean = tolower(neighbourhood_clean))



# reshape de bases de crimen
## todo alto impacto
crimen_gral_anual_wide <- mutate(crimen_gral_anual, crimen_anio = paste0(delito, "_media_alcaldia_", anio))
crimen_gral_anual_wide <- mutate(crimen_gral_anual_wide, crimen_anio = str_replace_all(crimen_anio, " ", "_"))
crimen_gral_anual_wide <- select(crimen_gral_anual_wide, alcaldia_clean, crimen_anio, incidencias)
crimen_gral_anual_wide <- pivot_wider(crimen_gral_anual_wide, 
                                      names_from = crimen_anio,
                                      values_from = incidencias)

## perspectiva de genero
crimen_genero_anual_wide <- mutate(crimen_genero_anual, crimen_anio = paste0(delito, "_media_alcaldia_", anio))
crimen_genero_anual_wide <- select(crimen_genero_anual_wide, alcaldia_clean, crimen_anio, incidencias)
crimen_genero_anual_wide <- pivot_wider(crimen_genero_anual_wide, 
                                        names_from = crimen_anio,
                                        values_from = incidencias)


# merge
listings1 <- left_join(listings, crimen_gral_anual_wide,
                       by = c("neighbourhood_clean" = "alcaldia_clean"))

listings2 <- left_join(listings, crimen_genero_anual_wide,
                       by = c("neighbourhood_clean" = "alcaldia_clean"))


# exportar
fwrite(listings1, "data/data_dch/processed/listings_delitos_alto_impacto.csv",
       row.names = F)
fwrite(listings1, "data/data_dch/processed/listings_delitos_perspectiva_genero.csv",
       row.names = F)

