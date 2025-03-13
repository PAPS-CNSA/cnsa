## code to prepare `DATASET` dataset goes here
library(sf)
library(readxl)
library(dplyr)
library(stringr)

# Carte des départements pour cartes dynamiques (?)

liste_departements <- read_excel("data-raw/EPCI_departements_regions_RP_21.xlsx") %>% filter(CODE_DPT != "200046977") %>%
                      mutate(CODE_DPT = str_pad(CODE_DPT, width = 3, side = "left", pad = "0"))

france_shapefile <- st_read("data-raw/departements-20140306-50m.shp") %>% mutate(code_insee = str_pad(code_insee, width = 3, side = "left", pad = "0"))
france_shapefile <- france_shapefile %>% left_join(liste_departements, by = c("code_insee"="CODE_DPT")) %>% select(-wikipedia)
france_shapefile <- france_shapefile %>% mutate(REGION_AGR = case_when(
  code_insee == "971" ~ "971",
  code_insee == "972" ~ "972",
  code_insee == "973" ~ "973",
  code_insee == "974" ~ "974",
  code_insee == "976" ~ "976",
  TRUE ~ "FRANCEMETRO"
))

usethis::use_data(france_shapefile, overwrite = TRUE)

# Carte des départements pour cartes statiques

france_shapefile_departements <- st_read("data-raw/france_shapefile_complet.gpkg") %>%
                                 filter(!(code_insee %in% c("CEA", "20", "69D", "69M"))) %>% mutate(code_insee = str_pad(code_insee, , width = 3, side = "left", pad = "0"))

france_shapefile_departements <- france_shapefile_departements %>% left_join(liste_departements, by = c("code_insee" = "CODE_DPT", "NOM_DEP"))
france_shapefile_departements <- france_shapefile_departements %>% mutate(REGION_AGR = case_when(
  code_insee == "971" ~ "971",
  code_insee == "972" ~ "972",
  code_insee == "973" ~ "973",
  code_insee == "974" ~ "974",
  code_insee == "976" ~ "976",
  TRUE ~ "FRANCEMETRO"
))

usethis::use_data(france_shapefile_departements, overwrite = TRUE)

# Carte des EPCI + départements pour cartes statiques

## Création de la table des EPCI + départements

liste_epci_departements <- read_excel("data-raw/EPCI_departements_regions_RP_21.xlsx")

### On garde la métropole de Lyon et on soustrait la pop de la métropole de Lyon à la pop du Rhône

liste_epci_departements[70,][3] <- "69D"
liste_epci_departements[102,][3] <- "69M"
liste_epci_departements[70,][5] <- liste_epci_departements[70,][5] - liste_epci_departements[102,][5]

### On créé la Communaité européenne d'Alsace

liste_epci_departements[69,][3] <- "CEA"
liste_epci_departements[69,][4] <- "Collectivité européenne d'Alsace"
liste_epci_departements[69,][5] <- liste_epci_departements[69,][5] + liste_epci_departements[68,][5]

### On créé la collectivité de Corse

liste_epci_departements[30,][3] <- "20"
liste_epci_departements[30,][4] <- "Collectivité de Corse"
liste_epci_departements[30,][5] <- liste_epci_departements[30,][5] + liste_epci_departements[29,][5]

### On filtre les départements inutiles

liste_epci_departements <- liste_epci_departements %>% filter(!(CODE_DPT %in% c("67", "2A")))

## Création de la carte

france_shapefile_epci_dept <- st_read("data-raw/france_shapefile_complet.gpkg") %>%
                              filter(!(code_insee %in% c("2A", "2B", "67", "68", "69"))) %>% mutate(code_insee = str_pad(code_insee, , width = 3, side = "left", pad = "0"))


france_shapefile_epci_dept <- france_shapefile_epci_dept %>% left_join(liste_epci_departements, by = c("code_insee" = "CODE_DPT", "NOM_DEP"))
france_shapefile_epci_dept <- france_shapefile_epci_dept %>% mutate(REGION_AGR = case_when(
  code_insee == "971" ~ "971",
  code_insee == "972" ~ "972",
  code_insee == "973" ~ "973",
  code_insee == "974" ~ "974",
  code_insee == "976" ~ "976",
  TRUE ~ "FRANCEMETRO"
))

usethis::use_data(france_shapefile_epci_dept, overwrite = TRUE)

# Carte des regions pour cartes statiques

liste_regions_dept <- liste_departements %>% select(c("REGION", "NOM_REG", "CODE_DPT", "NOM_DEP"))

liste_regions <- liste_departements %>% select(c("REGION", "NOM_REG", "POP", "CODE_DPT", "NOM_DEP")) %>% group_by(REGION, NOM_REG) %>% summarise(POP = sum(POP))

france_shapefile_regions <- st_read("data-raw/france_shapefile_complet.gpkg") %>% filter(!(code_insee %in% c("CEA", "20", "69D", "69M"))) %>% mutate(code_insee = str_pad(code_insee, , width = 3, side = "left", pad = "0"))

france_shapefile_regions <- france_shapefile_regions %>% left_join(liste_regions_dept, by = c("code_insee" = "CODE_DPT", "NOM_DEP"))

france_shapefile_regions <- france_shapefile_regions %>% st_as_sf() %>% group_by(REGION, NOM_REG) %>% summarise(geom = st_union(geom), .groups = "drop") %>% mutate(geom = st_buffer(geom, 0))

france_shapefile_regions <- france_shapefile_regions %>% rename(code_insee = "REGION") %>% mutate(code_insee = str_pad(code_insee, width = 3, side = "left", pad = "0"))

usethis::use_data(france_shapefile_regions, overwrite = TRUE)
