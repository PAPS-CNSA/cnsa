## code to prepare `DATASET` dataset goes here
library(sf)
library(readxl)
library(dplyr)

liste_departements <- read_excel("data-raw/departements_regions.xlsx")
france_shapefile <- st_read("data-raw/departements-20140306-50m.shp")
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
