## code to prepare `DATASET` dataset goes here
library(sf)
france_shapefile <- st_read("data-raw/departements-20140306-50m.shp")

usethis::use_data(france_shapefile, overwrite = TRUE)
