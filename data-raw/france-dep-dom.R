
# Carte de France métro + dom ---------------------------------------------

# remotes::install_github("antuki/CARTElette/CARTElette@RPackage")
library(CARTElette)
library(readxl)
library(dplyr)
library(sf)


# Fond de carte
france_depdom <- charger_carte(nivsupra = "DEP", geometrie_simplifiee = TRUE)
france_depdom <- deplacer_DOM(objet = france_depdom, positions_type = "topleft", zooms = c(1, 1 , 1, 1, 1))
plot(sf::st_geometry(france_depdom))

# Correspondance regions <-> departements
reg_dep <- readxl::read_excel("data-raw/departements_regions.xlsx")


france_depdom <- left_join(
  x = france_depdom,
  y = reg_dep,
  by = c("DEP" = "CODE_DPT")
)



# Enregistrement ----------------------------------------------------------

usethis::use_data(france_depdom, overwrite = TRUE)



