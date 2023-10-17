#' @importFrom dplyr filter
NULL

#' Restriction de la base des cartes en fonction des régions sélectionnées
#'
#' Fonction assez simple, a priori fonction intermédiaire du package (mais laissée en export pour le moment)
#' @param donnees des données issues du shapefile
#' @param region la région sélectionnée (parmi les régions de France + FRANCEMETRO + FRANCEENTIERE)
#' @return le tableau, filtré comme il faut
#' @export

carte_restreindre_base <- function(donnees, region) {
  if (region == "FRANCEMETRO") {
    resultat <- donnees %>% filter(.data$REGION_AGR %in% "FRANCEMETRO")
  } else if (region %in% c("FRANCEENTIERE","FRANCEENTIERE_IDF")) {
    resultat <- donnees
  } else {
    resultat <- donnees %>% filter(.data$REGION == region)
  }
  return(resultat)
}
