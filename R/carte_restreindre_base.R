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
  dom <- c('01','02','03','04','05','06')
  if ("FRANCEMETRO" %in% region) {
    if (any(dom %in% region)) {
      dom_absents <- dom[!dom %in% region]
      resultat <- donnees %>% filter(!(.data$REGION_AGR %in% dom_absents ))
    } else {
      resultat <- donnees %>% filter(.data$REGION_AGR %in% "FRANCEMETRO")
    }
  } else if (any(c("FRANCEENTIERE","FRANCEENTIERE_IDF") %in% region)) {
    resultat <- donnees
  } else {
    resultat <- donnees %>% filter(.data$REGION %in% region)
  }
  return(resultat)
}
