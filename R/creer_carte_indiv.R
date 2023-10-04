#' @importFrom dplyr filter
#' @importFrom sf st_bbox
#' @importFrom leaflet leaflet addLegend addPolygons
#' @importFrom leaflet.extras setMapWidgetStyle
NULL

#' Creer carte individuelle
#'
#' Cette fonction crée une carte pour une région donnée (FRANCEMETRO, ou un département d'outre mer)
#'
#' @param donnees un tableau de données, format SF, avec les coordonnées
#' @param region la région souhaitée, incluse dans le SF (a priori FRANCEMETRO, 971, 972,...)
#' @param palette une palette de couleurs, fournie a priori avant (note : peut être à changer)
#' @param titre_legende Si la légende s'affiche : qu'est-il marqué au dessus ?
#'
#' @return une carte format leaflet
#' @export
#'
creer_carte_indiv <- function(donnees, region, palette, titre_legende = "Legende") {
  # Fonction qui créée une carte pour une région donnée, avec une palette déjà prédéfinie
  data <- donnees %>% filter(.data$REGION == region)

  bbox <- st_bbox(data)

  if (is.numeric(data$VALEUR)) {
    carte <- leaflet(data) %>%
      addPolygons(
        fillColor = ~palette(VALEUR),
        fillOpacity = 0.8,
        color = "white",
        weight = 1
      ) %>%
      setMapWidgetStyle(list(background= "white"))
  } else {
    carte <- leaflet(data) %>%
      addPolygons(
        fillColor = ~palette(as.factor(VALEUR)),
        fillOpacity = 0.8,
        color = "white",
        weight = 1
      ) %>%
      setMapWidgetStyle(list(background= "white"))
  }


  if (region == "FRANCEMETRO") {
    carte <- carte %>% addLegend(pal = palette, values = ~VALEUR, title = titre_legende)
  }

  return(carte)
}
