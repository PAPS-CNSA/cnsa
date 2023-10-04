#' @importFrom leaflet colorNumeric colorFactor
NULL

#' Cette fonction crée une palette de couleur
#'
#' L'objectif est, pour une variable donnée, de créer une palette de couleurs associée qui sera utilisée dans les légendes.
#'
#' @param variable_entree variable au format numérique ou facteur (test à l'intérieur pour adapter). Un seul vecteur par contre
#' @param type_palette type de palette (nom de la palette). par défaut : viridis
#'
#' @return une palette
#' @export
creer_palette <- function(variable_entree, type_palette) {
  # On crée une palette. Si la variable est numérique, c'est une palette continue
  if (is.numeric(variable_entree)) {
    minimum <- min(variable_entree, na.rm = T)
    maximum <- max(variable_entree, na.rm = T)
    palette_couleur <- colorNumeric(palette = type_palette, domain = c(minimum, maximum))

  } else {
    # Sinon, on est sur une variable catégorielle
    palette_couleur <- colorFactor(palette = type_palette, domain = variable_entree)

  }
  return(palette_couleur)
}
