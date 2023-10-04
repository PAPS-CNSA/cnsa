#' @importFrom magick image_read image_append image_info image_resize image_append image_write
NULL

#' Cette fonction fusionne toutes les images en une
#'
#' On prend en entrée toutes les cartes, format png, france métropolitaine et DOM
#' Et on assemble tout pour sortir un jpeg
#'
#' @param chemin argument optionnel, répertoire dans lequel on met le résultat
#'
#' @return Une carte au format jpg dans le répertoire souhaité
#' @export


cumuler_cartes <- function(chemin = "") {
  # Fonction qui empile les cartes les unes sur les autres
  image1 <- ajouter_titre_image("Guadeloupe","map_971.png")
  image2 <- ajouter_titre_image("Martinique","map_972.png")
  image3 <- ajouter_titre_image("Guyane","map_973.png")
  image4 <- ajouter_titre_image("La Reunion","map_974.png")
  image5 <- ajouter_titre_image("Mayotte","map_976.png")
  image6 <- image_read("map_FRANCEMETRO.png")

  composite_side_by_side <- image_append(c(image1, image2, image3, image4, image5))

  # Obtenir la largeur de l'image composite
  composite_width <- image_info(composite_side_by_side)$width

  # Redimensionner image6 pour qu'elle ait la même largeur que l'image composite
  image6_resized <- image_resize(image6, geometry = paste0(composite_width, "x"))

  # Combinez image6_resized et composite_side_by_side en empilant verticalement
  combined_stack <- image_append(c(image6_resized, composite_side_by_side), stack = TRUE)
  image_write(combined_stack, paste0(chemin, "carte_resultat.jpg"))

}
