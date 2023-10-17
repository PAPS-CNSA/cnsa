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
  image1 <- ajouter_titre_image("Guadeloupe", paste0(chemin, "map_01.png"))
  image2 <- ajouter_titre_image("Martinique",paste0(chemin, "map_02.png"))
  image3 <- ajouter_titre_image("Guyane",paste0(chemin, "map_03.png"))
  image4 <- ajouter_titre_image("La Reunion",paste0(chemin, "map_04.png"))
  image5 <- ajouter_titre_image("Mayotte",paste0(chemin, "map_06.png"))
  image6 <- image_read(paste0(chemin, "map_FRANCEMETRO.png"))
  image7 <- ajouter_titre_image("Ile de France",paste0(chemin, "map_11.png"))

  # Création d'une liste pour stocker les images
  liste_images <- c()

  # Ajouter des images à la liste si elles ne sont pas "Image Inexistante"
  if (!is.character(image1)) liste_images <- append(liste_images, image1)
  if (!is.character(image2)) liste_images <- append(liste_images, image2)
  if (!is.character(image3)) liste_images <- append(liste_images, image3)
  if (!is.character(image4)) liste_images <- append(liste_images, image4)
  if (!is.character(image5)) liste_images <- append(liste_images, image5)

  composite_outre_mer <- image_append(liste_images)

  # Obtenir la largeur de l'image composite
  composite_width <- image_info(composite_outre_mer)$width

  # Redimensionner image6 pour qu'elle ait la même largeur que l'image composite
  image6_resized <- image_resize(image6, geometry = paste0(composite_width, "x"))

  # Combinez image6_resized et composite_side_by_side en empilant verticalement
  combined_stack <- image_append(c(image6_resized, composite_outre_mer), stack = TRUE)
  image_write(combined_stack, paste0(chemin, "carte_resultat.jpg"))

}
