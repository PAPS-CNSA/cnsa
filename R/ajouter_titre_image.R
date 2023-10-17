#' @importFrom magick image_read image_annotate image_write
NULL

#' On ajoute un titre sur une image
#'
#' Concrètement, c'est surtout utilisé pour le moment pour ajouter le nom des DOM en haut de l'image
#' D'ailleurs, ni le positionnement, ni la taille, ni quoique ce soit ne sont proposé en paramètre, preuve du côté très limité pour le moment de cette fonction
#'
#' @param titre Titre qu'on souhaite ajouter
#' @param nom_image Nom de l'image (qui doit être préchargée au bon format, on ne charge pas un png ou jpg)
#' @return une image avec un titre
#' @export

ajouter_titre_image <- function(titre, nom_image) {
  if (file.exists(nom_image)) {
    image_chargee <- image_read(nom_image)

    # Définir les propriétés du texte
    text <- titre
    font <- "Arial"
    size <- 80
    color <- "black"

    image_chargee <- image_chargee %>%
      image_annotate(text = text, gravity = "NorthEast", location = "+30-0",
                     font = font, size = size, color = color)


    # Sauvegarder l'image finale (remplacez "output_image.png" par le nom souhaité pour votre image finale)
    image_write(image_chargee, nom_image)

    return(image_chargee)
  } else {
    return("Image inexistante")
  }
}
