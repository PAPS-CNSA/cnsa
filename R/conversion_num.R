#' Convertit une entrée texte en un nombre numérique.
#'
#' Cette fonction prend en charge la conversion des entrées texte contenant des virgules,
#' des points-virgules, des pourcentages et des espaces, les transformant en une forme numérique appropriée.
#'
#' @param x Une entrée texte à convertir.
#'
#' @return Une valeur numérique correspondant à l'entrée texte.
#' @export
#'
#' @examples
#' # Exemple d'utilisation:
#' entree_texte <- "1,234.56 %"
#' # Appel de la fonction :
#' valeur_numerique <- conversion_num(entree_texte)


conversion_num <- function(x) {
  x <- gsub("[,;]", ".", x)
  x <- gsub("%", "", x)
  x <- gsub(" ", "", x)
  x <- as.numeric(x)
  return(x)
}
