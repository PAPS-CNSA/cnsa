#' Moyenne filtrée
#'
#' Cette fonction propose de calculer la moyenne filtrée d'un vecteur, i.e.
#' la moyenne sans les valeurs min et max du vecteur
#'
#' @param vecteur un vecteur de données numériques
#'
#' @return un numérique avec la moyenne filtrée
#'
#' @export

moyenne_filtree <- function(vecteur) {
  if (sum(!is.na(vecteur))>1) {
    min_vecteur <- min(vecteur, na.rm = T)
    max_vecteur <- max(vecteur, na.rm = T)
    if (sum(!is.na(vecteur))==2) {
      moyenne <- mean(c(min_vecteur, max_vecteur))
    } else {
      moyenne <- (sum(vecteur, na.rm = T)-min_vecteur-max_vecteur)/(sum(!is.na(vecteur))-2)
    }
  } else {
    moyenne <- -999999999999999999999
  }
  return(moyenne)
}
