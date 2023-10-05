#' Fonction calculer mode
#'
#' Cette fonction calcule la modalité la plus fréquente pour un vecteur de données. Si on lui fournit un argument vide, il ne renvoie rien
#' @param x un vecteur de données
#' @return Un élément avec
#' #' @examples
#' calculer_mode(c(1, 2, 2, 3, 4))
#' @export

calculer_mode <- function(x) {
  # Calcule la modalité la plus fréquente pour un vecteur de données
  if (is.null(x)) {
    mode_value <- x
  } else {
    tbl <- table(x)
    mode_value <- names(tbl)[which.max(tbl)]
  }
  if (is.numeric(x)) {
    mode_value <- as.numeric(mode_value)
  }
  return(mode_value)
}
