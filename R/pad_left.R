#' pad_left
#'
#' @param x chaine de caractère
#' @param len longueur cible de la sortie
#' @param char caractère à insérer pour compléter
#'
#' @return renvoie une chaine de caractères
#' @export
#'
#' @examples
#' pad_left("bonjour",10,"0")
pad_left <- function(x, len = 1 + max(nchar(x)), char = '0'){
  # Fonction qui complète une chaine de caractère x jusque à la longueur len (défaut = max +1 ) avec le contenu de char (défaut 0)
  unlist(lapply(x, function(x) {
    paste0(
      paste(rep(char, len - nchar(x)), collapse = ''),
      x
    )
  }))
}
