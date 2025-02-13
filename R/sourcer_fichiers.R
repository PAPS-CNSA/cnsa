#' Source l'ensemble des fichiers R d'un répertoire donné
#'
#' @param repertoire adresse du répertoire contenant les fichiers
#' @return rien du tout, si ce n'est des messages dans la console
#' @export
#'

sourcer_fichiers <- function(repertoire) {
  liste_fichiers <- list.files(path = repertoire, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(liste_fichiers, function(f) {
    tryCatch({
      source(f, echo = FALSE)
      message(paste0("Chargement du fichier ", f, " effectué"))
    }, error = function(e) {
      message(paste("Erreur avec le fichier :", f, "\nMessage :", e$message))
    })
  }))

}
