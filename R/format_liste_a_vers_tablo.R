#' @importFrom data.table rbindlist
NULL

#' Format liste Années vers Liste variables
#'
#' Cette fonction transforme une liste de variables (chaque élément de la liste = une variable, chaque colonne = une année)
#' en une liste de variables
#' @param liste_annees Une liste de dataframes, chaque élément de la liste représentant une année. La première colonne représente la variable identifiante
#' @return Un tableau, avec une colonne "ANNE"
#' @export

format_liste_a_vers_tablo <- function(liste_a,
                                      var_liste = "ANNEE",
                                      format_sortie = "data.frame") {
  # Permet de passer d'un format liste variable à un format tablo

  tablo <- rbindlist(liste_a, idcol = var_liste)
  if (format_sortie == "data.frame") tablo <- as.data.frame(tablo)

  return(tablo)
}
