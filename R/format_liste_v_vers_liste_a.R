#' @importFrom tidyr gather spread
#' @importFrom dplyr mutate bind_rows group_by group_split select
NULL

#' Format liste Années vers Liste variables
#'
#' Cette fonction transforme une liste de variables (chaque élément de la liste = une variable, chaque colonne = une année)
#' en une liste de variables
#' @param liste_v Une liste de dataframes, chaque élément de la liste représentant une variable La première colonne représente la variable identifiante
#' @param variable_ident le nom de la variable identifiante. Par défaut, "FINESS"
#' @param variable_temporelle le nom de la variable année. Par défaut, "ANNEE"
#' @return Une liste de dataframes, cette fois chaque élément représente une année
#' @export

format_liste_v_vers_liste_a <- function(liste_v, variable_ident= "FINESS", variable_temporelle="ANNEE", format_sortie="data.frame") {
  tablo <- format_liste_v_vers_tablo(liste_v, variable_ident, variable_temporelle, format_sortie)
  liste_a <- format_tablo_vers_liste_a(tablo, variable_ident, variable_temporelle, format_sortie)
  if (format_sortie == "data.frame") liste_a <- lapply(liste_a, as.data.frame)

  return(liste_a)
}
