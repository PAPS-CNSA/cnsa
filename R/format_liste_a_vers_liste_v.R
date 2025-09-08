#' Format liste Années vers Liste variables
#'
#' Cette fonction transforme une liste d'années (chaque élément de la liste = une année, chaque colonne = une variable)
#' en une liste de variables
#' @param liste_annees Une liste de dataframes, chaque élément de la liste représentant une année. La première colonne représente la variable identifiante
#' @param variable_ident variable avec le nom de la variable identifiante dans le tableau
#' @param variable_temporelle variable avec le nom de la variable temporelle (exemple : ANNEE)
#' @param format_sortie data.frame par défaut, mais si autre chose sortira un data.table (format de travail de la fonction)
#' @return Une liste de dataframes, cette fois chaque élément représente une variable
#' @export


format_liste_a_vers_liste_v <- function(liste_annees, variable_ident = "FINESS", variable_temporelle = "ANNEE", format_sortie = "data.frame") {
  tablo <- format_liste_a_vers_tablo(liste_annees, var_liste = variable_temporelle, format_sortie)
  liste_v <- format_tablo_vers_liste_v(tablo, variable_ident, variable_temporelle)
  if (format_sortie == "data.frame") liste_v <- lapply(liste_v, as.data.frame)

  return(liste_v)
}
