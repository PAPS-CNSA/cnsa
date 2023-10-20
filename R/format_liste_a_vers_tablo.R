#' @importFrom tidyr pivot_wider gather
#' @importFrom dplyr mutate bind_rows
NULL

#' Format liste Années vers Liste variables
#'
#' Cette fonction transforme une liste de variables (chaque élément de la liste = une variable, chaque colonne = une année)
#' en une liste de variables
#' @param liste_annees Une liste de dataframes, chaque élément de la liste représentant une année. La première colonne représente la variable identifiante
#' @return Un tableau, avec une colonne "ANNE"
#' @export

format_liste_a_vers_tablo <- function(liste_annees) {

  long_annees <- lapply(names(liste_annees), function(year) {
    nom_colonnes_id <- names(liste_annees[[year]])[1]
    liste_annees[[year]] %>%
      gather(key = "variable", value = "value", -!!(nom_colonnes_id)) %>%
      mutate(ANNEE = year)
  }) %>% bind_rows() %>% pivot_wider(names_from = variable, values_from = value)

  return(long_annees)
}
