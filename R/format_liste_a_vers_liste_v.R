#' @importFrom tidyr gather spread
#' @importFrom dplyr mutate bind_rows group_by group_split select
NULL

#' Format liste Années vers Liste variables
#'
#' Cette fonction transforme une liste d'années (chaque élément de la liste = une année, chaque colonne = une variable)
#' en une liste de variables
#' @param liste_annees Une liste de dataframes, chaque élément de la liste représentant une année. La première colonne représente la variable identifiante
#' @return Une liste de dataframes, cette fois chaque élément représente une variable
#' @export


format_liste_a_vers_liste_v <- function(liste_annees) {
  # Cette fonction transforme une liste d'années (chaque élément de la liste = une année, chaque colonne = une variable)
  # en une liste de variables

  # Mettre chaque dataframe en format long
  long_annees <- lapply(names(liste_annees), function(year) {
    nom_colonnes_id <- names(liste_annees[[year]])[1]
    liste_annees[[year]] %>%
      gather(key = "variable", value = "value", -!!(nom_colonnes_id)) %>%
      mutate(ANNEE = year)
  }) %>% bind_rows()

  # Pivoter les données pour obtenir une liste où chaque élément représente une variable
  result <- long_annees %>%
    group_by(variable) %>%
    group_split()

  result <- lapply(result, function(df) {
    df %>%
      select(-variable) %>%
      spread(key = "ANNEE", value = value)
  })

  # Nommer la liste d'après les variables
  names(result) <- unique(long_annees$variable)
  return(result)
}
