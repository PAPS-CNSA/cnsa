#' @importFrom tidyr gather spread
#' @importFrom dplyr mutate bind_rows group_by group_split select
NULL

#' Format liste Années vers Liste variables
#'
#' Cette fonction transforme une liste de variables (chaque élément de la liste = une variable, chaque colonne = une année)
#' en une liste de variables
#' @param liste_variables Une liste de dataframes, chaque élément de la liste représentant une variable La première colonne représente la variable identifiante
#' @return Une liste de dataframes, cette fois chaque élément représente une année
#' @export

format_liste_v_vers_liste_a <- function(liste_variables) {
  variable_id <- names(liste_variables[[1]])[1] # La variable identifiante est en première colonne

  long_vars <- lapply(names(liste_variables), function(var_name) {
    if (var_name != "ANNEE") {
      df <- liste_variables[[var_name]]
      df %>%
        gather(key = "ANNEE", value = "value", -!!(variable_id)) %>%
        mutate(variable = var_name)
    }
  }) %>% bind_rows()

  liste_annees <- unique(long_vars$ANNEE)

  # Pivoter les données pour obtenir une liste où chaque élément représente une année
  liste_annees_result <- long_vars %>%
    group_by("ANNEE") %>%
    group_split()

  liste_annees_result <- lapply(liste_annees_result, function(df) {
    df %>%
      select(-"ANNEE") %>%
      spread(key = "variable", value = value)
  })

  # Nommer la liste d'après les années
  names(liste_annees_result) <- unique(long_vars$ANNEE)
  return(liste_annees_result)

}
