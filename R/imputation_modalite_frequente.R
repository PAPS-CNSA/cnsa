#' @importFrom dplyr group_by mutate ungroup
#' @importFrom rlang sym
NULL

#' Imputation de la Modalité la Plus Fréquente
#'
#' Cette fonction remplace les valeurs manquantes (`NA`) dans une colonne spécifiée d'un tableau par la modalité la plus fréquente.
#' L'utilisateur peut également spécifier une variable de groupe pour effectuer cette imputation de manière segmentée.
#'
#' @param tablo Un dataframe contenant la variable à imputer ainsi que la variable de groupe (si spécifiée).
#' @param variable_a_imputer Le nom (sous forme de chaîne de caractères) de la variable du dataframe dont les valeurs `NA` doivent être imputées.
#' @param variable_de_groupe Le nom (sous forme de chaîne de caractères) de la variable de groupe à utiliser pour l'imputation segmentée. Par défaut, `NA`, ce qui signifie qu'aucune variable de groupe n'est utilisée.
#'
#' @return Un dataframe avec les valeurs `NA` imputées dans la variable spécifiée.
#'
#' @examples
#' df <- data.frame(
#'   groupe = c("A", "A", "B", "B", "C", "C"),
#'   valeur = c(1, NA, 2, 2, 3, NA)
#' )
#' imputation_modalite_frequente(df, "valeur", "groupe")
#'
#' @export

imputation_modalite_frequente <- function(tablo, variable_a_imputer, variable_de_groupe = NA) {
  # Fonction qui impute dans le tableau en remplaçant les NA dans "variable_a_imputer' par la modalité la plus fréquente
  # Il est possible grâce à la "variable de groupe" de prendre la modalité la plus fréquente du groupe

  if (!is.na(variable_de_groupe)) {
    tablo <- tablo %>%
      group_by(!!sym(variable_de_groupe))
  }
  tablo <- tablo %>%
    mutate(!!variable_a_imputer := ifelse(is.na(!!sym(variable_a_imputer)), calculer_mode(!!sym(variable_a_imputer)), !!sym(variable_a_imputer))) %>%

    ungroup()

  return(tablo)
}
