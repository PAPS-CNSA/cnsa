#' @importFrom tidyr pivot_wider gather
#' @importFrom dplyr mutate bind_rows
NULL

#' Convertir un tableau en une liste de tableaux élargis par variable
#'
#' Cette fonction prend en entrée un tableau contenant des départements, des années et des variables,
#' et retourne une liste de tableaux où chaque élément représente une variable.
#' Dans chaque tableau, vous aurez une colonne variable identifiante et ensuite une colonne pour chaque année.
#'
#' @param tablo Un tableau contenant au minimum les colonnes variable identifiante, `ANNEE` et au moins une variable.
#' @param variable_ident variable avec le nom de la variable identifiante dans le tableau
#' @return Une liste de tableaux où chaque tableau représente une variable.
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   DEPARTEMENT = c("01", "02", "03", "01", "02", "03", "01", "02", "03"),
#'   ANNEE = c(2014, 2014, 2014, 2015, 2015, 2015, 2016, 2016, 2016),
#'   VAR1 = c(10, 14, 16, 15, 19, 22, 10, 14, 25),
#'   VAR2 = c(NA, NA, NA, NA, NA, NA, 12, 18, 22)
#' )
#' res <- format_tablo_vers_liste_v(data)
#' }
#'
#' @export

format_tablo_vers_liste_v <- function(tablo, variable_ident = "DEPARTEMENT") {
  widen_data_by_variable <- function(tablo, variable_name) {
    tablo %>%
      select(!!(variable_ident), ANNEE, all_of(variable_name)) %>%
      pivot_wider(names_from = ANNEE, values_from = all_of(variable_name))
  }

  # Création d'une liste de tableaux élargis pour chaque variable
  liste_variables <- lapply(names(tablo)[3:ncol(tablo)], function(var_name) {
    widen_data_by_variable(tablo, var_name)
  })

  # Nommer la liste d'après les noms des variables
  names(liste_variables) <- names(tablo)[3:ncol(tablo)]

  return(liste_variables)
}
