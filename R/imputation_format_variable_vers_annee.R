#' @importFrom dplyr select
#' @importFrom dplyr full_join
NULL

#' Change le format d'une table de variables vers années
#'
#' Cette fonction transforme une liste de dataframes (un par variable) avec les mêmes variables
#' en une liste avec un tableau par année.
#'
#' @param base_variable Une liste de dataframes, un dataframe par variable, contenant les mêmes années.
#' @param nom_identifiant Un caractère indiquant le nom de la variable identifiante.
#' @param noms_a_garder Un vecteur de caractères indiquant les noms des variables à conserver.
#'   Par défaut à NA, signifiant qu'aucune autre variable n'est conservée.
#'
#' @return Une liste avec un tableau par année.
#'
#' @examples
#' # Exemple fictif pour illustrer l'utilisation :
#' # Supposons que base_variable est une liste contenant des dataframes pour var1 et var2.
#' base_var1 <- data.frame(id = 1:3, "2020" = c(100, 200, 300), "2021" = c(110, 210, 320))
#' base_var2 <- data.frame(id = 1:3, "2020" = c("A", "B", "C"), "2021" = c("A", "D", "E"))
#' base_variable <- list("var1" = base_var1, "var2" = base_var2)
#'
#' # Utilisation de la fonction :
#' res <- imputation_format_variable_vers_annee(base_variable, "id")
#' print(res)
#'
#' @export
imputation_format_variable_vers_annee <- function(base_variable, nom_identifiant, noms_a_garder = NA) {

  if (!is.na(noms_a_garder)) {
    liste_annees <- names(base_variable[[1]] %>% select(-c(nom_identifiant, noms_a_garder)))
  } else {
    liste_annees <- names(base_variable[[1]] %>% select(-c(nom_identifiant)))
  }

  base_annee_final <- list()

  for (annee in liste_annees) {
    for (nom_var in names(base_variable)) {
      if (!is.na(noms_a_garder)) {
        base_tempo <- base_variable[[nom_var]][,c(nom_identifiant, noms_a_garder, annee)]
        colnames(base_tempo) <- c(nom_identifiant, noms_a_garder, nom_var)
      } else {
        base_tempo <- base_variable[[nom_var]][,c(nom_identifiant, annee)]
        colnames(base_tempo) <- c(nom_identifiant, nom_var)
      }

      if (nom_var == names(base_variable)[1]) {
        base_res <- base_tempo
      } else {
        if (!is.na(noms_a_garder)) {
          base_res <- base_tempo %>% full_join(base_res, by = c(nom_identifiant, noms_a_garder))
        } else {
          base_res <- base_tempo %>% full_join(base_res, by = c(nom_identifiant))
        }

      }
    }
    base_annee_final[[annee]] <- base_res
  }
  return(base_annee_final)
}
