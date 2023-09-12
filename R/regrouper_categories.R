#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom rlang sym
NULL

#' Cette fonction permet de transformer une catégorie d'ESMS détaillé en une catégorie
#'
#' @param tableau un tableau sous format dataframe incluant le statut juridique parmi les colonnes
#' @param nom_var_cat le nom de la colonne incluant la catégorie (categetab a priori)
#'
#' @return le tableau initial, complété par une colonne "categetab_regr" incluant le statut juridique regroupé
#' @encoding UTF-8
#' @export
#'
#' @examples
#' df_exemple <- data.frame(categetab = c(183, 186, 188, 255), autre_colonne = c(10, 23, 47, 54))
#' resultat <- regrouper_categories(df_exemple, "categetab")
#' print(resultat)
regrouper_categories <- function(tableau, nom_var_cat) {
  structures_enfants <- c(183, 186, 188, 192, 194, 195,
                          196, 238, 377, 390, 396, 402, 182, 189, 190, 221)
  structures_adultes <- c(198, 246, 249, 252, 253, 255,
                          379, 382, 395, 437, 448, 449, 461, 464, 445, 446)
  structures_pa <- c(500)


  tableau <- tableau %>%
    mutate(
      categetab_regr = case_when(
        !!sym(nom_var_cat) %in% structures_enfants ~ "ENF",
        !!sym(nom_var_cat) %in% structures_adultes ~ "ADULT",
        !!sym(nom_var_cat) %in% structures_pa ~ "PA",
        TRUE ~ NA_character_
      )
    )
  return(tableau)
}
