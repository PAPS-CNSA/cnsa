#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom rlang sym
NULL

#' Cette fonction permet de transformer un statut juridique détaillé en un statut juridique regroupé
#'
#' @param tableau un tableau sous format dataframe incluant le statut juridique parmi les colonnes
#' @param nom_var_stat le nom de la colonne incluant le statut juridique
#'
#' @return le tableau initial, complété par une colonne "statut_jur_fin" incluant le statut juridique regroupé
#' @encoding UTF-8
#' @export
#'
#' @examples
#' df_exemple <- data.frame(stat_juridique = c("01", "10", "40", "70"),
#' autre_colonne = c(10, 23, 47, 54))
#' resultat <- regrouper_statut_juridique(df_exemple, "stat_juridique")
#' print(resultat)
regrouper_statut_juridique <- function(tableau, nom_var_stat) {
  tableau <- tableau %>%
    mutate(
      statut_jur_fin = case_when(
        !!sym(nom_var_stat) %in% c("02", "03", "04", "05", "06", "07", "08", "17") ~ "PublicTerritorialCCAS",
        !!sym(nom_var_stat) %in% c("10", "11", "12", "13", "14", "15", "16") ~ "PublicRattacheEPS",
        !!sym(nom_var_stat) %in% c("01", "18", "19", "20", "21", "22", "23",
                                   "24", "25", "26", "27", "28", "29", "30") ~ "PublicAutonome",
        !!sym(nom_var_stat) %in% c("40", "41", "42", "43", "44", "45", "46",
                                   "47", "48", "49", "50", "51", "52", "60",
                                   "61", "62", "63", "64", "65", "66", "89") ~ "PriveNonLucratif",
        !!sym(nom_var_stat) %in% c("70", "71", "72", "73", "74", "75", "76",
                                   "77", "78", "79", "80", "85", "86", "87",
                                   "88", "91", "93", "95") ~ "PriveLucratif",
        TRUE ~ "Autre"
      )
    )
  return(tableau)
}
