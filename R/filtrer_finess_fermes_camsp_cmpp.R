#' @importFrom dplyr select left_join
NULL

#' Dans un tableau de données, avec un FINESS par ligne et les années en colonne, remplace les valeurs des années où le FINESS n'est pas encore ouvert par NA
#'
#' @param tab tableau de données, une colonne "FINESS" et des colonnes d'années
#' @param finess_full résultat de creer_reference_finess ou charger_reference_finess
#'
#' @return un tableau de données avec les valeurs des années où le FINESS n'est pas encore ouvert remplacées par NA
#' @export

filtrer_finess_fermes_camsp_cmpp <- function(tab, finess_full = charger_reference_finess()) {
  annees <- setdiff(colnames(tab), "FINESS")
  for (annee in annees) {
    tab <- tab %>%
      left_join(finess_full[["SYNTHESE"]] %>%
                  select(FINESS, dateouvert), by = "FINESS")
    tab[as.numeric(substr(tab$dateouvert, 1, 4)) > as.numeric(annee), annee] <- NA
    tab <- tab %>% select(-dateouvert)
  }
  return(tab)
}
