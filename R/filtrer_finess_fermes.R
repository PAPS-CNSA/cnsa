#' @importFrom dplyr select left_join rename
NULL

#' Dans un tableau de données, avec un FINESS par ligne et les années en colonne, remplace les valeurs des années où le FINESS est fermé par NA
#'
#' @param tab tableau de données, une colonne "FINESS" et des colonnes d'années
#'
#' @return un tableau de données avec les valeurs des années où le FINESS est fermé remplacées par NA
#'
#' @export

filtrer_finess_fermes <- function(tab) {
  finess_full <- charger_reference_finess()
  annees <- setdiff(colnames(tab), "FINESS")
  for (annee in annees) {
    tab <- tab %>%
      left_join(finess_full[["CAP_BOOL"]] %>%
                  select(FINESS, !!sym(annee)) %>%
                  rename(capacite = annee), by = "FINESS")
    tab[tab$capacite == 0, annee] <- NA
    tab <- tab %>% select(-capacite)
  }
  return(tab)
}
