#' @importFrom dplyr select group_by summarize top_n filter
NULL

#' Selection finess dominant
#'
#' Cette fonction part d'une base finess_reduite (avec toutes les années), et cela évalue si
#' le finess sélectionné a une part de places supérieure au seuil
#'
#' Exemple : seuil = 80%, variable = "client" : on vérifie si le FINESS a plus de 80% de ses places sur un type de déficience (client)
#'
#' @param base_finess_reduite base finess reduite stockée sur GEOD pour une année
#' @param variable le nom de la variable à tester
#' @param seuil le seuil choisi pour le test
#' @return un tableau précisant par FINESS si ce dernier a une modalité dominante
#' @export


selection_finess_dominant <- function(base_finess_reduite, variable, seuil) {
  travail <- base_finess_reduite %>%
    select(nofinesset, !!sym(variable), capinstot) %>%
    group_by(nofinesset, !!sym(variable)) %>%
    summarize(capacite = sum(capinstot)) %>%
    ungroup()
  modalite_dominante <- travail %>%
    group_by(nofinesset) %>%
    mutate(part = capacite / sum(capacite)) %>%
    top_n(n = 1, wt = part) %>%
    filter(part > seuil) %>%
    select(-part) %>%
    ungroup()
  return(modalite_dominante)
}
