#' @importFrom dplyr group_by mutate select left_join rename summarise
NULL

#' Créer un tableau synthétisant une variable
#'
#' Crée un tableau synthétisant la somme d'une variable pour une année sur tous les FINESS ouverts selon un regroupement
#'
#' @param finess_full résultat de creer_reference_finess ou charger_reference_finess qui contient un tableau 0-1 indiquant par année si un FINESS est ouvert
#' @param tab tableau avec au moins une colonne FINESS et une variable numérique à synthétiser
#' @param groupe catégorie selon laquelle on veut regrouper : "categetab", "code_regroup_finess", "region" ou "departement"
#' @param variable variable que l'on veut synthétiser, sous forme de chaîne de caractères
#' @param annee année étudiée, sous forme de chaîne de caractères
#'
#' @return un tableau avec trois colonnes :
#'  nom du groupe : la liste des catégories ;
#'  nom de la variable : somme des valeurs pour les FINESS ouverts pour chaque catégorie ;
#'  nombre : nombre de FINESS ouverts dans chaque catégorie
#'
#' @export

creer_tableau_synthese <- function(finess_full, tab, groupe, variable, annee) {
  resultat <- tab %>%
    select(FINESS, !!sym(variable)) %>%
    left_join(finess_full[["CAP_BOOL"]] %>% select(FINESS, !!sym(groupe), !!sym(annee)), by = "FINESS")
  resultat[[variable]] <- resultat[[variable]] * resultat[[annee]]
  resultat <- resultat %>%
    group_by(!!sym(groupe)) %>% summarise(somme = sum(!!sym(variable), na.rm = T), nombre = sum(!!sym(annee), na.rm = T))
  colnames(resultat)[colnames(resultat) == "somme"] <- variable
  return (resultat)
}
