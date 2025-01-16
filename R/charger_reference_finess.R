#' Charger référence finess
#'
#' Charge une base de référence finess, utilisable pour du redressement, ou pour toute autre chose
#'
#' @return une liste d'années
#' une base avec la dernière version de chaque finess
#' un élément synthèse avec une valeur de référence pour chaque
#' un tableau des capacités pour chaque année
#' un tableau 0-1 des finess fermés-ouverts (capinsTOT à NA ou pas) chaque année
#' un tableau de la démographique des structures par catégorie
#'
#' @export

charger_reference_finess <- function(origine = "GEOD") {
  if (origine == "GEOD") {
    repertoire_finess <- cnsa::config$chemins$finess
  } else if (origine == "VM") {
    repertoire_finess <- cnsa::config$chemins$finess_vm
  }
  resultat <- readRDS(paste0(repertoire_finess, "finess_full.rds"))
  return(resultat)
}
