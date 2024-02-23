#' @title Shapefile pour faire des cartes de France
#' @description
#' Un shapefile qui contient les polygones par département de la France, y compris outre mer
#' @format Un shapefile avec les colonnes suivantes :
#' \describe{
#' \item{code_insee}{Le code département}
#' \item{nom}{Le nom du département}
#' \item{nuts3}{Le code européen du département}
#' \item{wikipedia}{Le nom wikipedia}
#' \item{geometry}{les coordonnées du polygone}
#' }
"france_shapefile"


#' Carte de France métropolitaine + DOM
#'
#'
#' @format
#' A data frame with 101 rows and 3 columns
#' @source Package CARTElette
"france_depdom"
