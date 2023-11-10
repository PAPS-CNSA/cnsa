#' Appliquer filtrage à une liste
#'
#' Cette fonction applique le filtrage des données (remplacer par NA) à une liste de tableaux
#' @param liste_tableau liste de tableaux
#' @param seuil_ecart Le seuil de filtrage, en %. Par exemple, un seuil à 0.5 signifie qu'on va garder les valeurs qui sont à moins de 50% d'écart de la moyenne (c'est le seuil par défaut si rien n'est précisé)
#' @param seuil_niveau le niveau à partir duquel on commence une correction. Par défaut égal à 100 : cela signifie que si la moyenne est inférieure à 100, on ne fait rien
#' @param seuil_na s'il y a trop de valeurs en NA dans la ligne, on préfère ne pas calculer la moyenne, et donc ne pas faire de correction (car la moyenne ne veut rien dire). Ce paramètre permet de fixer un seuil (par défaut, 50% : s'il manque plus de 50% de valeurs sur la ligne, on ne change rien). Ce seuil doit être défini en fonction du nombre de valeurs probablement
#' @param affichage booléen qui indique si on veut afficher un diagnostic ou non
#'
#' @return la même liste de tableaux, mais filtrée
#'
#' @export

appliquer_filtrage_liste <- function(liste_tableau, seuil_ecart=0.5, seuil_niveau = 100, seuil_na = 0.5, affichage = FALSE) {
  # Cette fonction applique les fonctions de filtrage à une liste de tableau, chaque tableau ayant en ligne les identifiantes, en colonne les années/périodes temporelles

  resultat <- lapply(test, filtrer_ecart_moyenne, seuil_ecart=0.5, seuil_niveau = 100, seuil_na = 0.5, affichage = FALSE)
  return(resultat)
}
