#' @importFrom dplyr ungroup mutate select rowwise across c_across starts_with
NULL

#' Filtrer les écarts à la moyenne
#'
#' Cette fonction est une fonction de correction d'outliers
#'
#' Elle prend entrée un tableau de données sous format : première colonne = variable identifiante ; colonnes suivantes = période temporelle
#' Sur cette base, elle va remplacer certaines valeurs en écart à la moyenne par NA
#' En guise de paramètres, plusieurs sont proposés, notamment pour ne rien faire quand les valeurs sont trop petites
#'
#' @param tableau Un tableau de paramètres
#' @param seuil_ecart Le seuil de filtrage, en %. Par exemple, un seuil à 0.5 signifie qu'on va garder les valeurs qui sont à moins de 50% d'écart de la moyenne (c'est le seuil par défaut si rien n'est précisé)
#' @param seuil_niveau le niveau à partir duquel on commence une correction. Par défaut égal à 100 : cela signifie que si la moyenne est inférieure à 100, on ne fait rien
#' @param seuil_na s'il y a trop de valeurs en NA dans la ligne, on préfère ne pas calculer la moyenne, et donc ne pas faire de correction (car la moyenne ne veut rien dire). Ce paramètre permet de fixer un seuil (par défaut, 50% : s'il manque plus de 50% de valeurs sur la ligne, on ne change rien). Ce seuil doit être défini en fonction du nombre de valeurs probablement
#' @param affichage booléen qui indique si on veut afficher un diagnostic ou non
#'
#' @return le même tableau, mais filtré
#'
#' @export




filtrer_ecart_moyenne <- function(tableau, seuil_ecart=0.5, seuil_niveau = 100, seuil_na = 0.5, affichage = FALSE) {
  # Cette fonction prend en entrée un tableau de données sous format : première colonne = variable identifiante ; colonnes suivantes = période temporelle
  # Et uniquement cela
  # On fournit alors un seuil, et cette fonction filtre les valeurs qui s'écartent de la moyenne de la ligne, hors valeur extrême, de plus de seuil%

  tableau <- tableau %>% ungroup()

  premiere_colonne <- names(tableau)[1]
  a_enlever_complet <- c(premiere_colonne, "moyenne_f", "min_value", "max_value")
  noms_colonnes <- names(tableau)

  nb_annees <- dim(tableau)[2]-1
  tempo <- tableau %>%  mutate(concerne = (rowSums(is.na(.))/nb_annees)<seuil_na)
  concernes <- tempo[tempo$concerne,] %>% select(-concerne)
  non_concernes <- tempo[!tempo$concerne,] %>% select(-concerne)

  if (dim(concernes)[1]>0) {
    resultat_concernes <- concernes %>% rowwise() %>% mutate(
      min_value = min(c_across(-all_of(premiere_colonne)), na.rm = T),
      max_value = max(c_across(-all_of(premiere_colonne)), na.rm = T),
      moyenne_f = moyenne_filtree(c_across(-all_of(c(premiere_colonne, "min_value","max_value"))))
    ) %>%
      mutate(across(-all_of(a_enlever_complet), ~ifelse(. > seuil_niveau & (. < (moyenne_f * (1-seuil_ecart)) | . > (moyenne_f * (1+seuil_ecart))), NA , .), .names= "outlier_ {.col}")) %>%
      select(all_of(premiere_colonne), starts_with("outlier")) %>%
      ungroup()

    colnames(resultat_concernes) <- noms_colonnes

    if (dim(non_concernes)[1]>0) {
      resultat <- rbind(resultat_concernes, non_concernes)
    } else {
      resultat <- rbind(resultat_concernes, non_concernes)
    }
  } else {
    resultat <- non_concernes

  }

  if (affichage == TRUE) {
    print(paste0("Nombre de valeurs filtrees : ", as.character(sum(is.na(resultat)) - sum(is.na(tableau))), " sur un total de ", sum(!is.na(tableau)), " possibles."))
  }

  resultat_complet <- list()
  resultat_complet[["tableau"]] <- resultat
  resultat_complet[["valeurs_filtrees"]] <- sum(is.na(resultat)) - sum(is.na(tableau))
  resultat_complet[["taux_filtre"]] <- resultat_complet[["valeurs_filtrees"]] / sum(!is.na(tableau))

  return(resultat)
}
