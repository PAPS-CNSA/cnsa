#' Créer référence finess
#'
#' Crée une base de référence finess, utilisable pour du redressement, ou pour toute autre chose
#'
#' @return une liste d'années + un élément synthèse avec une valeur de référence pour chaque
#' @export

creer_reference_finess <- function() {
  repertoire_finess <- cnsa::config$chemins$finess

  annee_debut_finess <- cnsa::config$donnees$debut_finess
  annee_fin_finess <- cnsa::config$donnees$fin_finess

  annees_finess <- as.character(c(annee_fin_finess:annee_debut_finess))
  load(paste0(repertoire_finess, "Base_Finess.RData"))
  resultat <- list()
  for (annee in annees_finess) {
    print(annee)
    finess <- base_finess_reduite[[as.numeric(annee)]]
    tempo <- structurer_donnees_finess(finess)
    tempo$categetab <- as.character(tempo$categetab)
    resultat[[annee]] <- tempo
    if (annee == annees_finess[1]) { # On créee une base complète avec la dernière version de chaque Finess
      base_full = tempo
    } else {
      base_full <- bind_rows(
        base_full,
        tempo[!(tempo$FINESS %in% base_full$FINESS),]
      )
    }
  }
  resultat[["SYNTHESE"]] <- base_full
  return(resultat)
}

