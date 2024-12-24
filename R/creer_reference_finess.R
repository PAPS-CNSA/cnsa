#' @importFrom dplyr group_by mutate distinct select left_join bind_rows if_else
NULL

#' Créer référence finess
#'
#' Crée une base de référence finess, utilisable pour du redressement, ou pour toute autre chose
#'
#' @return une liste d'années + un élément synthèse avec une valeur de référence pour chaque
#' @export

creer_reference_finess <- function(origine = "GEOD") {
  if (origine == "GEOD") {
    repertoire_finess <- cnsa::config$chemins$finess
  } else if (origine == "VM") {
    repertoire_finess <- cnsa::config$chemins$finess_vm
  }



  annee_debut_finess <- cnsa::config$donnees$debut_finess
  annee_fin_finess <- cnsa::config$donnees$fin_finess

  annees_finess <- as.character(c(annee_fin_finess:annee_debut_finess))
  load(paste0(repertoire_finess, "Base_Finess.RData"))
  resultat <- list()
  for (annee in annees_finess) {
    print(annee)

    # On évalue pour chaque FINESS la présence de l'internat
    tableau_internat <- base_finess_reduite[[as.numeric(annee)]] %>%
      group_by(nofinesset) %>%
      mutate(internat = if_else(any(ta == '11'), "OUI", "NON")) %>%
      distinct(nofinesset, .keep_all = TRUE) %>%
      select(nofinesset, internat)

    # On évalue pour chaque FINESS la présence d'une déficience principale
    tableau_deficience <- selection_finess_dominant(base_finess_reduite[[as.numeric(annee)]], "client", 0.8) %>%
      mutate(RMP = ifelse(client == 121, 1, 0),
             DGP = ifelse(client == 204, 1, 0),
             POLY = ifelse(client == 500, 1, 0),
             TSA = ifelse(client == 437, 1, 0)) %>%
      select(-capacite,-client)

    finess <- base_finess_reduite[[as.numeric(annee)]]
    tempo <- structurer_donnees_finess(finess, repertoire_finess)
    tempo$categetab <- as.character(tempo$categetab)
    tempo <- tempo %>% rename(FINESS = nofinesset)

    tempo <- tempo %>%
      left_join(tableau_internat, by = c("FINESS" = "nofinesset")) %>%
      left_join(tableau_deficience, by = c("FINESS" = "nofinesset"))

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

  # On va, en prévision d'une imputation, prévoir une base full avec les places 2019 (ou d'autres années si pas dispo)
  # L'objectif est d'avoir, dans tous les cas, un nombre de places en 2019

  base_spe <- base_full[,c("FINESS", "categetab")]
  base_spe <- base_spe %>% left_join(resultat[["2019"]] %>% select(c("FINESS", "capinsTOT", "PA_LARGE", "PA_RESTREINT", "PH", "RMP", "DGP", "POLY", "TSA", "internat")), by = "FINESS")
  base_spe$a_faire <- rowSums(base_spe[, c("PA_LARGE", "PA_RESTREINT", "PH")], na.rm = T)

  base_spe <- base_spe[base_spe$a_faire >= 1,]
  sum(is.na(base_spe$capinsTOT))
  for (year in c("2020", "2018", "2021", "2017", "2022")) {
    print(year)
    finess_a_completer <- base_spe[(is.na(base_spe$capinsTOT) | (base_spe$capinsTOT==0) ) & !(base_spe$categetab %in% c("189", "190", "460")) , ]$FINESS
    tempo <- resultat[[year]] %>% filter(FINESS %in% finess_a_completer) %>% select(FINESS, capinsTOT) %>% rename(capinsTOT_temp = capinsTOT)
    base_spe <- base_spe %>% left_join(tempo, by = "FINESS")
    base_spe[base_spe$FINESS %in% finess_a_completer,]$capinsTOT <-base_spe[base_spe$FINESS %in% finess_a_completer,]$capinsTOT_temp
    base_spe <- base_spe %>% select(-capinsTOT_temp)
  }
  base_spe[is.na(base_spe$capinsTOT) & base_spe$categetab=="460",]$capinsTOT <- 0 # Les SAAD n'ont pas de places

  base_spe[base_spe$PA_RESTREINT == 0 | (base_spe$PA_RESTREINT==1 & (!is.na(base_spe$capinsTOT) | (base_spe$capinsTOT>0))),]



  # On filtre les finess qui n'ont jamais eu de places côté PA_RESTREINT

  a_filtrer <- base_spe[(base_spe$PA_RESTREINT==1 & (is.na(base_spe$capinsTOT) | (base_spe$capinsTOT==0))),]$FINESS

  base_spe <- base_spe %>% filter(!(FINESS %in% a_filtrer))

  for (annee in annees_finess) {
    resultat[[annee]] <- resultat[[annee]] %>% filter(!(FINESS %in% a_filtrer))
  }

  base_full <- base_full %>% filter(!(FINESS %in% a_filtrer))

  resultat[["SYNTHESE"]] <- base_full
  resultat[["SPE_2019"]] <- base_spe

  return(resultat)
}

