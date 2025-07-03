#' @importFrom dplyr group_by mutate distinct select left_join bind_rows if_else summarise
#' @importFrom mice mice complete
#' @importFrom readxl read_xlsx

NULL

#' Créer référence finess
#'
#' Crée une base de référence finess, utilisable pour du redressement, ou pour toute autre chose et l'enregistre
#'
#' @return une liste d'années
#' une base avec la dernière version de chaque finess
#' un élément synthèse avec une valeur de référence pour chaque
#' un tableau des capacités pour chaque année
#' un tableau 0-1 des finess fermés-ouverts (capinsTOT à NA ou pas) chaque année
#' un tableau 0-1 des finess fermés-ouverts pour l'hébergement permanent (capinsHP à NA ou pas) chaque année
#' un tableau 0-1 des finess fermés-ouverts pour l'hébergement temporaire (capinsHT à NA ou pas) chaque année
#' un tableau de la démographique des structures par catégorie
#'
#' @export

creer_reference_finess <- function(origine = "GEOD", annee_ref = 2019) {
  if (origine == "GEOD") {
    repertoire_finess <- cnsa::config$chemins$finess
  } else if (origine == "VM") {
    repertoire_finess <- cnsa::config$chemins$finess_vm
  }

  annee_debut_finess <- cnsa::config$donnees$debut_finess
  annee_fin_finess <- cnsa::config$donnees$fin_finess

  annees_finess <- as.character(c(annee_fin_finess:annee_debut_finess))
  load(paste0(repertoire_finess, "Base_Finess.RData"))

  file_active_cmpp <- read_xlsx(paste0(repertoire_finess,"File_active_CMPP.xlsx"))

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
    tempo <- structurer_donnees_finess(finess, repertoire_finess) # une base finess pour l'année retravaillée (une ligne = un finess)
    tempo$categetab <- as.character(tempo$categetab)
    tempo <- tempo %>% rename(FINESS = nofinesset)

    tempo <- tempo %>%
      left_join(tableau_internat, by = c("FINESS" = "nofinesset")) %>%
      left_join(tableau_deficience, by = c("FINESS" = "nofinesset"))

    # ajout des files actives des cmpp
    tempo <- tempo %>%
      left_join(file_active_cmpp[!is.na(file_active_cmpp[[annee]]),] %>%
                  select(FINESS, annee) %>%
                  rename("fileactive" = annee), by = "FINESS")

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


  liste_variables <- c("capinsHP", "capinsHT", "capinsAT", "capinsAJ", "capinsTOT", "statut_jur_agrege", "RMP", "DGP", "POLY", "TSA", "internat", "type_esms")
  base_spe <- base_full[,c("FINESS", "categetab", "PA_LARGE", "PA_RESTREINT", "PH", "libelle_categorie_regroup", "financeur", "code_domaine", "code_regroup_finess", "region")]
  base_spe <- base_spe %>% left_join(resultat[[as.character(annee_ref)]] %>% select(all_of(c("FINESS", liste_variables))), by = "FINESS")
  base_spe$champ <- rowSums(base_spe[,c("PA_RESTREINT", "PA_LARGE", "PH")], na.rm = T)
  base_spe <- base_spe %>% filter(champ >= 1)

  for (variable in liste_variables) {
    # On cherche d'abord en annee_ref - 1 et annee_ref + 1, puis annee_ref - 2 et annee_ref + 2, etc
    for (year in as.character(c(sort_by(setdiff(as.numeric(annees_finess), annee_ref), abs(setdiff(as.numeric(annees_finess), annee_ref) - annee_ref))))) {
      base_spe <- finess_completer_places(base_spe, resultat, year, variable)
    }
    if (sum(is.na(base_spe[[variable]]) & base_spe$categetab %in% c("189", "190", "460"))>0) {
      base_spe[is.na(base_spe[[variable]]) & base_spe$categetab %in% c("189", "190", "460"),][[variable]] <- 0 # Les SAAD n'ont pas de places
    }

  }

  # S'il reste des places à 0 ailleurs que dans les structures dédiées, on les place à NA pour imputer
  # On ne fait cela que pour capinsTOT (pour le reste, c'est possible)

  base_spe[!is.na(base_spe$capinsTOT) & base_spe$capinsTOT == 0 & !base_spe$categetab %in% c("189", "190", "460"), ]$capinsTOT <- NA

  imput <- mice(base_spe, printFlag = F, maxit = 10, m = 6, warnings = F, seed = 123) # nolint: line_length_linter
  base_spe <- complete(imput, 5)

  # On filtre les finess qui n'ont jamais eu de places côté PA_RESTREINT

  a_filtrer <- base_spe[(base_spe$PA_RESTREINT==1 & (is.na(base_spe$capinsTOT) | (base_spe$capinsTOT==0))),]$FINESS

  base_spe <- base_spe %>% filter(!(FINESS %in% a_filtrer))

  for (annee in annees_finess) {
    resultat[[annee]] <- resultat[[annee]] %>% filter(!(FINESS %in% a_filtrer))
  }

  base_full <- base_full %>% filter(!(FINESS %in% a_filtrer))


  capins <- base_full %>% select(FINESS, region, departement, categetab, code_regroup_finess)
  for (annee in sort(annees_finess)) {
    capins <- capins %>% left_join(resultat[[annee]] %>% select(FINESS, capinsTOT), by = "FINESS")
    capins[[annee]] <- capins[["capinsTOT"]]
    capins <- capins %>% select(-capinsTOT)
  }

  capins_bool <- base_full %>% select(FINESS, region, departement, categetab, code_regroup_finess)
  for (annee in sort(annees_finess)) {
    capins_bool <- capins_bool %>% left_join(resultat[[annee]] %>% select(FINESS, capinsTOT), by = "FINESS")
    capins_bool[[annee]] <- as.numeric(!is.na(capins_bool[["capinsTOT"]]))
    capins_bool <- capins_bool %>% select(-capinsTOT)
  }

  capins_hp_bool <- base_full %>% select(FINESS, region, departement, categetab, code_regroup_finess)
  for (annee in sort(annees_finess)) {
    capins_hp_bool <- capins_hp_bool %>% left_join(resultat[[annee]] %>% select(FINESS, capinsHP), by = "FINESS")
    capins_hp_bool[[annee]] <- as.numeric(!is.na(capins_hp_bool[["capinsHP"]]))
    capins_hp_bool <- capins_hp_bool %>% select(-capinsHP)
  }

  capins_ht_bool <- base_full %>% select(FINESS, region, departement, categetab, code_regroup_finess)
  for (annee in sort(annees_finess)) {
    capins_ht_bool <- capins_ht_bool %>% left_join(resultat[[annee]] %>% select(FINESS, capinsHT), by = "FINESS")
    capins_ht_bool[[annee]] <- as.numeric(!is.na(capins_ht_bool[["capinsHT"]]))
    capins_ht_bool <- capins_ht_bool %>% select(-capinsHT)
  }

  demo <- data.frame(unique(base_full[["categetab"]]))
  names(demo) <- c("categetab")
  for (annee in sort(annees_finess)) {
    demo <- demo %>% left_join(resultat[[annee]] %>% group_by(categetab) %>% summarise(nb = n()), by = "categetab")
  }
  names(demo) <- c("categetab", sort(annees_finess))

  resultat[["SYNTHESE"]] <- base_full
  resultat[[paste0("SPE_", annee_ref)]] <- base_spe
  resultat[["CAPACITE"]] <- capins
  resultat[["CAP_BOOL"]] <- capins_bool
  resultat[["CAP_HP_BOOL"]] <- capins_hp_bool
  resultat[["CAP_HT_BOOL"]] <- capins_ht_bool
  resultat[["DEMO_ETAB"]] <- demo

  # Deux FINESS ont des dates d'ouverture fausses dans la base finess

  for (name in c(annees_finess, "SYNTHESE")) {
    resultat[[name]][resultat[[name]]$FINESS == "450022561", "dateouvert"] <- "2020-02-24"
    resultat[[name]][resultat[[name]]$FINESS == "780806493", "dateouvert"] <- "1980-10-01"
  }

  saveRDS(resultat, paste0(repertoire_finess, "finess_full.rds"))

  return(resultat)
}
