#' @importFrom dplyr select group_by filter rename ungroup summarize group_by left_join mutate row_number
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate year
NULL

#' Structurer les donnees de Finess
#'
#' Permet de mettre en place une base de référence pour Finess, pour une année donnée (la base est fournie en entrée)
#'
#' @param base base finess de l'année de référence (une année de base_finess_reduite)
#' @param champ_pa categetab considérée comme faisant partie
#'
#' @return une base finess pour l'année retravaillée (une ligne = un finess). Utile notamment pour les redressements (mais sans doute pas que)
#' @export


structurer_donnees_finess <- function(base, champ_pa = c(202,207,500,501,502)) {
  #Changer certains codes geo cog dans FINESS puor harmoniser avec table insee et recupérer STATUT
  base[(base$departement=='75'),]$cog <- '75056'
  base[(base$cog %in% c('69025','69381','69382','69383','69384','69385','69386','69387','69387','69388','69389')),]$cog <- '69123'
  base[(base$cog %in% c('13201','13202','13203','13204','13205','13206','13207','13208','13209','13210','13211','13212','13213','13214','13215','13216')),]$cog <- '13055'

  base_ref <- base %>%
    select("cog", "nofinesset", "region", "departement", "categetab", "statutjuridique", "statut", "mft", "dateouvert") %>%
    group_by(nofinesset) %>%
    filter(row_number()==1) %>%
    rename(statut_jur_agrege = statut) %>%
    ungroup()

  # sommmer les capinstot par type d'hébergement pour le champ PA
  base_pa <- base %>%
    filter(categetab %in% champ_pa, indsupinst == "N")

  base_calc_pa_heb <- base_pa %>%
    group_by(nofinesset, hebergement) %>%
    summarize(capins = sum(capinstot), .groups = "drop") %>%
    pivot_wider(
      id_cols = c("nofinesset"),
      names_from = "hebergement",
      values_from = "capins",
      values_fill = 0,
      names_prefix = ""
    )

  base_calc_pa_tot <- base_pa %>%
    group_by(nofinesset) %>%
    summarize(capinsTOT = sum(capinstot), .groups = "drop")

  # Joindre les deux bases de données
  base_calc_pa <- left_join(base_calc_pa_heb, base_calc_pa_tot, by = "nofinesset")

  # sommmer les capinstot pour le champ PH
  base_ph <- base %>%
    filter(!(categetab %in% champ_pa), indsupinst == "N")

  base_calc_ph <- base_ph %>%
    group_by(nofinesset) %>%
    summarize(capinsTOT = sum(capinstot), .groups = "drop")

  # Joindre les deux bases de données
  base_calc <- bind_rows(base_calc_pa, base_calc_ph)
  # base_calc <- full_join(base_calc_pa, base_calc_ph, by = "nofinesset")

  # Remplacer les valeurs manquantes par 0
  base_calc[is.na(base_calc)] <- 0

  # Renommer les colonnes HP et HT en capinsHP et capinsHT respectivement
  output <- base_ref %>% left_join(base_calc, by = "nofinesset") %>%
    select("cog", "nofinesset", "region", "departement", "categetab", "statutjuridique", "statut_jur_agrege", "mft", "dateouvert", "HP", "HT", "AJ", "AN", "AT","F1", "F2", "F1b", "capinsTOT") %>%
    rename(capinsHP = HP, capinsHT = HT, capinsAJ = AJ, capinsAN = AN, capinsAT = AT)
  # %>%
  #   mutate(anneeouvert = year(dateouvert))

  output <-data.frame(output)

  print("--- --- Création de la variable statut_juridique_fin")
  output <- output %>%
    mutate(
      statut_jur_fin = case_when(
        statutjuridique %in% c("02", "03", "04", "05", "06", "07", "08", "17") ~ "PublicTerritorialCCAS",
        statutjuridique %in% c("10", "11", "12", "13", "14", "15", "16") ~ "PublicRattacheEPS",
        statutjuridique %in% c("01", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29","30") ~ "PublicAutonome",
        TRUE ~ statut_jur_agrege
      )
    )

  # Création de la variable ASH FINESS en utilisant la condition ifelse
  print("--- --- Creation de la variable ASH FINESS")
  output$finess_ash <- ifelse(output$mft %in% c("08", "09", "21", "40", "41", "44", "45", "48", "50", "52", "56"), "OUI", "NON")


  return(output)
}
