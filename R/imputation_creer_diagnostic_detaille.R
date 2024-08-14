#' @importFrom dplyr left_join group_by summarize ungroup filter select across
#' @importFrom rlang sym
NULL

#' Imputation creer diagnostic détaillé
#'
#' Principe : on a une base stat, une base brute, les deux au format tablo
#' On a une base 2019 pour les capacités
#' On regarde, année par année, et variable de la base par variable de la base, les taux de non réponse
#'
#' @param base_stat une base stat au format tablo
#' @param base_brute une base brute au format tablo
#' @param base_2019 la base 2019 de référence (contient PA/PH mais pas SAAD, avec imputation de capacité si pas présente)
#' @param categetab_sel argument optionnel, pour que le tableau ne sorte que sur certains categetab
#'
#' @return une liste d'années avec les résultats attendus pour chaque année
#' @export

imputation_creer_diagnostic_detaille <- function(base_stat, base_brute, var_reference, base_2019, categetab_sel = NULL) {


  if (!is.null(categetab_sel)) {
    base_stat <- base_stat %>% left_join(base_2019[,c("FINESS", "categetab")], by = "FINESS") %>% filter(categetab %in% categetab_sel) %>% select(-categetab)
    base_brute <- base_brute %>% left_join(base_2019[,c("FINESS", "categetab")], by = "FINESS") %>% filter(categetab %in% categetab_sel) %>% select(-categetab)
  }
  tablo <- list()
  tablo[["brut"]] <- base_brute
  tablo[["stat"]] <- base_stat

  resultats <- list()
  for (type in c("brut", "stat")) {
    tempo <- tablo[[type]] %>% left_join(base_2019[, c("FINESS", "capinsTOT", "categetab")], by = "FINESS")

    full_variables <- colnames(tempo %>% select(-c("FINESS","capinsTOT", "ANNEE", "categetab")))



    par_categetab <- tempo %>% group_by(categetab, ANNEE) %>% summarize(capacite = sum(capinsTOT),
                                                                        repondants = n(),
                                                                        across(full_variables, \(x) sum(x, na.rm = T))) %>% ungroup()

    par_annee <- tempo %>% group_by(ANNEE) %>% summarize(capacite = sum(capinsTOT, na.rm = T),
                                                         across(full_variables, \(x) sum(x, na.rm = T))) %>% ungroup()

    par_annee_na <- tempo %>% group_by(ANNEE) %>% summarize(capacite = sum(capinsTOT, na.rm = T),
                                                            across(full_variables, \(x) sum(!is.na(x), na.rm = T))) %>% ungroup()

    capacite <- tempo %>% filter(!is.na(!!sym(var_reference))) %>% group_by(ANNEE)  %>% summarize(capacite = sum(capinsTOT, na.rm = T)) %>% ungroup()

    colnames(par_annee) <- paste0(type, "_", colnames(par_annee))
    colnames(par_annee_na) <- paste0(type, "_NA_", colnames(par_annee_na))
    colnames(par_categetab) <- paste0(type, "_", colnames(par_categetab))
    colnames(capacite) <- paste0(type, "_", colnames(capacite))

    resultats[[type]]$par_annee <- par_annee
    resultats[[type]]$par_annee_na <- par_annee_na
    resultats[[type]]$par_categetab <- par_categetab
    resultats[[type]]$capacite <- capacite
  }

  resultats[["synth"]]$par_annee <- resultats[["brut"]]$par_annee %>% left_join(resultats[["stat"]]$par_annee, by = c("brut_ANNEE" = "stat_ANNEE"))
  resultats[["synth"]]$par_annee_na <- resultats[["brut"]]$par_annee_na %>% left_join(resultats[["stat"]]$par_annee_na, by = c("brut_NA_ANNEE" = "stat_NA_ANNEE"))
  resultats[["synth"]]$par_categetab <- resultats[["brut"]]$par_categetab %>% left_join(resultats[["stat"]]$par_categetab, by = c("brut_ANNEE" = "stat_ANNEE", "brut_categetab" = "stat_categetab"))
  resultats[["synth"]]$capacite <- resultats[["brut"]]$capacite %>% left_join(resultats[["stat"]]$capacite, by = c("brut_ANNEE" = "stat_ANNEE"))


  results_na_annee <- list()
  for (annee in unique(resultats[["synth"]]$par_annee$brut_ANNEE)) {
    na_annee <- as.data.frame(matrix(0, nrow = length(full_variables), ncol = 7))
    full_variables <- full_variables %>% setdiff(c("denom", "numerateur", "resu"))

    for (i in seq_along(full_variables)) {
      var <- full_variables[i]
      na_annee[i, 1] <- var
      na_annee[i, 2] <- as.numeric(resultats[["synth"]]$par_annee_na %>% filter(brut_NA_ANNEE == annee) %>% select(paste0("brut_NA_", var)))
      na_annee[i, 3] <- as.numeric(resultats[["synth"]]$par_annee_na %>% filter(brut_NA_ANNEE == annee) %>% select(paste0("stat_NA_", var)))
      na_annee[i, 4] <- na_annee[i, 2] / na_annee[i, 3]
      na_annee[i, 5] <- as.numeric(resultats[["synth"]]$capacite %>% filter(brut_ANNEE == annee) %>% select(brut_capacite))
      na_annee[i, 6] <- as.numeric(resultats[["synth"]]$par_annee %>% filter(brut_ANNEE == annee) %>% select(stat_capacite))
      na_annee[i, 7] <- na_annee[i, 5] / na_annee[i, 6]
      na_annee[i, 8] <- as.numeric(resultats[["synth"]]$par_annee %>% filter(brut_ANNEE == annee) %>% select(paste0("brut_", var)))
      na_annee[i, 9] <- as.numeric(resultats[["synth"]]$par_annee %>% filter(brut_ANNEE == annee) %>% select(paste0("stat_", var)))
      na_annee[i, 10] <- na_annee[i, 8] / na_annee[i, 9]

      colnames(na_annee) <- c("variable",
                              "REPONDANTS_BRUT", "REPONDANTS_STATS", "TX_NON_REP_REPONDANTS",
                              "CAP_REPONDANTS_BRUT", "CAP_REPONDANTS_STAT", "TX_CAP",
                              "TOT_VAL_BRUT", "TOT_VAL_NET", "TX_VAL")

    }
    results_na_annee[[annee]] <- na_annee
  }
  return(results_na_annee)
}
