#' @importFrom dplyr left_join
#' @importFrom dplyr all_of
#' @importFrom dplyr distinct
#' @importFrom dplyr semi_join
NULL

#' Imputation de Valeurs Manquantes par Taux de Croissance
#'
#' Impute les valeurs manquantes d'une série temporelle en utilisant un taux de croissance.
#' Les séries temporelles sont groupées par une variable de groupe, et le taux de croissance
#' est appliqué de manière chronologique puis antéchronologique.
#'
#' @param tablo Un dataframe avec une colonne identifiante suivie de colonnes temporelles.
#' @param table_reference Un dataframe optionnel contenant des données de référence pour le regroupement.
#' @param variable_groupe Un caractère indiquant la colonne de regroupement utilisée dans la table de référence.
#' @param affichage Un booléen pour activer ou désactiver l'affichage des informations sur les valeurs manquantes.
#'
#' @return Un dataframe avec les valeurs manquantes imputées.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = c(1,2,3),
#'                  annee1 = c(100, NA, 130),
#'                  annee2 = c(NA, 120, 140))
#' df_reference <- data.frame(id = c(1,2,3), groupe = c('A','A','B'))
#' df_imputed <- imputation_taux_croissance(df, table_reference = df_reference,
#' variable_groupe = "groupe", affichage = TRUE)
#' }
#'
#' @export
imputation_taux_croissance <- function(tablo, table_reference = NA, variable_groupe = NA, affichage=FALSE) {
  # Dans cette version de la fonction, les données doivent être organisées comme suit :
  # - Le "tablo" : première colonne = identifiante ; autres colonnes : période temporelle, dans l'ordre
  # - La référence : toutes les colonnes qu'on veut, avec la même identifiante

  # Effectue la correction simple complète
  nombre_colonnes <- dim(tablo)[2]
  # Etape 0 : combien a t'on de valeurs manquantes ?
  if (affichage) {
    print( paste("Nombre de valeurs manquantes initiales :",  sum(is.na(tablo[,(1+1):nombre_colonnes])),sep = '' ) )
  }

  # Pour corriger des valeurs manquantes, la stratégie est simple
  # On parcourt le tableau dans le sens chronologique, et si la deuxième valeur est manquante, on l'impute avec le même taux de croissance que le national
  # Puis, on le parcourt dans l'autre sens, avec la même logique
  # Au final, la stratégie ne devrait échouer que dans le cas où il manque toute la ligne

  # Seule subtilité : on veut le faire en décomposant éventuellement selon les colonnes de regroupement
  # Pour cela, on commence par identifier les types de regroupement dans les colonnes choisies

  # Pour cela, on va d'abord utiliser la table de référence pour ajouter

  variable_identifiante <- names(tablo)[1]
  variables_temporelles <- names(tablo)[2:dim(tablo)[2]]

  besoin_regroupement <- 0
  if ((dim(table_reference)[2]>0) && sum(!is.na(variable_groupe))>0) {
    besoin_regroupement <- 1
    table_complete <- tablo %>% left_join(table_reference[,c(variable_identifiante, variable_groupe)], by = variable_identifiante)
    # On remet dans l'ordre les colonnes du tableau
    table_complete <- table_complete[,c(variable_identifiante, variable_groupe, variables_temporelles)]
    combinaisons_existantes <- table_complete %>% select(all_of(variable_groupe)) %>% distinct()

  }

  # Sens chronologique
  if (besoin_regroupement == 1) {
    for (j in (1:dim(combinaisons_existantes)[1])) {
      if (dim(combinaisons_existantes)[2] == 1) { # Si on a une seule variable de groupe
      tablo_de_travail <- tablo %>% left_join(table_reference[,c(variable_identifiante, variable_groupe)], by = variable_identifiante) %>% 
        filter(!!sym(variable_groupe) == combinaisons_existantes[j,]) %>% 
        select(-all_of(variable_groupe))
      } else {
        tablo_de_travail <- tablo %>% left_join(table_reference[,c(variable_identifiante, variable_groupe)], by = variable_identifiante) %>% 
          semi_join(combinaisons_existantes[j,], by = variable_groupe) %>% 
          select(-all_of(variable_groupe))
      }
      
      for (i in (2:(nombre_colonnes-1))) {
        # On suppose que le tableau est sous la forme "numéro du département puis colonnes d'années"
        tablo_de_travail[,i+1] = correction_simple_deux_annees(tablo_de_travail[,i:(i+1)] , TRUE)
      }
      if (j == 1) {
        tablo_de_sortie <- tablo_de_travail
      } else {
        tablo_de_sortie <- rbind(tablo_de_sortie, tablo_de_travail)
      }
    }
    tablo <- tablo_de_sortie

    if (affichage) print( paste("Nombre de valeurs manquantes restantes :",  sum(is.na(table_complete[,(1+1+length(variable_groupe)):nombre_colonnes])),sep = '' ) )

  } else {
    for (i in (2:(nombre_colonnes-1))) {
      # On suppose que le tableau est sous la forme "numéro du département puis colonnes d'années"
      tablo[,i+1] = correction_simple_deux_annees(tablo[,i:(i+1)], TRUE)
    }
    if (affichage) print( paste("Nombre de valeurs manquantes restantes :",  sum(is.na(tablo[,2:nombre_colonnes])),sep = '' ) )

  }

  # Sens antéchronologique

  if (besoin_regroupement == 1) {
    # Sens chronologique
    for (j in (1:dim(combinaisons_existantes)[1])) {
      if (dim(combinaisons_existantes)[2] == 1) { # Si on a une seule variable de groupe 
        tablo_de_travail <- tablo %>% left_join(table_reference[,c(variable_identifiante, variable_groupe)], by = variable_identifiante) %>% 
                filter(!!sym(variable_groupe) == combinaisons_existantes[j,]) %>% 
                select(-all_of(variable_groupe))
      } else {
        tablo_de_travail <- tablo %>% left_join(table_reference[,c(variable_identifiante, variable_groupe)], by = variable_identifiante) %>%
        semi_join(combinaisons_existantes[j,], by = variable_groupe) %>% select(-all_of(variable_groupe))
      }
      
      for (i in (nombre_colonnes - 1) : (2) ) {
        # On suppose que le tableau est sous la forme "numéro du département puis colonnes d'années"
        tablo_de_travail[,i] = correction_simple_deux_annees(tablo_de_travail[,i:(i+1)] , FALSE)
      }
      if (j == 1) {
        tablo_de_sortie <- tablo_de_travail
      } else {
        tablo_de_sortie <- rbind(tablo_de_sortie, tablo_de_travail)
      }
    }
    tablo <- tablo_de_sortie[,c(variable_identifiante, variables_temporelles)]

  } else {
    for (i in ((nombre_colonnes-1):2)) {
      # On suppose que le tableau est sous la forme "numéro du département puis colonnes d'années"
      tablo[,i] = correction_simple_deux_annees(tablo[,i:(i+1)], FALSE)
    }
  }


  # for (i in ((nombre_colonnes-1):-1:2)) {
  #   # On suppose que le tableau est sous la forme "numéro du département puis colonnes d'années"
  #   tablo[,i] = correction_simple_deux_annees(tablo[,i:(i+1)], FALSE)
  # }
  #
 if (affichage) {
   print( paste("Nombre de valeurs manquantes finales :",  sum(is.na(tablo[,(1+1):nombre_colonnes])),sep = '' ) )
 }

  # A ce stade, il ne reste plus que les lignes pour lesquelles on n'avait rien
  # Pour celles là, il va forcément falloir une valeur externe
  return(tablo)

}
