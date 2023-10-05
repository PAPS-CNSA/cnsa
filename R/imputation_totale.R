#' @importFrom dplyr left_join summarize group_by
#' @importFrom rlang syms
NULL

#' Imputation de Valeurs Manquantes par Ratio de Référence
#'
#' Impute les valeurs manquantes d'une variable en utilisant un ratio calculé à partir
#' de données de référence. Le ratio est calculé en regroupant les données par certaines
#' variables et en divisant la somme de la variable cible par la somme d'une autre variable.
#'
#' @param tablo Un dataframe contenant la variable à imputer.
#' @param nom_a_imputer Nom de la colonne de `tablo` contenant les valeurs à imputer.
#' @param nom_identifiant Nom de la colonne servant de clé pour la jointure avec `table_de_reference`.
#' @param table_de_reference Un dataframe contenant les données de référence pour le calcul du ratio.
#' @param variables_de_groupe Noms des colonnes pour regrouper les données lors du calcul du ratio.
#' @param variable_de_ratio Nom de la colonne utilisée comme dénominateur lors du calcul du ratio.
#'
#' @return Un vecteur avec les valeurs imputées.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = c(1,2,3),
#'                  valeur = c(100, NA, 130))
#' df_reference <- data.frame(id = c(1,2,3),
#'                            groupe = c('A','A','B'),
#'                            ratio_var = c(10, 20, 30))
#' imputed_values <- imputation_totale(df, "valeur", "id", df_reference, "groupe", "ratio_var")
#' }
#'
#' @export
imputation_totale <- function(tablo, nom_a_imputer, nom_identifiant, table_de_reference, variables_de_groupe, variable_de_ratio) {
  # Imputation totale : on considère les variable "nom_a_imputer" de la table "tablo"
  # On va aller chercher, si besoin, des variables de caractérisation dans table_de_reference avec comme clé de jointure "nom_identifiant"
  # Les variables qu'on utilise sont dans "variables_de_groupe"

  variables_de_groupe_syms <- syms(variables_de_groupe)

  # Renvoie un vecteur en résultat

  table_de_travail <- tablo[,c(nom_identifiant, nom_a_imputer)] %>%
    left_join(table_de_reference[,c(nom_identifiant, variables_de_groupe,variable_de_ratio)] , by = nom_identifiant)

  # On calcule la référence en limitant la table aux répondants
  table_repondants <- table_de_travail[!is.na(table_de_travail[,nom_a_imputer]) & !is.na(table_de_travail[,variable_de_ratio]),]

  table_ref_imp <- table_repondants %>% group_by(!!!variables_de_groupe_syms) %>% summarize(NUMERATEUR = sum(!!sym(nom_a_imputer)), DENOM = sum(!!sym(variable_de_ratio))) %>% ungroup()
  table_ref_imp$ratio <- table_ref_imp$NUMERATEUR / table_ref_imp$DENOM

  table_de_travail_2 <- table_de_travail %>% left_join(table_ref_imp[,c(variables_de_groupe, "ratio")], by =variables_de_groupe)

  table_de_travail_2[[nom_a_imputer]] <- as.numeric(table_de_travail_2[[nom_a_imputer]])

  non_repondants <- unique(table_de_travail_2[is.na(table_de_travail_2[,nom_a_imputer]),]$nofinesset)

  table_de_travail_2[table_de_travail_2$nofinesset %in% non_repondants,nom_a_imputer] <- table_de_travail_2[table_de_travail_2$nofinesset %in% non_repondants, "ratio"] * table_de_travail_2[table_de_travail_2$nofinesset %in% non_repondants,variable_de_ratio]

  return(table_de_travail_2[,nom_a_imputer])
}
