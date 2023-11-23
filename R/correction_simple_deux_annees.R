#' Correction Simple Basée sur le Taux de Croissance entre Deux Années
#'
#' Cette fonction impute les valeurs manquantes de l'une des deux colonnes d'un dataframe
#' en fonction du taux de croissance calculé à partir des valeurs non manquantes.
#'
#' @param tablo Un dataframe avec deux colonnes: la première représente l'année 1 et la deuxième représente l'année 2.
#' @param ordre Si `TRUE`, la deuxième colonne est imputée à partir de la première colonne.
#'        Si `FALSE`, la première colonne est imputée à partir de la deuxième colonne. Par défaut, cette valeur est `TRUE`.
#'
#' @return Une colonne de dataframe avec des valeurs manquantes imputées.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(annee1 = c(100, 110, NA, 140),
#'                  annee2 = c(105, 115, 130, NA))
#' corrected_col <- correction_simple_deux_annees(df)
#' }
#'
#' @export
correction_simple_deux_annees <- function(tablo, ordre = TRUE) {
  # Fonction très simple : on impute l'année 2 en fonction du taux de croissance. Prend un tableau. La première est l'année 1, la deuxième l'année 2
  # En sortie, on renvoie la colonne modifiée

  noms_de_colonnes <- colnames(tablo)
  nom1 <- noms_de_colonnes[1]
  nom2 <- noms_de_colonnes[2]
  cylindre = !is.na(tablo[, nom1]) & !is.na(tablo[, nom2])

  # On isole deux cas de figure : on fait dans l'ordre, ou dans le désordre

  if (ordre == TRUE) {
    # Dans l'ordre, cela fait dire qu'on redresse la deuxième colonne à partir de la première
    a_redresser = !is.na(tablo[, nom1]) & is.na(tablo[, nom2])
    taux_croissance <- sum(tablo[cylindre, nom2])/sum(tablo[cylindre, nom1]) - 1
    if (!is.nan(taux_croissance)) tablo[a_redresser, nom2] <- tablo[a_redresser, nom1] * (1 + taux_croissance)
    return(tablo[, nom2])
  } else {
    # Dans le désordre, c'est l'inverse : première colonne à partir de la deuxième
    a_redresser = !is.na(tablo[, nom2]) & is.na(tablo[, nom1])
    taux_croissance <- sum(tablo[cylindre, nom1])/sum(tablo[cylindre, nom2]) - 1
    if (!is.nan(taux_croissance)) tablo[a_redresser, nom1] <- tablo[a_redresser, nom2] * (1 + taux_croissance)
    return(tablo[, nom1])
  }
}
