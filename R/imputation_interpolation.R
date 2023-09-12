#' Imputation via Interpolation pour les Séries Temporelles
#'
#' Cette fonction effectue une interpolation linéaire pour les valeurs manquantes
#' dans un dataframe contenant des séries temporelles.
#'
#' @param df Un dataframe dont la première colonne est un identifiant. Les colonnes suivantes
#'        doivent représenter des périodes temporelles consécutives.
#' @param nb_variables_de_cote Le nombre de colonnes (après l'identifiant) qui ne sont pas des
#'        périodes temporelles et ne doivent pas être interpolées. Par défaut, cette valeur est 0.
#'
#' @return Un dataframe avec les valeurs manquantes interpolées linéairement.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = 1:5,
#'                  t1 = c(NA, 1, 2, NA, 5),
#'                  t2 = c(NA, NA, 3, 4, NA),
#'                  t3 = c(NA, 3, NA, 5, 7))
#' imputed_df <- imputation_interpolation(df)
#' }
#'
#' @importFrom zoo na.approx
#' @export
imputation_interpolation <- function(df, nb_variables_de_cote = 0) {
  # L'idée de cette fonction est de réaliser des redressements sur un tableau présentant des séries temporelles
  # Plus précisément, on vise une logique d'interpolation : s'il manque une valeur entre deux valeurs connnues,
  # on corrige grâce à une interpolation linéaire
  # On suppose ici que la première colonne est celle de l'identifiant, le reste est constitué des différentes périodes temporelles, consécutives

  df[, (2+nb_variables_de_cote):ncol(df)] <- t(apply(df[, (2+nb_variables_de_cote):ncol(df)], 1, function(row) {
    if (any(!is.na(row))) {  # pour vérifier que toutes les valeurs ne sont pas NA
      return(zoo::na.approx(row, na.rm = FALSE, rule = 1))
    } else {
      return(row)  # si toutes les valeurs sont NA, retournez la ligne telle quelle
    }
  }))

  return(df)
}
