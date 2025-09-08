#' @importFrom dplyr mutate
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @importFrom data.table :=
NULL

#' Enlève les valeurs extrêmes de chaque colonne numérique d'un tableau
#'
#' Cette fonction élimine les valeurs qui sont en dehors des bornes définies par les probabilités `p_min` et `p_max`
#' pour chaque colonne numérique d'un tableau.
#'
#' @param tableau Un dataframe contenant les données.
#' @param p_min Probabilité inférieure définissant la borne inférieure des valeurs à conserver.
#' @param p_max Probabilité supérieure définissant la borne supérieure des valeurs à conserver.
#'
#' @return Un dataframe avec les valeurs extrêmes remplacées par NA pour chaque colonne numérique.
#'
#' @examples
#' # Exemple fictif pour illustrer l'utilisation :
#' df <- data.frame(a = c(1, 5, 1000, 10000, 20000), b = c(2, 10, 2000, 15000, 25000))
#' res <- enlever_valeurs_extremes(df, 0.00, 0.995)
#' print(res)
#'
#' @export
enlever_valeurs_extremes <- function(tableau, p_min = 0.01, p_max = 0.99) {
  # On enlève les valeurs extrêmes pour chaque colonne numérique du tableau
  # Nécessite le package dplyr

  resultat <- tableau

  # On repère les colonnes numériques
  numeric_cols <- colnames(resultat)[sapply(resultat, is.numeric)]

  # On recherche les bornes inf et sup qui correspondent aux distribution de probabilité p_min et p_max
  bounds <- lapply(numeric_cols, function(col) {
    lower_bound <- quantile(resultat[[col]], probs = p_min, na.rm = TRUE)
    upper_bound <- quantile(resultat[[col]], probs = p_max, na.rm = TRUE)
    c(lower_bound, upper_bound)
  })

  names(bounds) <- numeric_cols

  # On boucle ensuite sur les différentes colonnes pour remplacer par NA les valeurs qui sont en dehors des bornes
  for (col in numeric_cols) {
    resultat <- resultat %>%
      mutate(
        {{col}} := ifelse(
          is.na(.data[[col]]) |
            ((.data[[col]] >= bounds[[col]][1]) & (.data[[col]] <= bounds[[col]][2])),
          .data[[col]],
          NA
        )
      )
  }

  return(resultat)}
