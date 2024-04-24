#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select full_join
#' @importFrom purrr imap
#' @importFrom utils txtProgressBar setTxtProgressBar
NULL

#' Transforme une liste de variables en un tableau
#'
#' Liste de variables = liste de tableaux, première colonne = variable identifiante, les suivantes = des années
#'
#' @param liste_v une liste de variables
#' @return un tablo, i.e. une grand tableau avec en première colonne la variable identifiante, en 2e les années (sur plusieurs lignes), puis les autres colonnes sont les variables
#' @export

format_liste_v_vers_tablo <- function(liste_v) {
  # Permet de passer d'un format liste variable à un format tablo

  # On met une petite fonction imbriquée car elle ne sert que là :
  transformer_et_ajouter_nom <- function(df, nom_variable) {
    df %>%
      pivot_longer(
        cols = -FINESS,
        names_to = "ANNEE",
        values_to = nom_variable
      ) %>%
      mutate(VARIABLE = nom_variable) %>% select(-VARIABLE)
  }

  # Puis, l'idée est de transformer chaque élément de la liste de tableau, puis de les fusionner les uns avec les autres

  liste_tableaux <- imap(liste_v, transformer_et_ajouter_nom)

  # Ensuite, on fusionne tous les tableaux. On le fait avec une boucle - peut être pas optimal - dans le but de pouvoir afficher une barre de progrès

  grand_tableau <- liste_tableaux[[1]]
  var_identifiante <- names(grand_tableau)[1]
  progress_bar <- txtProgressBar(min = 0, max = length(liste_tableaux), style = 3)

  # Fusionner les tableaux avec une barre de progression
  for (i in 2:length(liste_tableaux)) {
    grand_tableau <- full_join(grand_tableau, liste_tableaux[[i]], by = c(var_identifiante, "ANNEE"))
    setTxtProgressBar(progress_bar, i)

  }
  close(progress_bar)

  return(grand_tableau)
}
