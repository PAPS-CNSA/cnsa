#' @importFrom dplyr full_join
NULL

#' Change le format d'une table
#'
#' Cette fonction transforme une liste de dataframes (un par année) avec les mêmes variables
#' en une liste avec un tableau par variable contenant toutes les années.
#'
#' @param base_annee Une liste de dataframes, un dataframe par année, contenant les mêmes variables.
#' @param noms_variable Un vecteur de caractères indiquant les noms des variables à traiter.
#' @param nom_identifiant Un caractère indiquant le nom de la variable identifiante.
#' @param noms_a_garder Un vecteur de caractères indiquant les noms des variables à conserver.
#'   Par défaut à NA, signifiant qu'aucune autre variable n'est conservée.
#'
#' @return Une liste avec un tableau par variable contenant toutes les années.
#'
#' @examples
#' # Exemple fictif pour illustrer l'utilisation :
#' # Supposons que base_annee est une liste contenant des dataframes pour 2020 et 2021.
#' base_2020 <- data.frame(id = 1:3, var1 = c(100, 200, 300), var2 = c("A", "B", "C"))
#' base_2021 <- data.frame(id = 1:3, var1 = c(110, 210, 320), var2 = c("A", "D", "E"))
#' base_annee <- list("2020" = base_2020, "2021" = base_2021)
#'
#' # Utilisation de la fonction :
#' res <- imputation_format_annee_vers_variable(base_annee, c("var1", "var2"), "id")
#' print(res)
#'
#' @export
imputation_format_annee_vers_variable <- function(base_annee, noms_variable, nom_identifiant, noms_a_garder = NA) {
  # Cette fonction change le format d'une table
  # On part du format suivant : une liste de dataframe, un par année, avec les mêmes variables
  # Dans chaque dataframe, une variable identifiante, éventuellement d'autres variables
  # A la fin, on veut récupérer une liste avec un tableau par variable contenant toutes les années

  liste_annees <- names(base_annee)

  resultat_final <- list()

  # On créée ensuite le tableau de résultat par variables
  for (nom_var in noms_variable) {
    for (annee in liste_annees) {
      base_a_utiliser <- base_annee[[annee]]

      if (!is.na(noms_a_garder)) {
        resultat_tempo <- base_a_utiliser[,c(nom_identifiant, noms_a_garder, nom_var)]
        colnames(resultat_tempo) <- c(nom_identifiant,noms_a_garder, annee)
      } else {
        resultat_tempo <- base_a_utiliser[,c(nom_identifiant, nom_var)]
        colnames(resultat_tempo) <- c(nom_identifiant, annee)
      }

      if (annee == liste_annees[1]) {
        resultat_full <- resultat_tempo
      } else {
        if (!is.na(noms_a_garder)) {
          resultat_full <- resultat_full %>% full_join(resultat_tempo, by = c(nom_identifiant, noms_a_garder))
        } else {
          resultat_full <- resultat_full %>% full_join(resultat_tempo, by = c(nom_identifiant))
        }

      }
    }
    # On enlève les doublons (pas très propre !)
    resultat_full <- resultat_full[!duplicated(resultat_full$nofinesset),]
    resultat_final[[nom_var]] <- resultat_full
  }
  return(resultat_final)}
