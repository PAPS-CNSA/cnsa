#' @importFrom data.table as.data.table
NULL

#' Convertir un tableau en une liste de tableaux élargis par année
#'
#' Cette fonction prend en entrée un tableau contenant des départements, des années et des variables,
#' et retourne une liste de tableaux où chaque élément représente une année
#' Dans chaque tableau, vous aurez une colonne variable identifiante et ensuite une colonne pour chaque variable
#'
#' @param tablo Un tableau contenant au minimum les colonnes variable identifiante,une colonne variable temporelle et au moins une variable.
#' @param variable_temporelle variable avec le nom de la variable temporelle (exemple : ANNEE)
#' @param format_sortie data.frame par défaut, mais si autre chose sortira un data.table (format de travail de la fonction)
#' @return Une liste de tableaux où chaque tableau représente une année
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   DEPARTEMENT = c("01", "02", "03", "01", "02", "03", "01", "02", "03"),
#'   ANNEE = c(2014, 2014, 2014, 2015, 2015, 2015, 2016, 2016, 2016),
#'   VAR1 = c(10, 14, 16, 15, 19, 22, 10, 14, 25),
#'   VAR2 = c(NA, NA, NA, NA, NA, NA, 12, 18, 22)
#' )
#' res <- format_tablo_vers_liste_a(data, variable_ident = "DEPARTEMENT", variable_temporelle = "ANNEE", format_sortie = "data.frame")
#' }
#'
#' @export

format_tablo_vers_liste_a <- function(tablo, variable_ident = "FINESS", variable_temporelle = "ANNEE", format_sortie = "data.frame") {
  tablo <- as.data.table(tablo)
  liste_a <- split(tablo, by = variable_temporelle)

  liste_a <- lapply(liste_a, function(dt) {
    dt[, (variable_temporelle) := NULL]  # Supprime la colonne année devenue inutile
    return(dt)
  })
  names(liste_a) <- unique(tablo[[variable_temporelle]])

  if (format_sortie == "data.frame") liste_a <- lapply(liste_a, as.data.frame)
  return(liste_a)
}
