#' @importFrom data.table dcast as.data.table
NULL

#' Convertir un tableau en une liste de tableaux élargis par variable
#'
#' Cette fonction prend en entrée un tableau contenant des départements, des années et des variables,
#' et retourne une liste de tableaux où chaque élément représente une variable.
#' Dans chaque tableau, vous aurez une colonne variable identifiante et ensuite une colonne pour chaque année.
#'
#' @param tablo Un tableau contenant au minimum les colonnes variable identifiante,une colonne variable temporelle et au moins une variable.
#' @param variable_ident variable avec le nom de la variable identifiante dans le tableau
#' @param variable_temporelle variable avec le nom de la variable temporelle (exemple : ANNEE)
#' @param format_sortie data.frame par défaut, mais si autre chose sortira un data.table (format de travail de la fonction)
#' @return Une liste de tableaux où chaque tableau représente une variable.
#'
#' @examples
#' \dontrun{
#' data <- tibble(
#'   DEPARTEMENT = c("01", "02", "03", "01", "02", "03", "01", "02", "03"),
#'   ANNEE = c(2014, 2014, 2014, 2015, 2015, 2015, 2016, 2016, 2016),
#'   VAR1 = c(10, 14, 16, 15, 19, 22, 10, 14, 25),
#'   VAR2 = c(NA, NA, NA, NA, NA, NA, 12, 18, 22)
#' )
#' res <- format_tablo_vers_liste_v(data, variable_ident = "DEPARTEMENT", variable_temporelle = "ANNEE", format_sortie = "data.frame")
#' }
#'
#' @export

format_tablo_vers_liste_v <- function(tablo, variable_ident = "FINESS", variable_temporelle = "ANNEE", format_sortie = "data.frame") {
  tablo <- as.data.table(tablo)
  if (tablo[, .N, by = c(variable_ident, variable_temporelle)][N > 1, .N] > 0) {
    stop("Le tableau contient des doublons pour la combinaison de variable identifiante et temporelle. Veuillez vérifier les données.")
  }
  vars <- setdiff(names(tablo), c(variable_ident, variable_temporelle))
  liste_v <- lapply(vars, function(v) {
    dcast(tablo, formula = as.formula(paste(variable_ident, "~", variable_temporelle)), value.var = v)
  })
  names(liste_v) <- vars
  if (format_sortie == "data.frame") liste_v <- lapply(liste_v, as.data.frame)
  return(liste_v)
}
