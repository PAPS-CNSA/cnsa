#' @importFrom data.table melt dcast as.data.table
NULL

#' Transforme une liste de variables en un tableau
#'
#' Liste de variables = liste de tableaux, première colonne = variable identifiante, les suivantes = des années
#'
#' @param liste_v une liste de variables
#' @param variable_ident le nom de la variable identifiante. Par défaut, "FINESS"
#' @param variable_temporelle le nom de la variable année. Par défaut, "ANNEE"
#' @return un tablo, i.e. une grand tableau avec en première colonne la variable identifiante, en 2e les années (sur plusieurs lignes), puis les autres colonnes sont les variables
#' @export

format_liste_v_vers_tablo <- function(liste_v, variable_ident= "FINESS", variable_temporelle="ANNEE", format_sortie="data.frame") {
  if (length(liste_v) == 0) {
    return(data.table(FINESS = integer(), ANNEE = character()))
  }

  resultat <- rbindlist(lapply(names(liste_v), function(var_name) {
    dt <- as.data.table(liste_v[[var_name]])  # Convertir en data.table
    dt[, variable := var_name]  # Ajouter une colonne avec le nom de la variable
    return(dt)
  }), use.names = TRUE, fill = TRUE)  # Fusionner tous les tableaux

  dt_long <- melt(resultat, id.vars = c(variable_ident, "variable"),
                  variable.name = variable_temporelle,
                  value.name = "valeur")

  formula <- as.formula(paste(variable_ident, "+", variable_temporelle, "~ variable"))
  dt_large <- dcast(dt_long, formula, value.var = "valeur")
  if (format_sortie == "data.frame") dt_large <- as.data.frame(dt_large)

  return(dt_large)
}
