#' Cette fonction transforme une variable numérique en une variable catégorielle
#'
#' Ceci ne fonctionne que si la variable d'entrée est numérique, sinon on se contente de renvoyer la même
#'
#' @param variable_entree vecteur au format idéalement numérique
#' @param type_souhaite type de sortie souhaité de la variable. Par défaut "CAT_AUTO", c'est à dire variable catégorielle avec des seuils calculés automatiquement
#' @param classes_souhaitees Nombre de classes souhaitées (si on est dans de l'auto) ou vecteur de classes (si on est dans du manuel)
#'
#' @return une variable modifiée pour devenir catégorielle (a priori)
#'
#' @export
transformer_variable <- function(variable_entree, type_souhaite = "CAT_AUTO", classes_souhaitees=5) {
  # Cette fonction transforme une variable discrète en variable catégorielle, si souhaité

  if (type_souhaite == "CAT_AUTO") {
    if (is.numeric(variable_entree)) {
      # Alors, on fait quelque chose (sinon, on renvoie la table inchangée)

      # On fait de jolis breaks
      breaks_pretty <- pretty(variable_entree, n = classes_souhaitees)
      formatted_breaks <- format(breaks_pretty, scientific = FALSE)

      # On découpe en classes
      variable_cat <- cut(variable_entree,
                          breaks = breaks_pretty,
                          include.lowest = TRUE)

      niveaux <- levels(variable_cat)

      nouveaux_niveaux <- paste0("Entre ", formatted_breaks[-length(formatted_breaks)], " et ", formatted_breaks[-1])
      nouveaux_niveaux <- gsub("     ","", nouveaux_niveaux)
      levels(variable_cat) <- nouveaux_niveaux

      sortie <- variable_cat

    } else {
      sorte <- variable_entree
    }

  } else {
    sortie <- variable_entree
  }
  return(sortie)
}
