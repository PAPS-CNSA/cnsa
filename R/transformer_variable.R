#' Cette fonction transforme une variable numérique en une variable catégorielle
#'
#' Ceci ne fonctionne que si la variable d'entrée est numérique, sinon on se contente de renvoyer la même
#'
#' @param variable_entree vecteur au format idéalement numérique
#' @param type_souhaite type de sortie souhaité de la variable. Par défaut "CAT_AUTO", c'est à dire variable catégorielle avec des seuils calculés automatiquement
#' @param classes_souhaitees Nombre de classes souhaitées (si on est dans de l'auto) ou vecteur de classes (si on est dans du manuel)
#' @param formatter_valeurs Fonction pour formatter les libellés
#'
#' @return une variable modifiée pour devenir catégorielle (a priori)
#'
#' @export
transformer_variable <- function(variable_entree,
                                 type_souhaite = "CAT_AUTO",
                                 classes_souhaitees = 5,
                                 formatter_valeurs = NULL) {
  # Cette fonction transforme une variable discrète en variable catégorielle, si souhaité

  if (is.null(formatter_valeurs)) {
    formatter_valeurs <- function(x) {
      format(x, scientific = FALSE, big.mark = " ", dec = ",", trim = TRUE)
    }
  }

  if (type_souhaite == "CAT_AUTO") {
    if (is.numeric(variable_entree)) {
      # Alors, on fait quelque chose (sinon, on renvoie la table inchangée)

      # On fait de jolis breaks
      breaks_pretty <- pretty(variable_entree, n = classes_souhaitees)
      formatted_breaks <- formatter_valeurs(breaks_pretty)

      # On découpe en classes
      variable_cat <- cut(
        variable_entree,
        breaks = breaks_pretty,
        include.lowest = TRUE
      )

      niveaux <- levels(variable_cat)

      nouveaux_niveaux <- paste("Entre", formatted_breaks[-length(formatted_breaks)], "et", formatted_breaks[-1])

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
