#' Création et mise en forme des quantiles
#'
#' @param base Fichier à considérer pour la création de carte
#' @param var_interet Variable à représenter
#' @param type_valeur type de variable
#' @param cat_quantiles Par défaut les quantiles
#' @param arrondir si on veut arrondir les quantiles
#'
#' @return La base avec une variable `valeur` contenant les classes.

# Fonction nouvelle pour création des quartiles
creation_quantiles <- function(base, var_interet, type_valeur,
                               cat_quantiles = c(), arrondir = arrondir){

  if (length(cat_quantiles) == 0) {
    quantiles <- quantile(base[,var_interet], probs = seq(0, 1, by = 0.25), na.rm = T)

  } else {
    quantiles <- cat_quantiles
  }

  labels_carte <- labels_quantiles(quantiles, type_valeur, arrondir = arrondir)
  base[[var_interet]] <- as.numeric(as.character(base[[var_interet]]))

  base <- base %>%
    mutate(valeur = cut(.data[[var_interet]],
                        breaks = quantiles,
                        labels = labels_carte,
                        include.lowest = TRUE))

  return(base)
}

# Fonctions reprises directes package rapport_branche (TODO à charger)
labels_quantiles <- function(vec, type_valeur, arrondir = FALSE) {

  if (is.infinite(vec[1]) && vec[1] < 0) {

    bornes <- vec[-c(1, length(vec))]

    labels_quantiles <- c(
      paste0("Inférieur à ", bornes[1]),
      paste0(
        bornes[-length(bornes)] + 1,
        "-",
        bornes[-1]
      ),
      paste0(
        "Plus de ",
        bornes[length(bornes)] + 1
      )
    )

  } else {

    vec_arrondis <- arrondir_personnalise(
      vec,
      arrondir = arrondir
    )

    labels_quantiles <- c(
      paste0(
        vec_arrondis[-length(vec_arrondis)],
        "-",
        vec_arrondis[-1]
      )
    )

    # Correction des bornes intermédiaires
    if (length(vec_arrondis) > 2) {

      labels_quantiles <- c(
        paste0(vec_arrondis[1], "-", vec_arrondis[2]),
        paste0(
          vec_arrondis[2:(length(vec_arrondis)-1)] + 1,
          "-",
          vec_arrondis[3:length(vec_arrondis)]
        )
      )
    }
  }


  if(type_valeur == "pourcentage") {
    labels_quantiles <- paste0(labels_quantiles, "%")
  }

  if(type_valeur == "pourmille") {
    labels_quantiles <- paste0(labels_quantiles, "‰")
  }

  return(labels_quantiles)
}


# Fonction pour l'arrondi
arrondir_personnalise <- function(x, arrondir = FALSE) {

  if (arrondir == FALSE) {
    return(x)
  }

  ordre_grandeur <- floor(log10(x))
  facteur_arrondi <- 10^(ordre_grandeur - 1)

  resultat <- round(x / facteur_arrondi) * facteur_arrondi

  return(as.numeric(format(resultat, scientific = FALSE)))
}
