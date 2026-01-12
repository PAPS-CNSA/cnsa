# Fonction pour retourner une carte accessible

#' Créer une carte d’accessibilité
#'
#' Génère une carte d’accessibilité à partir de données départementales.
#'
#' @param donnees Dataframe contenant les données par département.
#' @param calcul_classes Indique si les classes sont calculées automatiquement.
#' @param classes Type de classes à utiliser.
#' @param type_valeur Type de valeur affichée ("niveau" ou "pourcentage").
#' @param couleurs Vecteur de couleurs.
#' @param manual_pattern Vecteur des motifs par classe.
#' @param angle_pattern Vecteur des angles des motifs.
#'
#' @export

creation_carte_accessible <- function(donnees,
                                      #par défaut on fait en sorte que les classes de la carte soient des quantiles
                                      calcul_classes = "OUI",
                                      classes = "quantiles",
                                      type_valeur = "niveau", #par défaut, mais on peut aussi choisir d'afficher en "pourcentage"
                                      couleurs = c("#E8F6FC", "#B9E4F7", "#A0DBF4", "#7EB2C9", "#5D8B9F", "#3E6677", "#234655", "#D1EDFA"),
                                      manual_pattern = c('none', 'circle', 'stripe', 'crosshatch', 'stripe', 'stripe', "crosshatch", "stripe"),
                                      angle_pattern = c(0,0,90,45,180, 45, 0, 0)) {

  colnames(donnees)[1] <- "code_insee"

  # Identifier la colonne qui n'est pas "code_insee"
  var_interet <- colnames(donnees)[2]


  # Convertir cette colonne en numérique
  donnees[[var_interet]] <- as.numeric(donnees[[var_interet]])

  # On ajoute des 0 pour que cela corresponde au fichier pour la jointure après
  if (any(!is.character(donnees$code_insee) | sapply(donnees$code_insee,
                                                     nchar) != 3)) {
    donnees <- donnees %>% mutate(code_insee = as.character(code_insee)) %>%
      mutate(code_insee = str_pad(code_insee, width = 3,
                                  side = "left", pad = "0"))
  }

  # Jointure avec un fond de carte
  france_dep <- left_join(cnsa::france_shapefile_une_carte,
                          donnees, by = "code_insee") %>% st_as_sf()


  # Regroupement en catégorie
  if (calcul_classes == "OUI") {
    # Si on veut calculer par quantiles
    if (classes == "quantiles") {
      quantiles <- quantile(france_dep[,var_interet][[1]], probs = seq(0, 1, by = 0.25), na.rm = T)
      labels_carte <- labels_quantiles(quantiles, type_valeur)

      france_dep$valeur <- cut(france_dep[,var_interet][[1]],
                               breaks = quantiles,
                               labels = labels_carte,
                               include.lowest = TRUE)
    }
    # Si les classes sont fournies
    else {
      class_info <- create_classes(classes, type_valeur)
      france_dep$valeur <- cut(france_dep[,var_interet][[1]],
                               breaks = length(class_info$labels),
                               labels = class_info$labels,
                               right = TRUE)
    }

    # Si on ne sait pas et qu'on laisse R faire les classes
  } else {
    france_dep$valeur <- france_dep[,var_interet][[1]]
  }

  # Traitement des valeurs
  france_dep$valeur <- addNA(france_dep$valeur)
  levels(france_dep$valeur) <- c(levels(france_dep$valeur), "ND")
  france_dep$valeur[is.na(france_dep$valeur)] <- "ND"
  france_dep$id_departement <- as.factor(france_dep$code_insee)


  # Création de la carte

  carte <- ggplot(france_dep) +
    geom_sf_pattern(aes(pattern = valeur,
                        fill = valeur,
                        pattern_angle = valeur),
                    pattern_fill = "black",
                    pattern_spacing = 0.02,
                    pattern_size = 0.01) +
    theme_minimal(base_size = 18, base_family = 'IBM Plex Mono') +
    theme(legend.position = 'bottom',
          legend.box.margin = margin(5, 5, 5, 5), # Espace pour la légende
          legend.spacing.y = unit(0.001, 'cm'),
          legend.text = element_text(size = 8),
          axis.title = element_blank(),        # Supprimer les titres des axes
          axis.text = element_blank(),         # Supprimer les étiquettes des axes
          axis.ticks = element_blank(),        # Supprimer les ticks des axes
          panel.grid.major = element_blank(),  # Supprimer la grille majeure
          panel.grid.minor = element_blank(),  # Supprimer la grille mineure
          panel.background = element_blank()) +   # Supprimer l'arrière-plan du panneau) +
    scale_fill_manual(values = couleurs, guide = guide_legend(title = NULL)) + # Choix des couleurs pour les classes
    scale_pattern_manual(values = manual_pattern, guide = guide_legend(title = NULL)) + # Choix des pattern
    scale_pattern_angle_manual(values = angle_pattern, guide = guide_legend(title = NULL)) # Choix des angles pour les pattern


  return(carte)

}


# Pour l'affichage des valeurs
labels_quantiles <- function(vec, type_valeur) {

  vec_arrondis <- arrondir_personnalise(vec)

  # Ajouter des espaces tous les 3 chiffres
  vec_formatte <- format(vec_arrondis, big.mark = " ", decimal.mark = ",", scientific = FALSE)

  if(type_valeur == "pourcentage") {
    vec_formatte <- paste0(vec_formatte, "%")
  }

  # On supprime les espaces
  vec_fin <- sub("//^s+", "", vec_formatte)
  vec_fin <- gsub("^\\s+", "", vec_fin)

  labels_quantiles <- c(paste0("Moins de ", vec_fin[2]),
                        paste0(vec_fin[2], "-", vec_fin[3]),
                        paste0(vec_fin[3], "-", vec_fin[4]),
                        paste0("Plus de ", vec_fin[4]))

  return(labels_quantiles)
}

arrondir_personnalise <- function(x) {
  # Trouver l'ordre de grandeur (puissance de 10)
  ordre_grandeur <- floor(log10(x))

  # Calculer le facteur d'arrondi (10 à la puissance de l'ordre de grandeur - 1)
  # pour arrondir à la bonne "tranche" (par ex. 10, 100, 1000)
  facteur_arrondi <- 10^(ordre_grandeur - 1)

  # Arrondir au multiple le plus proche de cette puissance
  resultat <- round(x / facteur_arrondi) * facteur_arrondi

  # Retourner le résultat sous forme de nombre classique, sans notation scientifique
  return(as.numeric(format(resultat, scientific = FALSE)))
}
