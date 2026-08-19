#' Création de la carte accessible
#'
#' @param notre_base Fichier à considérer pour la création de carte
#' @param corse_ens Si on veut que les départements de la Corse soient regroupés
#' @param alsace_ens Si on veut que les départements de l'Alsace soient regroupés
#' @param metropole_lyon Si on veut séparer les données 69M et 69D
#' @param medaillon Type de médaillon : "non", "idf" ou "grand_paris"
#' @param dom Si on veut ajouter les DOM dans la carte
#' @param com Si on veut ajouter les COM
#' @param accessible Si on veut rendre la carte accessible
#' @param libelle_na Le libellé quand les données sont manquantes
#' @param titre Titre du graphique
#'
#' @return une carte.

# Fonction pour faire la carte avec multi-options
creation_carte_accessible <- function(notre_base,
                                      corse_ens = FALSE,
                                      alsace_ens = FALSE,
                                      metropole_lyon = FALSE,
                                      medaillon = c("idf", "grand_paris", "non"),
                                      dom = TRUE,
                                      com = FALSE,
                                      accessible = FALSE,
                                      couleurs,
                                      manual_pattern = c('none', 'circle', 'stripe', 'crosshatch', 'stripe', "wave"),
                                      angle_pattern = c(0, 0, 90, 45, 45, 0),
                                      output_path,
                                      libelle_na = "Aucune donnée",
                                      legende = "",
                                      titre = "",
                                      sous_titre = "",
                                      filename = filename) {

  # Chargement du shapefile de la France
  sp_france <- cnsa::france_shapefile_departements %>%
    st_as_sf()

  # On rajoute la Corse en une seule fois, la métropole de Lyon, ainsi que les COM
  if (corse_ens == TRUE) {
    sp_france <- sp_france %>%
      filter(!code_insee %in% c('02A', '02B')) %>%
      bind_rows(cnsa::france_shapefile_epci_dept %>%
                  filter(code_insee %in% c('020')))


  }
  if (alsace_ens == TRUE) {
    sp_france <- sp_france %>%
      filter(!code_insee %in% c('067', '068')) %>%
      bind_rows(cnsa::france_shapefile_epci_dept %>%
                  filter(code_insee %in% c('CEA')))
  }

  if (metropole_lyon == TRUE) {
    sp_france <- sp_france %>%
      filter(!code_insee %in% c('069')) %>%
      bind_rows(cnsa::france_shapefile_epci_dept %>%
                  filter(code_insee %in% c('69D', '69M')))
  }

  if (dom == TRUE) {
    sp_france <- sp_france
  } else {
    sp_france <- sp_france[!(sp_france$code_insee %in% c("971", "972", "973", "974", "976")),]
  }


  # Dans tous les cas on garde un dataframe sans les com (on les rajoutera plus tard)
  france_dep <- sp_france %>%
    left_join(notre_base %>%
                filter(!DEPARTEMENT %in% c("975", "977", "978")) %>%
                mutate(code_insee = as.character(DEPARTEMENT)) %>%
                mutate(code_insee = str_pad(code_insee,
                                            width = 3,
                                            side = "left",
                                            pad = "0")),
              by = "code_insee") %>%
    st_as_sf()

  # Si données manquantes
  if (any(is.na(france_dep$valeur))) {
    categories <- c(levels(france_dep$valeur), libelle_na)
  } else {
    categories <- levels(france_dep$valeur)
  }

  if (accessible) {

    carte_france <- ggplot(france_dep) +
      ggpattern::geom_sf_pattern(
        aes(
          pattern = valeur,
          fill = valeur
        ),
        pattern_fill = "black",
        pattern_spacing = 0.02,
        pattern_size = 0.01
      ) +
      scale_pattern_manual(
        values = manual_pattern,
        guide = guide_legend(title = NULL),
        na.value = "none",
        labels = categories
      ) +
      scale_pattern_angle_manual(
        values = angle_pattern,
        guide = guide_legend(title = NULL),
        na.value = 0,
        labels = categories
      )

  } else {

    carte_france <- ggplot(france_dep) +
      geom_sf(aes(fill = valeur))

  }

  carte_france <- carte_france +
    theme_minimal(
      base_size = 18,
      base_family = "IBM Plex Mono"
    ) +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(5, 5, 5, 5),
      legend.key.spacing.x = unit(0.7, "cm"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.text = element_text(size = 15),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank()
    ) +
    scale_fill_manual(
      values = couleurs,
      guide = guide_legend(title = NULL),
      na.value = "grey80",
      labels = categories
    )


  # Médaillon pour IDF
  medaillon <- match.arg(medaillon)

  if (medaillon == "idf") {
    dep_medaillon <- c("075", "077", "078", "091", "092", "093", "094", "095")
  } else if (medaillon == "grand_paris") {
    dep_medaillon <- c("075", "092", "093", "094")
  } else {
    dep_medaillon <- NULL
  }


  if (!is.null(dep_medaillon)) {

    base_medaillon <- france_dep %>%
      dplyr::filter(code_insee %in% dep_medaillon)

    if (accessible) {

      medaillon <- ggplot(base_medaillon) +
        ggpattern::geom_sf_pattern(
          aes(
            pattern = valeur,
            fill = valeur,
            pattern_angle = valeur
          ),
          pattern_fill = "black",
          pattern_spacing = 0.08,
          pattern_size = 0.005
        ) +
        scale_pattern_manual(
          values = manual_pattern,
          na.value = "none"
        ) +
        scale_pattern_angle_manual(
          values = angle_pattern,
          na.value = 0
        )

    } else {

      medaillon <- ggplot(base_medaillon) +
        geom_sf(
          aes(fill = valeur)
        )

    }

    medaillon <- medaillon +
      theme_minimal(
        base_size = 18,
        base_family = "IBM Plex Mono"
      ) +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
      ) +
      scale_fill_manual(
        values = couleurs,
        na.value = "grey80"
      )

  } else {

    medaillon <- ggplot() +
      theme_void() +
      theme(
        plot.background = element_blank()
      )
  }

  ## Combinaison des cartes

  # Graphe vide pour espacer
  spacer <- ggplot() +
    theme_void()


  if (com == TRUE) {
    # fonds de carte COM : les mettre ailleurs /!\
    sp_com <- cnsa::france_shapefile_com %>% as_tibble()
    # sp_975 <- st_read("G:/08. DPE/11. STATISTIQUES/02. Etudes/Repères _stat_mdph/Repères_stat_n°XX/shapefiles/975/DEPARTEMENT.shp")
    # sp_977_978 <- st_read("G:/08. DPE/11. STATISTIQUES/02. Etudes/Repères _stat_mdph/Repères_stat_n°XX/shapefiles/977_978/DEPARTEMENT.shp")
    # note : les CRS ne sont pas compatibles entre eux donc pas de jointure ni bind rows

    # Dataframe pour St bart
    df_st_barth <- notre_base %>%
      mutate(code_insee = as.character(DEPARTEMENT)) %>%
      filter(code_insee %in% c("977", "978")) %>%
      left_join(sp_com %>%
                  select(code_insee = INSEE_DEP, NOM_DEP = NOM, REGION = INSEE_REG, geometry)
      ) %>%
      st_as_sf()

    if (accessible) {

      carte_st_barth <- ggplot(df_st_barth) +
        ggpattern::geom_sf_pattern(
          aes(
            pattern = valeur,
            fill = valeur,
            pattern_angle = valeur
          ),
          pattern_fill = "black",
          pattern_spacing = 0.08,
          pattern_size = 0.005
        ) +
        scale_pattern_manual(
          values = manual_pattern,
          guide = "none",
          na.value = "none"
        ) +
        scale_pattern_angle_manual(
          values = angle_pattern,
          guide = "none",
          na.value = 0
        )

    } else {

      carte_st_barth <- ggplot(df_st_barth) +
        geom_sf(
          aes(fill = valeur)
        )

    }

    carte_st_barth <- carte_st_barth +
      theme_minimal(
        base_size = 18,
        base_family = "IBM Plex Mono"
      ) +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      scale_fill_manual(
        values = couleurs,
        guide = guide_legend(title = NULL),
        na.value = "grey80"
      )

    # Dataframe pour St Pierre
    df_st_pierre <- notre_base %>%
      mutate(code_insee = as.character(DEPARTEMENT)) %>%
      filter(code_insee %in% c("975")) %>%
      left_join(sp_com %>%
                  select(code_insee = INSEE_DEP, NOM_DEP = NOM, REGION = INSEE_REG, geometry)
      ) %>%
      st_as_sf()


    if (accessible) {

      carte_st_pierre <- ggplot(df_st_pierre) +
        ggpattern::geom_sf_pattern(
          aes(
            pattern = valeur,
            fill = valeur,
            pattern_angle = valeur
          ),
          pattern_fill = "black",
          pattern_spacing = 0.08,
          pattern_size = 0.005
        ) +
        scale_pattern_manual(
          values = manual_pattern,
          guide = "none",
          na.value = "none"
        ) +
        scale_pattern_angle_manual(
          values = angle_pattern,
          guide = "none",
          na.value = 0
        )

    } else {

      carte_st_pierre <- ggplot(df_st_pierre) +
        geom_sf(
          aes(fill = valeur)
        )

    }

    carte_st_pierre <- carte_st_pierre +
      theme_minimal(
        base_size = 18,
        base_family = "IBM Plex Mono"
      ) +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      scale_fill_manual(
        values = couleurs,
        guide = guide_legend(title = NULL),
        na.value = "grey80"
      )

    # ajout cartes COM à gauche
    side_plot_left <- plot_grid(spacer, carte_st_pierre, spacer, carte_st_barth, spacer,
                                ncol = 1, rel_heights = c(.35, .14, .07, .14, .3))


    side_plot_right <- plot_grid(medaillon, spacer,
                                 ncol = 1, rel_heights = c(.35, .65))

    side_plot_left <- ggdraw(side_plot_left)
    side_plot_right <- ggdraw(side_plot_right)

    carte_france <- ggdraw(carte_france)

    # Combiner les graphes plot1 and the side_plots into the final layout
    carte_finale <- plot_grid(side_plot_left, carte_france, side_plot_right, ncol = 3, rel_widths = c(1, 3, 1))
  } else {

    # ajout carte IDF à droite
    side_plot_right <- plot_grid(medaillon, spacer,
                                 ncol = 1, rel_heights = c(.4, .6))

    carte_finale <- plot_grid(
      carte_france,
      side_plot_right,
      ncol = 2,
      rel_widths = c(4, 1)  # carte 3× plus large que la colonne de droite
    )
  }

  titre_bloc <- ggdraw() +
    draw_label(titre, fontface = "bold", size = 18, hjust = 0.5, y = 0.7) +
    draw_label(sous_titre, fontface = "bold", size = 14, hjust = 0.5, y = 0.5)

  carte_finale_titre <- plot_grid(
    titre_bloc,
    carte_finale,
    ncol = 1,
    rel_heights = c(0.08, 1)  # ajuste 0.08-0.1 pour le titre
  )

  ggsave(plot = carte_finale_titre,
         filename = file.path(output_path, filename),
         width = 15,
         height = 10)

  return(carte_finale)

}
