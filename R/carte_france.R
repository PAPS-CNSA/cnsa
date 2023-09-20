#' @importFrom dplyr filter left_join mutate
#' @importFrom rnaturalearth ne_states
#' @importFrom stringr str_sub
#' @importFrom sf st_bbox st_centroid st_geometry st_as_sf st_crs
#' @importFrom ggplot2 ggplot geom_sf scale_fill_distiller labs theme_void aes_string geom_sf_text aes coord_sf theme
#' @importFrom gridExtra grid.arrange arrangeGrob
NULL

#' Cette fonction permet, à partir d'un data frame et/ou d'un fichier Excel, de créer une carte de France par département
#'
#' Cette carte de France est réalisée à partir d'une variable continue, pour laquelle il va falloir définir des "classes",
#' C'est à dire des intervalles
#'
#' @param dataframe Une tableau de données, ou un fichier Excel. Ce dernier doit comporter des données départementales (une ligne par département), et au moins deux colonnes (une pour les départements, une pour la variable souhaitée)
#' @param variable La variable sur laquelle on souhaite faire une carte
#' @param nom_legende La légende souhaitée
#' @param breaks Un vecteur avec les intervalles proposés
#' @param labels Les labels des intervalles
#'
#' @return Une carte de France telle que souhaitée
#' @export
#'
#' @examples
carte_france <- function(dataframe, variable, nom_legende, breaks, labels) {

  #Chargement du contour France
  france <- ne_states(country = "France", returnclass = "sf")

  #Récupération du numéro de département
  france$DPT <- str_sub(france$iso_3166_2, 4,5)

  #Numéros des DOM
  france$DPT[france$DPT == "MQ"] <- "972"
  france$DPT[france$DPT == "GP"] <- "971"
  france$DPT[france$DPT == "GF"] <- "973"
  france$DPT[france$DPT == "RE"] <- "974"
  france$DPT[france$DPT == "YT"] <- "976"

  #On joint à la base qui nous intéresse
  france <- france %>% left_join(dataframe,
                                 by = c("DPT"))

  fra <- france %>%
    filter(!DPT %in% c("971", "972", "973", "974", "976"))

  #Martinique
  mtq <- france %>%
    filter(DPT %in% c("972"))

  #Guadeloupe
  glp <- france %>%
    filter(DPT %in% c("971"))

  #Guyane
  guy <- france %>%
    filter(DPT %in% c("973"))

  #La Réunion
  reu <- france %>%
    filter(DPT %in% c("974"))

  #Mayotte
  myt <- france %>%
    filter(DPT %in% c("976"))

  liste_idf <- c("75", "92", "93", "94")
  idf <- france %>%
    filter(DPT %in% liste_idf)


  #Position des DOM
  #Martinique
  mtq_position2 <- c(st_bbox(fra)$xmin - st_bbox(mtq)$xmin  # correspondance des 'xmin'
                     - 2,  # décalage axe X
                     st_bbox(fra)$ymin - st_bbox(mtq)$ymin  # correspondance des 'ymin'
                     + 0)  # décalage axe Y



  mtq2 <- mtq %>%
    mutate(geometry = place_geometry(geometry = st_geometry(mtq),
                                     position = mtq_position2,
                                     scale = 2.5))
  # Guadeloupe
  glp2_position <- c(st_bbox(fra)$xmin - st_bbox(glp)$xmin - 2,  # position X
                     st_bbox(mtq2)$ymax - st_bbox(glp)$ymin + 1)  # position Y
  glp2 <- glp %>%
    mutate(geometry = place_geometry(geometry = st_geometry(glp),
                                     position = glp2_position,
                                     scale = 2.5))

  # Réunion
  reu2_position <- c(st_bbox(fra)$xmin - st_bbox(reu)$xmin - 2,  # position X
                     st_bbox(glp2)$ymax - st_bbox(reu)$ymin + 1)  # position Y

  reu2 <- reu %>%
    mutate(geometry = place_geometry(geometry = st_geometry(reu),
                                     position = reu2_position,
                                     scale = 2.5))

  # Mayotte
  myt2_position <- c(st_bbox(fra)$xmin - st_bbox(myt)$xmin - 2,  # position X
                     st_bbox(reu2)$ymax - st_bbox(myt)$ymin + 1)  # position Y

  myt2 <- myt %>%
    mutate(geometry = place_geometry(geometry = st_geometry(myt),
                                     position = myt2_position,
                                     scale = 2.5))

  # Guyane
  guy2_position <- c(st_bbox(fra)$xmin - st_bbox(guy)$xmin - 3,  # position X
                     st_bbox(myt2)$ymax - st_bbox(guy)$ymin +  0)  # position Y

  guy2 <- guy %>%
    mutate(geometry = place_geometry(geometry = st_geometry(guy),
                                     position = guy2_position,
                                     scale = 0.4))


  #Réintégration de tous les dataframes
  all_sf <- plyr::rbind.fill(fra, mtq2, glp2, reu2, myt2, guy2)
  all_sf_scale <- st_as_sf(all_sf)

  p1 <- ggplot() +
    geom_sf(data = all_sf_scale, aes_string(fill = variable)) +
    #Breaks pour définir le nombre de classes et labels pour avoir le nom des classes
    scale_fill_distiller(palette="OrRd", na.value = "grey", direction = 1, breaks = breaks, labels = labels) +
    labs(x = "Longitude", y = "Latitude") +
    geom_sf_text(data = all_sf_scale %>% filter(name %in% c("Martinique", "Guadeloupe", "La Reunion", "Mayotte",
                                                            "Guyane française")),
                 aes(label = name), nudge_y = 1, nudge_x = 0.5, size = 3)  +
    labs(fill = nom_legende) + #titre de la légende
    theme_void()

  #Ajout d'un zoom sur Paris et sa petite couronne
  xlim <- as.numeric(c(2.17,2.58))
  ylim <- as.numeric(c(48.7,49))


  zoomed_map <- p1 +
    labs(subtitle = "Paris et sa \n petite couronne") +
    coord_sf(xlim = xlim, ylim = ylim) +
    theme(legend.position = "none")

  layout=rbind(c(1,  1, 2),
               c(1, 1, 1),
               c(1, 1, 1))

  return(grid.arrange(p1, arrangeGrob(zoomed_map, heights = c(2,1)), layout_matrix = layout))

}
