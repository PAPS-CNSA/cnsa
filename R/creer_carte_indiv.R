#' @importFrom dplyr filter
#' @importFrom sf st_bbox st_centroid
#' @importFrom leaflet leaflet addLegend addPolygons addLabelOnlyMarkers labelOptions
#' @importFrom leaflet.extras setMapWidgetStyle
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @importFrom htmltools HTML
NULL

#' Creer carte individuelle
#'
#' Cette fonction crée une carte pour une région donnée (FRANCEMETRO, ou un département d'outre mer)
#'
#' @param donnees un tableau de données, format SF, avec les coordonnées
#' @param region la région souhaitée, incluse dans le SF (a priori FRANCEMETRO, 971, 972,...)
#' @param palette une palette de couleurs, fournie a priori avant (note : peut être à changer)
#' @param titre_legende Si la légende s'affiche : qu'est-il marqué au dessus ?
#' @param afficher_valeurs TRUE ou FALSE : détermine si on souhaite afficher les valeurs choisies sur la carte. Par défaut : FALSE
#' @param couleur_valeurs couleur d'affichage des valeurs sur la carte. par défaut "black".
#' @param arrondi_valeurs Si on souhaite arrondir les valeurs, par exemple au milliers. Le principe est celui de round, dans R : round(115,4554, 1) => 115,5 / round(115,45554, -2) => 100
#' @param taille_valeurs taille des valeurs affichées, en pourcentage de la hauteur de la carte. Par défaut, 2 (pour 2%)
#' @param afficher_legende affiche, ou non, la légende à côté de la carte
#' @param formatter_valeurs Fonction de formattage des valeurs du tooltip.
#' @param save_png TRUE ou FALSE, selon qu'on souhaite ou non sauver un png avec l'image
#'
#' @return une carte format leaflet
#' @export
#'
creer_carte_indiv <- function(donnees,
                              region = "FRANCEMETRO",
                              palette,
                              titre_legende = "Legende",
                              afficher_valeurs = FALSE,
                              couleur_valeurs = "black",
                              arrondi_valeurs = NA,
                              taille_valeurs = 2,
                              afficher_legende = TRUE,
                              formatter_valeurs = NULL,
                              save_png = FALSE) {

  if (is.null(formatter_valeurs)) {
    formatter_valeurs <- function(x) {
      format(x, scientific = FALSE, big.mark = " ", dec = ",", trim = TRUE)
    }
  }

  # Fonction qui créée une carte pour une région donnée, avec une palette déjà prédéfinie
  data <- carte_restreindre_base(donnees,region)

  if (dim(data)[1]==0) { # La région n'est pas dans les données : on ne fait rien
    return(NA)
  } else {
    bbox <- st_bbox(data)

    if (is.numeric(data$VALEUR_CLASSE)) {
      carte <- leaflet(data) %>%
        addPolygons(
          fillColor = ~palette(VALEUR),
          fillOpacity = 0.8,
          color = "white",
          weight = 1,
          label = ~lapply(
            paste0(
              nom, " : ",
              "<b>",
              ifelse(
                is.na(VALEUR),
                "Non renseign\u00e9",
                formatter_valeurs(VALEUR)
              ),
              "</b>"
            ),
            HTML
          ),
          labelOptions = labelOptions(
            style = list(padding = "3px 8px"),
            textsize = "16px",
            direction = "auto"
          )
        ) %>%
        setMapWidgetStyle(list(background = "white"))
      if (afficher_legende) {
        carte <- carte %>%
          addLegend(
            pal = palette,
            values = ~VALEUR,
            title = "",
            position = "topright",
            na.label = "Non renseign\u00e9"
          )
      }
    } else {
      carte <- leaflet(data) %>%
        addPolygons(
          fillColor = ~palette(as.factor(VALEUR_CLASSE)),
          fillOpacity = 0.8,
          color = "white",
          weight = 1,
          label = ~lapply(
            paste0(
              nom, " : ",
              "<b>",
              ifelse(
                is.na(VALEUR),
                "Non renseign\u00e9",
                formatter_valeurs(VALEUR)
              ),
              "</b>"
            ),
            HTML
          ),
          labelOptions = labelOptions(
            style = list(padding = "3px 8px"),
            textsize = "16px",
            direction = "auto"
          )
        ) %>%
        setMapWidgetStyle(list(background= "white"))
      if (afficher_legende) {
        carte <- carte %>%
          addLegend(
            pal = palette,
            values = ~as.factor(VALEUR_CLASSE),
            title = "",
            position = "topright",
            na.label = "Non renseign\u00e9"
          )
      }
    }

    if (afficher_valeurs) { # Cas où on souhaite afficher les valeurs sur la carte
      centroids <- st_centroid(data) # On calcule le centre de chaque polygone, pour bien afficher les valeurs

      if (is.na(arrondi_valeurs)) {
        label_values <- as.character(data$VALEUR)
      } else {
        label_values <- as.character(round(data$VALEUR, arrondi_valeurs))
      }

      carte <- carte %>%
        addLabelOnlyMarkers(
          data = centroids,
          label = ~label_values,
          labelOptions = labelOptions(
            style = list("font-size"=paste0(taille_valeurs, "vw"), color = couleur_valeurs),
            noHide = TRUE, direction = "center", textOnly = TRUE, offset=c(0,0)
          )
        )
    }


    if (!all(is.na(carte)) & save_png) {
      # Créez un nom de fichier basé sur la région (en supprimant les caractères non valides)
      file_name_html <- paste0("map_", gsub(" ", "_", gsub("/", "", region)), ".html")
      file_name_png <- paste0("map_", gsub(" ", "_", gsub("/", "", region)), ".png")

      # Sauvegardez le widget Leaflet en tant que fichier HTML
      saveWidget(carte, file = file_name_html, selfcontained = TRUE)

      # Capturez une image du fichier HTML et sauvegardez-la en tant que PNG
      webshot::webshot(file_name_html, file = file_name_png,vwidth = 1024, vheight = 768, cliprect = c(10, 100, 1014, 740))

    }

    return(carte)
  }

}
