#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
NULL

#'  Cette fonction lance la création des différentes cartes utiles (notamment DOM)
#'
#'  Dans le principe, cette fonction lance la création d'une carte par région de la base, et la sauve en png
#' Cette sauvegarde se fait dans le répertoire courant (option mettable pour plus de flexibilité)
#'
#' @param france_sf Un tableau de données SF, qui inclut notamment les coordonnées des départements (polygones)
#' @param palette une palette de couleurs
#' @param titre_legende Le nom de la légende qui doit apparaitre
#'
#' @return Beaucoup de cartes, stockées au format png (une par département)
#' @export
#'
creer_toutes_cartes <- function(france_sf,palette, titre_legende) {
  # Cumule différentes cartes en fonction des régions
  # Les sauvegarde dans des PNG
  for (region in unique(france_sf$REGION)) {
    # Générez la carte avec votre fonction generate_map
    map <- creer_carte_indiv(france_sf, region, palette, titre_legende)  # Supposant que generate_map retourne un objet Leaflet

    # Créez un nom de fichier basé sur la région (en supprimant les caractères non valides)
    file_name_html <- paste0("map_", gsub(" ", "_", gsub("/", "", region)), ".html")
    file_name_png <- paste0("map_", gsub(" ", "_", gsub("/", "", region)), ".png")

    # Sauvegardez le widget Leaflet en tant que fichier HTML
    saveWidget(map, file = file_name_html, selfcontained = TRUE)

    # Capturez une image du fichier HTML et sauvegardez-la en tant que PNG
    webshot::webshot(file_name_html, file = file_name_png,vwidth = 1024, vheight = 768, cliprect = c(10, 100, 1014, 740))
  }

}
