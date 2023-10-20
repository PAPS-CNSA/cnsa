#'  Cette fonction lance la création des différentes cartes utiles (notamment DOM)
#'
#'  Dans le principe, cette fonction lance la création d'une carte par région de la base, et la sauve en png
#' Cette sauvegarde se fait dans le répertoire courant (option mettable pour plus de flexibilité)
#'
#' @param france_sf Un tableau de données SF, qui inclut notamment les coordonnées des départements (polygones)
#' @param palette une palette de couleurs
#' @param titre_legende Le nom de la légende qui doit apparaitre
#' @param afficher_valeurs TRUE ou FALSE : détermine si on souhaite afficher les valeurs choisies sur la carte. Par défaut : FALSE
#' @param couleur_valeurs couleur d'affichage des valeurs sur la carte. par défaut "black".
#' @param arrondi_valeurs Si on souhaite arrondir les valeurs, par exemple au milliers. Le principe est celui de round, dans R : round(115,4554, 1) => 115,5 / round(115,45554, -2) => 100
#' @param taille_valeurs taille des valeurs affichées, en pourcentage de la hauteur de la carte. Par défaut, 2 (pour 2%)
#' @param save_png TRUE ou FALSE, selon qu'on souhaite ou non sauver un png avec l'image
#' @param afficher_legende affiche, ou non, la légende à côté de la carte
#' @param regions_selectionnees Type de visualisation. Par défaut "FRANCEENTIERE", c'est à dire y compris outremer

#' @return Beaucoup de cartes, stockées au format png (une par département)
#' @export
#'
creer_toutes_cartes <- function(france_sf,palette, titre_legende = "Legende", regions_selectionnees = "FRANCEENTIERE", afficher_valeurs = FALSE, couleur_valeurs = "black", arrondi_valeurs = NA, taille_valeurs = 2, afficher_legende = FALSE, save_png = TRUE) {
  # Cumule différentes cartes en fonction des régions
  # Les sauvegarde dans des PNG

  if (regions_selectionnees == "FRANCEENTIERE") {
    regions_selectionnees = c("FRANCEMETRO","01","02","03","04","06")
  }

  for (region in regions_selectionnees) {
    # Générez la carte avec votre fonction generate_map
    if (region == "FRANCEMETRO") {
      creer_carte_indiv(france_sf, region, palette, titre_legende, afficher_valeurs=afficher_valeurs, couleur_valeurs=couleur_valeurs , arrondi_valeurs=arrondi_valeurs, taille_valeurs=taille_valeurs, save_png = save_png, afficher_legende = TRUE)  # Supposant que generate_map retourne un objet Leaflet
    } else {
      creer_carte_indiv(france_sf, region, palette, titre_legende, afficher_valeurs=afficher_valeurs, couleur_valeurs=couleur_valeurs , arrondi_valeurs=arrondi_valeurs, taille_valeurs=5*taille_valeurs, save_png = save_png)  # Supposant que generate_map retourne un objet Leaflet
    }

  }

}
