#' @importFrom dplyr mutate case_when
NULL

#' Fonction principale pour faire une carte
#'
#' Fournit pas mal d'options (en construction :)) pour obtenir des cartes dans différentes circonstances
#' Le principe est d'utiliser un leaflet, voire plusieurs si on veut afficher les DOM
#'
#' @param table Un tableau de données. Deux colonnes : la première avec des départements, la 2e avec la variable à mettre sur la carte (c'est la seule contrainte pour que cela marche !)
#' @param type_output Type d'output souhaité (à terme, doit permettre d'intégrer une option "shiny")
#' @param type_visu Type de visualisation. Par défaut "France Entière", c'est à dire y compris outremer
#' @param titre_legende Le nom de la légende (par défaut "Legende")
#' @param type_var_souhait Le type de variable souhaité. Cela peut être "CAT_AUTO" : la variable est alors automatiquement transformée en catégorielle si besoin. "NUM" : dans ce cas, si la variable est numérique, elle reste telle quelle.
#' @param classes_souh Classes souhaitées. Si on est dans la CAT_AUTO, il faut un nombre qui est le nombre de classes ; si on est dans CAT_MAN : alors on fournit les catégories. Par défaut CAT_AUTO
#' @param chemin_sortie le répertoire dans lequel on enregistre le jpg résultat (par défaut le répertoire de travail)
#' @param type_palette Type de la palette (par défaut : viridis)
#' @param afficher_valeurs TRUE ou FALSE : détermine si on souhaite afficher les valeurs choisies sur la carte. Par défaut : FALSE
#' @param couleur_valeurs couleur d'affichage des valeurs sur la carte. par défaut "black".
#' @param arrondi_valeurs Si on souhaite arrondir les valeurs, par exemple au milliers. Le principe est celui de round, dans R : round(115,4554, 1) => 115,5 / round(115,45554, -2) => 100
#' @param taille_valeurs taille des valeurs affichées, en pourcentage de la hauteur de la carte. Par défaut, 2 (pour 2%)
#' @param save_png TRUE ou FALSE, selon qu'on souhaite ou non sauver un png avec l'image
#' @return une carte format jpg
#' @export

faire_une_carte <- function(table, type_output = "image", region_concernee = "FRANCEENTIERE", titre_legende = "Legende", type_var_souhait = "CAT_AUTO", classes_souh = 5, chemin_sortie = "", type_palette = "viridis",
                            afficher_valeurs = FALSE, couleur_valeurs = "black", arrondi_valeurs = NA, taille_valeurs = 2, afficher_legende = FALSE, save_png = FALSE) {
  # Cette fonction fait une carte ! En entrée, il faut juste :
  # - les données à cartographier
  # - le type d'output (image, ou shiny)
  # - le type de visualisation : francemetro, ou rien

  # La table en entrée doit avoir deux colonnes : la première doit contenir les départements, la 2e les valeurs à grapher

  if (dim(table)[2] != 2) {
    print("ERREUR ! Les donnees fournies n'ont pas deux colonnes")
  } else {
    colnames(table) <- c("DEPARTEMENT","VALEUR")
    table$DEPARTEMENT <- pad_left(table$DEPARTEMENT,2) # On s'assure de transformer les '1' en '01' pour eviter ce type de problème sur les départements
    france_sf <- merge(france_shapefile, table, by.x = "code_insee", by.y = "DEPARTEMENT")

    france_sf <- carte_restreindre_base(france_sf, region_concernee) # On restreint la base aux régions souhaitées

    france_sf$VALEUR <- transformer_variable(france_sf$VALEUR, type_var_souhait, classes_souh) # On transforme en classe
    palette_couleur <- creer_palette(france_sf$VALEUR, type_palette) # Et on crée la palette de couleurs

    if (type_output == "image") {
      if (region_concernee == "FRANCEENTIERE") {
        creer_toutes_cartes(france_sf, palette = palette_couleur, titre_legende = titre_legende, regions_selectionnees = region_concernee, afficher_valeurs = afficher_valeurs, couleur_valeurs = couleur_valeurs, arrondi_valeurs = arrondi_valeurs, taille_valeurs = taille_valeurs, afficher_legende = afficher_legende, save_png = TRUE)
        cumuler_cartes(chemin_sortie)
      } else {
        creer_carte_indiv(donnees = france_sf, region = region_concernee, palette = palette_couleur, titre_legende = titre_legende, afficher_valeurs=afficher_valeurs, arrondi_valeurs = arrondi_valeurs, taille_valeurs = taille_valeurs, afficher_legende = afficher_legende, save_png = TRUE )
      }

    } else {
      resultat <- creer_carte_indiv(donnees = france_sf, region = region_concernee, palette = palette_couleur, titre_legende = titre_legende, afficher_valeurs=afficher_valeurs, arrondi_valeurs = arrondi_valeurs, taille_valeurs = taille_valeurs, afficher_legende = afficher_legende, save_png = save_png )
      return(resultat)
    }
  }
}
