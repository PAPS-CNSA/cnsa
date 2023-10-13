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
#' @return une carte format jpg
#' @export

faire_une_carte <- function(table, type_output = "image", type_visu = "FRANCE_ENTIERE", titre_legende = "Legende", type_var_souhait = "CAT_AUTO", classes_souh = 5, chemin_sortie = "", type_palette = "viridis") {
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

    if (type_visu == "FRANCE_ENTIERE") { # On est sur de la France entiere

      # On crée les différentes régions
      france_sf <- france_sf %>% mutate(REGION = case_when(
        code_insee == "971" ~ "971",
        code_insee == "972" ~ "972",
        code_insee == "973" ~ "973",
        code_insee == "974" ~ "974",
        code_insee == "976" ~ "976",
        TRUE ~ "FRANCEMETRO"
      ))

      france_sf$VALEUR <- transformer_variable(france_sf$VALEUR, type_var_souhait, classes_souh)

      # On détermine une palette de couleurs
      palette_couleur <- creer_palette(france_sf$VALEUR, type_palette)

      if (type_output == "image") {
        creer_toutes_cartes(france_sf, palette_couleur, titre_legende)
        cumuler_cartes(chemin_sortie)
      } else {
        resultat <- creer_carte_indiv(france_sf, "FRANCEMETRO", palette_couleur )
        return(resultat)
      }
    }
  }
}
