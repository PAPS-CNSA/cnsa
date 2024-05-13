#' @importFrom dplyr left_join filter
#' @importFrom sf st_as_sf
#' @importFrom mapsf mf_theme mf_map mf_title mf_credits mf_inset_on mf_inset_off
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_pad
NULL

#' Fonction principale pour faire une carte statique à l'échelle départementale.
#'
#'
#'
#'
#' @param donnees La base de données qui contient les valeurs à afficher sur la carte. la colonne qui renseigne le département doit impérativement s'appeler 'code_insee'. Il ne doit pas y avoir de géométries !!
#' @param var Variable qui doit être affichée sur la carte.
#' @param niveau_geo Il y en a trois : 'France_entiere' (France métropolitaine et départements d'Outre-mer), 'Metropole' (France métropolitaine uniquement) et 'Outre-mer' (Départements d'Outre-mer uniquement).
#' @param choro Si TRUE (défaut) la carte sera de type choroplète, sinon elle sera en proportions.
#' @param discretisation Choix de la méthode de discrétisation (le détail des méthodes disponibles est accessible dans le dictionnaire de mapsf::mf_get_breaks()). Utilisé uniquement avec les cartes choroplètes.
#' @param nbre_classes Nombre de classes dans laquelle est décomposée une variable. Utilisé uniquement avec les cartes choroplètes.
#' @param titre_legende Titre de la légende.
#' @param couleurs Palette de couleurs à utiliser pour les cartes. Les palettes disponibles sont celles du package RColorBrewer. La palette par défaut est "RdYlBu" (adaptée aux daltoniens).
#' @param medaillon Si TRUE (défaut) une carte médaillon affiche l'Île-De-France.
#' @param medaillon_emprise Emprise du médaillon.Par défaut, il s'agit de l'ïle-de-France. Pour avoir une autre empprise, il faut donner une combinaison de valeurs au format trois caractères. Par exemple, pour l'ïle-de-France la combinaison de valeurs est c("075", "077", "078", "091", "092", "093", "094", "095").

#' @export

carte_statique <- function(donnees,
                           var,
                           niveau_geo = "France_entiere",
                           choro = TRUE,
                           discretisation = "quantile",
                           nbre_classes = 5,
                           titre_legende = "",
                           couleurs = "RdYlBu", # Couleurs adaptées aux daltoniens.
                           medaillon = TRUE,
                           medaillon_emprise = "IDF"){

if(any(!is.character(donnees$code_insee) | sapply(donnees$code_insee, nchar) != 3)){
  donnees <- donnees %>% mutate(code_insee = as.character(code_insee)) %>%
    mutate(code_insee = str_pad(code_insee, width = 3, side = "left", pad = "0"))
}

  donnees_sf_fusionnees <- left_join(france_shapefile_une_carte, donnees, by = "code_insee") %>% st_as_sf()

  if(niveau_geo == "Metropole"){
    donnees_sf <- donnees_sf_fusionnees %>% filter(!(code_insee %in% c("971", "972", "973", "974", "976")))
  } else if(niveau_geo == "Outre-mer"){
    donnees_sf <- donnees_sf_fusionnees %>% filter(code_insee %in% c("971", "972", "973", "974", "976"))
  } else if(niveau_geo == "France_entiere"){
    donnees_sf <- donnees_sf_fusionnees
}

if(medaillon_emprise == "IDF"){
  IDF <- donnees_sf %>% filter(code_insee %in% c("075", "077", "078", "091", "092", "093", "094", "095"))
}else{
  IDF <- donnees_sf[emprise, ]
}


brewer.pal(nbre_classes, couleurs)

mf_theme("brutal", bg = "white", mar = c(0, 0, 0, 0))
mf_map(x = donnees_sf, col = "white")

if(isTRUE(choro)){

  mf_map(x = donnees_sf, var, type = "choro",
         breaks = discretisation, nbreaks = nbre_classes,
         leg_pos = "bottomleft",
         leg_title = titre_legende,
         leg_no_data = "Absence de donnée")

  if(isTRUE(medaillon)){
    mf_inset_on(x = IDF, pos = "topright")
    mf_map(x = IDF, var, type = "choro", breaks = discretisation, nbreaks = nbre_classes, leg_pos = NA)
    mf_inset_off()
  }

} else {

  mf_map(x = donnees_sf, var, type = "prop", col = "azure",
         leg_pos = "bottomleft",
         leg_title = titre_legende)

  if(isTRUE(medaillon)){
    mf_inset_on(x = IDF, pos = "topright")
    mf_map(x = IDF, var, type = "prop")
    mf_inset_off()
    }
  }
}

