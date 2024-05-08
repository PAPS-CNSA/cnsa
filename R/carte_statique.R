#' @importFrom dplyr left_join filter
#' @importFrom sf st_as_sf
#' @importFrom mapsf mf_theme mf_map mf_title mf_credits mf_inset_on mf_inset_off
#' @importFrom RColorBrewer brewer.pal
NULL

#' Fonction principale pour faire une carte statique à l'échelle départementale.
#'
#'
#'
#'
#' @param donnees La base de données qui contient les valeurs à afficher sur la carte. la colonne qui renseigne le département doit impérativement s'appeler 'code_insee'. Cette colonne doit être au format caractère et en avoir trois. Il ne doit pas y avoir de géométries !!
#' @param var Variable qui doit être affichée sur la carte.
#' @param niveau_geo Il y en a trois : 'France_entiere' (France métropolitaine et départements d'Outre-mer), 'Metropole' (France métropolitaine uniquement) et 'Outre-mer' (Départements d'Outre-mer uniquement).
#' @param choro Si TRUE (défaut) la carte sera de type choroplète, sinon elle sera en proportions.
#' @param discretisation Choix de la méthode de discrétisation (le détail des méthodes disponibles est accessible dans le dictionnaire de la fonction mf_get_breaks()). Utilisé uniquement avec les cartes choroplètes.
#' @param nbre_classes Nombre de classes dans laquelle est décomposée une variable. Utilisé uniquement avec les cartes choroplètes.
#' @param titre_legende Titre de la légende.
#' @param titre_carte Titre de la carte.
#' @param source Source des données.
#' @param auteur Auteur de la carte.
#' @param medaillon Si TRUE (défaut) une carte médaillon affiche l'Île-De-France.

#' @export

carte_statique <- function(donnees,
                           var,
                           niveau_geo = "France_entiere",
                           choro = TRUE,
                           discretisation = "quantile",
                           nbre_classes = 5,
                           titre_legende = "",
                           titre_carte = "",
                           source = "",
                           auteur = "",
                           medaillon = TRUE){

  donnees_sf_fusionnees <- left_join(france_shapefile_une_carte, donnees, by = "code_insee") %>% st_as_sf()

  if(niveau_geo == "Metropole"){
    donnees_sf <- donnees_sf_fusionnees %>% filter(!(code_insee %in% c("971", "972", "973", "974", "976")))
  } else if(niveau_geo == "Outre-mer"){
    donnees_sf <- donnees_sf_fusionnees %>% filter(code_insee %in% c("971", "972", "973", "974", "976"))
  } else if(niveau_geo == "France_entiere"){
    donnees_sf <- donnees_sf_fusionnees
}

brewer.pal(nbre_classes, "RdYlBu") # Couleurs adaptées aux daltoniens.

mf_theme("brutal", bg = "cornflowerblue", mar = c(1, 1, 2, 1))
mf_map(x = donnees_sf, col = "chartreuse2")

if(isTRUE(choro)){

  mf_map(x = donnees_sf, var, type = "choro",
         breaks = discretisation, nbreaks = nbre_classes,
         leg_pos = "bottomleft",
         leg_title = titre_legende,
         leg_no_data = "Absence de donnée")

  mf_title(txt = titre_carte, pos = "center", fg = "black", bg = "white")
  mf_credits(txt = paste0("Source :", source, "\n",  auteur, " (CNSA-DPE)"), pos = "bottomright")

  if(isTRUE(medaillon)){
    mf_inset_on(x = donnees_sf, pos = "topright")
    mf_map(x = donnees_sf[c(76, 78, 79, 92:96), ], var, type = "choro", breaks = discretisation, nbreaks = nbre_classes, leg_pos = NA)
    mf_inset_off()
  }

} else {

  mf_map(x = donnees_sf, var, type = "prop", col = "azure",
         leg_pos = "bottomleft",
         leg_title = titre_legende)

  mf_title(txt = titre_carte, pos = "center", fg = "black", bg = "white")
  mf_credits(txt = paste0("Source :", source, "\n",  auteur, " (CNSA-DPE)"), pos = "bottomright")

  if(isTRUE(medaillon)){
    mf_inset_on(x = donnees_sf[c(76, 78, 79, 92:96), ], pos = "topright")
    mf_map(x = donnees_sf[c(76, 78, 79, 92:96), ], var, type = "prop")
    mf_inset_off()
    }
  }
}

