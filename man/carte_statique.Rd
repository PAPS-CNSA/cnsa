% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carte_statique.R
\name{carte_statique}
\alias{carte_statique}
\title{Fonction principale pour faire une carte statique à l'échelle départementale.}
\usage{
carte_statique(
  donnees,
  var,
  france_outre_mer = "France_entiere",
  choro = TRUE,
  discretisation = "quantile",
  nbre_classes = 5,
  nivea_geo = "departements",
  titre_legende = "",
  couleurs = "RdYlBu",
  medaillon = TRUE,
  medaillon_emprise = "IDF",
  leg_no_data = "Absence de donnée"
)
}
\arguments{
\item{donnees}{La base de données qui contient les valeurs à afficher sur la carte. la colonne qui renseigne le département doit impérativement s'appeler 'code_insee'. Il ne doit pas y avoir de géométries !!}

\item{var}{Variable qui doit être affichée sur la carte.}

\item{france_outre_mer}{Il y a trois possibilités : 'France_entiere' (France métropolitaine et départements d'Outre-mer), 'Metropole' (France métropolitaine uniquement) et 'Outre-mer' (Départements d'Outre-mer uniquement).}

\item{choro}{Si TRUE (défaut) la carte sera de type choroplète, sinon elle sera en proportions.}

\item{discretisation}{Choix de la méthode de discrétisation (le détail des méthodes disponibles est accessible dans le dictionnaire de mapsf::mf_get_breaks()). Utilisé uniquement avec les cartes choroplètes.}

\item{nbre_classes}{Nombre de classes dans laquelle est décomposée une variable. Utilisé uniquement avec les cartes choroplètes.}

\item{nivea_geo}{Détermine les contours géographiques. Il y en a trois : 'departements'  (départements seulement), 'collectivites et départements'  (les départements, les collectivités -CEA et Corse- et la métropole de Lyon), 'regions' (les régions).}

\item{titre_legende}{Titre de la légende.}

\item{couleurs}{Palette de couleurs à utiliser pour les cartes. Les palettes disponibles sont celles du package RColorBrewer. La palette par défaut est "RdYlBu" (adaptée aux daltoniens).}

\item{medaillon}{Si TRUE (défaut) une carte médaillon affiche l'Île-De-France.}

\item{medaillon_emprise}{Emprise du médaillon.Par défaut, il s'agit de l'ïle-de-France. Pour avoir une autre empprise, il faut donner une combinaison de valeurs au format trois caractères. Par exemple, pour l'ïle-de-France la combinaison de valeurs est c("075", "077", "078", "091", "092", "093", "094", "095").}

\item{leg_no_data}{Commentaire dans la légende pour les observations sans donnée. Par défaut, il affiche 'Absence de donnée".}
}
\description{
Fonction principale pour faire une carte statique à l'échelle départementale.
}
