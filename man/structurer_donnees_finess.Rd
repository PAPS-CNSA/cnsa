% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structurer_donnees_finess.R
\name{structurer_donnees_finess}
\alias{structurer_donnees_finess}
\title{Structurer les donnees de Finess}
\usage{
structurer_donnees_finess(base, champ_pa = c(202, 207, 500, 501, 502))
}
\arguments{
\item{base}{base finess de l'année de référence (une année de base_finess_reduite)}

\item{champ_pa}{categetab considérée comme faisant partie}
}
\value{
une base finess pour l'année retravaillée (une ligne = un finess). Utile notamment pour les redressements (mais sans doute pas que)
}
\description{
Permet de mettre en place une base de référence pour Finess, pour une année donnée (la base est fournie en entrée)
}
