% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correction_simple_deux_annees.R
\name{correction_simple_deux_annees}
\alias{correction_simple_deux_annees}
\title{Correction Simple Basée sur le Taux de Croissance entre Deux Années}
\usage{
correction_simple_deux_annees(tablo, ordre = TRUE, taux_croissance = NULL)
}
\arguments{
\item{tablo}{Un dataframe avec deux colonnes: la première représente l'année 1 et la deuxième représente l'année 2.}

\item{ordre}{Si \code{TRUE}, la deuxième colonne est imputée à partir de la première colonne.
Si \code{FALSE}, la première colonne est imputée à partir de la deuxième colonne. Par défaut, cette valeur est \code{TRUE}.}

\item{taux_croissance}{NULL ou un taux de croissance à utiliser}
}
\value{
Une colonne de dataframe avec des valeurs manquantes imputées.
}
\description{
Cette fonction impute les valeurs manquantes de l'une des deux colonnes d'un dataframe
en fonction du taux de croissance calculé à partir des valeurs non manquantes.
}
\examples{
\dontrun{
df <- data.frame(annee1 = c(100, 110, NA, 140),
                 annee2 = c(105, 115, 130, NA))
corrected_col <- correction_simple_deux_annees(df)
}

}
