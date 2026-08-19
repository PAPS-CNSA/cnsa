#' Lancement d'une cartographique
#'
#' @param base Fichier à considérer pour la création de carte
#' @param var_interet notre variable à représenter
#' @param type_valeur le type de la variable (nombre, pourcentage)
#' @param corse_ens Si on veut que les départements de la Corse soient regroupés
#' @param alsace_ens Si on veut que les départements de l'Alsace soient regroupés
#' @param metropole_lyon Si on veut séparer les données 69M et 69D
#' @param medaillon Type de médaillon : "non", "idf" ou "grand_paris"
#' @param com Si on veut ajouter les COM dans la carte
#' @param dom Si on veut ajouter les DOM
#' @param libelle_na Le libellé quand les données sont manquantes
#' @param titre Titre du graphique
#'
#' @return une carte.
#'
#' @export
#'
# Création finale de la carte + enregistrement
faire_carte <- function(base,
                        var_interet,
                        type_valeur = "pourcentage",
                        cat_quantiles = c(),
                        corse_ens = F,
                        alsace_ens = F,
                        metropole_lyon = F,
                        medaillon = "idf",
                        dom = T,
                        com = F,
                        accessible = T,
                        couleurs = c("#E8F6FC", "#B9E4F7", "#A0DBF4", "#7EB2C9", "#5D8B9F"),
                        output_path,
                        legende = "",
                        titre = "",
                        sous_titre = "",
                        nom_carte = "ma_carte",
                        arrondir = FALSE,
                        libelle_na = "Aucune donnée"
){

  # Regroupement en catégories de la variable d'interet
  colnames(base)[1] <- "DEPARTEMENT"

  base_valeur <- creation_quantiles(base, var_interet, type_valeur,
                                    cat_quantiles = cat_quantiles,
                                    arrondir = arrondir)

  # Carte
  filename = paste0(nom_carte, ".png")


  creation_carte_accessible(
    notre_base = base_valeur,
    corse_ens = corse_ens,
    alsace_ens = alsace_ens,
    metropole_lyon = metropole_lyon,
    medaillon = medaillon,
    dom = dom,
    com = com,
    accessible = accessible,
    couleurs = couleurs,
    output_path = output_path,
    libelle_na = libelle_na,
    legende = legende,
    titre = titre,
    sous_titre = sous_titre,
    filename = filename,
  )

}
