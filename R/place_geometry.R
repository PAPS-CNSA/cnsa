#' @importFrom sf st_centroid st_crs
NULL

#' Place a Geometry at a Specified Position
#'
#' This function moves an existing geometry (`geometry`) to a given position (`position`) and scales it by a specified factor (`scale`).
#'
#' @param geometry A `sf` geometry object that you wish to move.
#' @param position A numeric vector of length 2 specifying the x and y coordinates to move the geometry's centroid to.
#' @param scale A numeric value (default is 1) by which to scale the geometry.
#'
#' @return A `sf` geometry object that has been moved and scaled as specified.
#' @export
#'
#'
place_geometry <- function(geometry, position, scale = 1) {
  # prend en entrée une géométrie existante : 'geometry'
  # déplace cette géométrie au point 'position'
  # par défaut, pas de changement d'échelle

  # Nouvelle géométrie
  output_geometry <- (geometry - st_centroid(geometry)) * scale +  (geometry) +
    # translation
    position

  # Ajouter le système de coordonnées
  st_crs(output_geometry) <- st_crs(geometry)
  return(output_geometry)
}
