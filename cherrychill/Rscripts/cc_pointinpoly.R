#' Checks if a point falls within in a polygon
#'
#' @param pt either be a sf object or a numeric vector of length 2 containing longitude and latitude coordinates
#' @param poly a sf polygon object
#'
#' @details
#' This function can be used on a web app to test whether a user-selected point falls within the coverage area.
#' pt and poly do not have to be in the same crs.
#'

cc_pointinpoly <- function(pt, poly) {
  if (!isNamespaceLoaded("sf")) stop("This function requires that sf is loaded")
  if (!inherits(poly, "sf")) stop("poly must be a sf object")
  if (nrow(poly) != 1) stop("poly should contain 1 and only 1 polygon")

  if (is.numeric(pt)) {
    if (length(pt) != 2) stop("Point should be a vector of longitude-latitude coordinates, or a sf object")

    pt_sf <- data.frame(id = 1,
                    lon = pt[1],
                    lat = pt[2]) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      sf::st_transform(sf::st_crs(poly))

  } else if (inherits(pt, "sf")) {
    pt_sf <- sf::st_transform(pt, sf::st_crs(poly))
  }

  ## Return TRUE or FALSE
  as.vector(sf::st_intersects(pt_sf, poly, sparse = FALSE))

}





