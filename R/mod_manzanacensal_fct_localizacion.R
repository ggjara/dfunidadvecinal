
#' Get Unidad Vecinal With Address
#'
#' @parameter direccion Calle y Número
#' @parameter comuna Comuna de la dirección
#' @parameter region Región de la dirección
#'
#' @return Unidad Vecinal
#' @export
#'
#' @examples
#' @import sf googleway ggmap
get_mc_with_address <- function(direccion,
                                comuna = NULL,
                                region = NULL) {

  direccion <- direccion
  if (!is.null(comuna)) {
    direccion <- paste0(direccion, ", ", comuna)
  }
  if (!is.null(region)) {
    direccion <- paste0(direccion, ", REGION ", region)
  }

  res_google <- ggmap::geocode(toupper(direccion))

  point.sf <- sf::st_as_sf(
    data.frame(
      "long" = c(res_google$lon),
      "lat" = c(res_google$lat)
    ),
    coords = c("long", "lat"),
    crs = "WGS84"
  )

  point.sf <- sf::st_transform(point.sf, sf::st_crs(unidad_vecinales))

  sf::st_filter(manzanas, point.sf)

}

#' Get Unidad Vecinal With Lat Long
#'
#' @parameter lat Latitude
#' @parameter lng Longitude
#'
#' @return Unidad Vecinal
#' @export
#'
#' @examples
#' @import sf googleway ggmap
get_mc <- function(point) {

  point <- sf::st_transform(point, sf::st_crs(manzanas))
  sf::st_filter(manzanas, point)

}
