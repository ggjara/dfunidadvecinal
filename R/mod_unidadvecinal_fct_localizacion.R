# path_reg = "https://raw.githubusercontent.com/altazor-1967/Comunas-de-Chile/master/Latitud%20-%20Longitud%20Chile.csv"
# regiones_comunas <- read.csv(path_reg)
# regiones_comunas <- regiones_comunas |>
#   dplyr::mutate(`Región` = stringr::str_trim(`Región`)) |>
#   dplyr::select(`Comuna`, `Provincia`, `Región`)
#
# regiones = unique(regiones_comunas$Región)
# comunas <- list()
# for(region in regiones){
#   comunas[[region]] = unique(regiones_comunas[regiones_comunas$Región == region, "Comuna"])
# }
#
# dic_variables <- read.csv("data/diccionariovariables.csv")
#
# unidad_vecinales <-
#   sf::read_sf(
#     "data/resultados-censo-2017-según-unidad-vecinal/Unidades_Vecinales_Poblacion_Viviendas.shp"
#   )


sf::sf_use_s2(FALSE)
ggmap::register_google(config::get("google_api_key"))
googleway::set_key(key = config::get("google_api_key"))



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
get_uv_with_address <- function(direccion,
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

  sf::st_filter(unidad_vecinales, point.sf)

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
get_uv <- function(point) {

  point <- sf::st_transform(point, sf::st_crs(unidad_vecinales))
  sf::st_filter(unidad_vecinales, point)

}
