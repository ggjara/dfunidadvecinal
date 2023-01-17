# # Data manipulation, transformation and visualisation
# library(tidyverse)
# # Nice tables
# library(kableExtra)
# # Simple features (a standardised way to encode vector data ie. points, lines, polygons)
# library(sf)
# # Spatial objects conversion
# library(sp)
# # Thematic maps
# library(tmap)
# # Colour palettes
# library(RColorBrewer)
# # More colour palettes
# library(viridis)
#
# tmap_mode("view")
# tmap_options(list(basemaps = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldGrayCanvas")))
# tmap_options()$basemaps
#
# lng <- -71.536196
# lat <- -32.9761618
# point.sf <- sf::st_as_sf(
#   data.frame(
#     "long" = c(lng),
#     "lat" = c(lat)
#   ),
#   coords = c("long", "lat"),
#   crs = "WGS84"#sf::st_crs(unidad_vecinales)
# )
#
# unidad_vecinales <- sf::read_sf("data/resultados-censo-2017-según-unidad-vecinal/Unidades_Vecinales_Poblacion_Viviendas.shp")
# unidad_vecinales["color"] = "Others"
# unidad_vecinales[unidad_vecinales$CODIGO_UV=="05109_124", "color"] = "Selected"
# tm_shape(unidad_vecinales |> filter(COD_REGION=="05")) +
#   tm_polygons(col = "color",
#               alpha=0.7,
#               popup.vars = c("Hogares" = "HOGARES",
#                              "Personas" = "PERSONAS"),
#               title = "Unidades Vecinales",
#               id = "CODIGO_UV")
#
#
#     tm_fill(col = "color", title = "Titulo", style = "cont", alpha=0.5) + # add fill
#   tm_borders(col = "white", lwd = .01)
#
#
# # add borders
#   #tm_compass(type = "arrow", position = c("right", "top") , size = 4) + # add compass
#   #tm_scale_bar(breaks = c(0,1,2), text.size = 0.5, position =  c("center", "bottom")) |>
#   # add scale bar |>
# map_oa
#
