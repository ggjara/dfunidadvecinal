#' unidadvecinal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_unidadvecinal_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::fluidRow(
    bs4Dash::column(
      width = 12,
      bs4Dash::box(
        id = ns("parameters"),
        width = 4,
        title = "Parameters",
        # icon = shiny::icon("list", lib = "glyphicon"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        div(
          shiny::textInput(inputId = ns("my_address"), label = "Type An Address"),
          shiny::htmlOutput(outputId = ns("my_html"))
        )
      )
    )
  ),
  shiny::fluidRow(
    bs4Dash::column(
      width = 6,
      bs4Dash::box(
        id = ns("results_unidadvecinal"),
        width = 12,
        title = "Unidad vecinal",
        collapsed = TRUE,
        solidHeader = TRUE,
        collapsible = TRUE,
        shinycssloaders::withSpinner(DT::DTOutput(outputId = ns("unidad_vecinal")))
      )
    ),
    bs4Dash::column(
      width = 6,
      bs4Dash::box(
        id = ns("results_map"),
        width = 12,
        title = "Map",
        collapsed = TRUE,
        solidHeader = TRUE,
        collapsible = TRUE,
        shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map")))
      ),
      bs4Dash::box(
        id = ns("results_google"),
        width = 12,
        title = "Google Results",
        collapsed = TRUE,
        solidHeader = TRUE,
        collapsible = TRUE,
        shinycssloaders::withSpinner(shiny::htmlOutput(outputId = ns("full_address")))
      )
    )
  ))
}

collapsed =  TRUE

#' unidadvecinal Server Functions
#'
#' @noRd
#' @import dplyr
mod_unidadvecinal_server <- function(id, tab_selected, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Lazy loading
    observeEvent(coords(), {
      if(!is.null(coords()) & !r$unidadvecinal_downloaded){
        r$unidadvecinal_downloaded <- TRUE
      }
    })

    unidad_vecinales <- eventReactive(r$unidadvecinal_downloaded, {
      if(r$unidadvecinal_downloaded){
        base::readRDS("data/lazy/unidad_vecinales.rds")
      }
      else{
        NULL
      }
    })

    output$my_html <- shiny::renderUI({
      shiny::HTML(
        paste0(
          " <script>
                function initAutocompleteLala() {

                 var autocomplete =   new google.maps.places.Autocomplete(document.getElementById('",
          ns("my_address"),
          "'),{types: ['geocode']});
                 autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                 autocomplete.addListener('place_changed', function() {
                 var place = autocomplete.getPlace();
                 if (!place.geometry) {
                 return;
                 }

                 var addressPretty = place.formatted_address;
                 var address = '';
                 if (place.address_components) {
                 address = [
                 (place.address_components[0] && place.address_components[0].short_name || ''),
                 (place.address_components[1] && place.address_components[1].short_name || ''),
                 (place.address_components[2] && place.address_components[2].short_name || ''),
                 (place.address_components[3] && place.address_components[3].short_name || ''),
                 (place.address_components[4] && place.address_components[4].short_name || ''),
                 (place.address_components[5] && place.address_components[5].short_name || ''),
                 (place.address_components[6] && place.address_components[6].short_name || ''),
                 (place.address_components[7] && place.address_components[7].short_name || '')
                 ].join(' ');
                 }
                 var address_number =''
                 address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                 var coords = place.geometry.location;
                 console.log(address);
                 Shiny.onInputChange('",
          ns('jsValue'),
          "', address);
                 Shiny.onInputChange('",
          ns('jsValueAddressNumber'),
          "', address_number);
                 Shiny.onInputChange('",
          ns('jsValuePretty'),
          "', addressPretty);
                 Shiny.onInputChange('",
          ns('jsValueCoords'),
          "', coords);});}
                 </script>
                 <script src='https://maps.googleapis.com/maps/api/js?key=",
          config::get("google_api_key"),
          "&libraries=places&callback=initAutocompleteLala' async defer></script>"
        )
      )
    })

    coords <- reactive({
      if (!is.null(input$jsValueCoords)) {
        coords <- input$jsValueCoords
      } else{
        coords = NULL
      }
      coords
    })

    address <- reactive({
      if (!is.null(input$jsValueAddressNumber)) {
        if (length(grep(
          pattern = input$jsValueAddressNumber,
          x = input$jsValuePretty
        )) == 0) {
          final_address <- c(input$jsValueAddressNumber, input$jsValuePretty)
        } else{
          final_address <- input$jsValuePretty
        }
        final_address
      }
    })


    output$full_address <- renderUI({
      shiny::req(address(), coords())
      if (!is.null(address()) & !is.null(coords())) {
        shiny::tagList(shiny::HTML(paste0(
          "<b>Full address:</b><br>", address()
        )),
        shiny::br(),
        shiny::HTML(
          paste0(
            "<b>Coords:</b><br>",
            "Latitude: ",
            coords()$lat,
            "<br>",
            "Longitude: ",
            coords()$lng
          )
        ))
      }
    })

    point_selected <- reactive({
      shiny::req(coords())
      point <- sf::st_as_sf(
        data.frame(
          "long" = c(coords()$lng),
          "lat" = c(coords()$lat)
        ),
        coords = c("long", "lat"),
        crs = "WGS84"
      )
      sf::st_transform(point, sf::st_crs(unidad_vecinales()))

    })

    unidad_vecinal_temp <- reactive({
      shiny::req(point_selected())
      sf::st_drop_geometry(get_uv(unidad_vecinales(), point_selected()))
    })

    observeEvent(coords(), {
      if (input$results_map$collapsed) { bs4Dash::updateBox("results_map", action = "toggle")}
      if (input$results_unidadvecinal$collapsed) { bs4Dash::updateBox("results_unidadvecinal", action = "toggle")}
      if (input$results_google$collapsed) { bs4Dash::updateBox("results_google", action = "toggle")}
    })

    unidad_vecinal_result <- reactive({
      shiny::req(unidad_vecinal_temp())
      if (!is.null(unidad_vecinal_temp())) {
        result <- t(unidad_vecinal_temp())
        result <- cbind("Feature" = rownames(result), result)
        rownames(result) <- 1:nrow(result)
        result <- as.data.frame(result)
        result <- result[1:(nrow(result) - 2), ]
        result
      }
    })

    output$map <- leaflet::renderLeaflet({
      shiny::req(unidad_vecinal_temp())
      tmap::tmap_options(list(
        basemaps = c(
          "OpenStreetMap",
          "Esri.WorldTopoMap",
          "Esri.WorldGrayCanvas"
        )
      ))
      tmap::tmap_leaflet(
        tmap::tm_shape(
          unidad_vecinales() |>
            filter(COD_COMUNA == unidad_vecinal_temp()$COD_COMUNA) |>
            mutate(
              color = case_when(
                CODIGO_UV == unidad_vecinal_temp()$CODIGO_UV ~ "Selected",
                T ~ "Others"
              )
            ),
          name = "Unidades Vecinales"
        ) +
          tmap::tm_polygons(
            col = "color",
            alpha = 0.7,
            popup.vars = c("Hogares" = "HOGARES",
                           "Personas" = "PERSONAS"),
            title = "Unidades Vecinales",
            id = "CODIGO_UV"
          ) +
          tmap::tm_shape(point_selected(),
                         name = "Direción") +
          tmap::tm_dots()
      )

    })



    output$unidad_vecinal <- DT::renderDT({
      result <- unidad_vecinal_result() |>
        dplyr::left_join(dic_variables, by = c("Feature" = "NOMBRE")) |>
        dplyr::select(`Description` = "DESCRIPCIÓN", `Value`="1")

      DT::datatable(
        result,
        style = "bootstrap4",
        extensions = "Scroller",
        escape = FALSE,
        rownames = FALSE,
        filter = "top",
        selection = "none",
        options = list(
          dom = "t",
          pageLength = 100,
          autowidth = TRUE,
          #scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 550
          #scroller = TRUE
        )
      )
    })
  })
}
