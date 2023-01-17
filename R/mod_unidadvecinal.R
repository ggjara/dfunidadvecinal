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
    column(
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
          HTML(
            paste0(
              " <script>
                function initAutocomplete() {

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
                 //console.log(address);
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
              "&libraries=places&callback=initAutocomplete' async defer></script>"
            )
          )
        )
      )
    )
  ),
  shiny::fluidRow(
    column(
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
    column(
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
mod_unidadvecinal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Waiter
    w_results <- waiter::Waiter$new(id = c(ns("results_map"),
                                           ns("results_unidadvecinal"),
                                           ns("results_google")),
                                    html = waiter::spin_3k(),
                                    color = waiter::transparent(0.5))

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
      sf::st_as_sf(
        data.frame(
          "long" = c(coords()$lng),
          "lat" = c(coords()$lat)
        ),
        coords = c("long", "lat"),
        crs = sf::st_crs(unidad_vecinales)
      )
    })

    unidad_vecinal_temp <- reactive({
      shiny::req(point_selected())
      sf::st_drop_geometry(get_uv(point_selected()))
    })

    observeEvent(coords(), {
      if (input$results_map$collapsed) { bs4Dash::updateBox("results_map", action = "toggle")}
      if (input$results_unidadvecinal$collapsed) { bs4Dash::updateBox("results_unidadvecinal", action = "toggle")}
      if (input$results_google$collapsed) { bs4Dash::updateBox("results_google", action = "toggle")}
    })

    unidad_vecinal_result <- reactive({
      shiny::req(coords())
      if (!is.null(coords())) {
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
      tm <- tmap::tm_shape(
        unidad_vecinales |>
          filter(COD_REGION == unidad_vecinal_temp()$COD_REGION) |>
          mutate(
            color = case_when(
              CODIGO_UV == unidad_vecinal_temp()$CODIGO_UV ~ "Selected",
              T ~ "Others"
            )
          )
      ) +
        tmap::tm_polygons(
          col = "color",
          alpha = 0.7,
          popup.vars = c("Hogares" = "HOGARES",
                         "Personas" = "PERSONAS"),
          title = "Unidades Vecinales",
          id = "CODIGO_UV"
        ) +
        tmap::tm_shape(point_selected()) +
        tmap::tm_dots()
      tmap::tmap_leaflet(tm)
    })



    output$unidad_vecinal <- DT::renderDT({
      result <- unidad_vecinal_result() |>
        dplyr::left_join(dic_variables, by = c("Feature" = "NOMBRE")) |>
        dplyr::rename("Value" = "1") |>
        dplyr::rename(`Description` = `DESCRIPCIÓN`) |>
        dplyr::select(`Description`, `Value`)

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
          scrollY = 600
          #scroller = TRUE
        )
      )
    })
  })
}

## To be copied in the UI
# mod_unidadvecinal_ui("unidadvecinal_1")

## To be copied in the server
# mod_unidadvecinal_server("unidadvecinal_1")
