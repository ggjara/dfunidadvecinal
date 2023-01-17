#' conventions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_conventions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      id = ns("conventions_box"),
      width = 12,
      title = "Conventions",
      #icon = shiny::icon("list", lib = "glyphicon"),
      solidHeader = TRUE,
      collapsible = TRUE,
      "TBD"
      )
  )
}

#' conventions Server Functions
#'
#' @noRd
mod_conventions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_conventions_ui("conventions_1")

## To be copied in the server
# mod_conventions_server("conventions_1")
