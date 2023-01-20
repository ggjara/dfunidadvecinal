#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bs4Dash shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Use shinyjs

    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      preloader = list(html = waiter::spin_pixel()),
      dark = NULL,
      # Deactivates light/darktoggle
      title = "DFunidadVecinal",
      fullscreen = TRUE,
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "dfunidadvecinal",
          color = "primary",
          image = "www/img/DFL_square_logo.png"
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = shiny::icon("bars"),
        controlbarIcon = shiny::icon("th"),
        fixed = FALSE,
        leftUi = NULL,
        rightUi = bs4Dash::userOutput("user_panel")
      ),
      sidebar = bs4Dash::bs4DashSidebar(
        id = "sidebar",
        status = "primary",
        elevation = 3,
        collapsed = TRUE,
        bs4Dash::bs4SidebarMenu(
          id = "tab_selected",
          bs4Dash::bs4SidebarHeader(shiny::textOutput(outputId = "sidebar_intro")),
          bs4Dash::bs4SidebarMenuItem(
            "Unidad Vecinal",
            tabName = "unidadvecinal",
            icon = icon(
              lib = "font-awesome",
              "house"
            )
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Manzana Censal",
            tabName = "manzanacensal",
            icon = icon(
              lib = "font-awesome",
              "apple-whole"
            )
          )
        )
      ),
      footer = bs4Dash::dashboardFooter(
        left = "Created by DFundamental",
        right = "2022"
      ),
      body = bs4Dash::dashboardBody(
        shinyjs::useShinyjs(),
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "unidadvecinal",
            mod_unidadvecinal_ui("unidadvecinal_1")
          ),
          bs4Dash::tabItem(
            tabName = "manzanacensal",
            mod_manzanacensal_ui("manzanacensal_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dfunidadvecinal"
    )
  )
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert())
}
