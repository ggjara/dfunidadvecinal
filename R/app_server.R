#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Polished Server-Side
  observeEvent(input$sign_out, {
    sign_out_from_shiny()
    session$reload()
  })


  output$user_panel <- renderUser({
    dashboardUser(
      name = stringr::str_replace(session$userData$user()$email, "@.*", ""),
      image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      title = session$userData$user()$email,
      subtitle = if (session$userData$user()$is_admin) "Admin" else "User",
      # footer = p("The footer", class = "text-center"),
      dashboardUserItem(
        width = 12,
        tagList(
          bs4Dash::actionButton(
            inputId = "sign_out",
            label = "Sign Out",
            icon = icon("sign-out-alt"),
            # width = "100px",
            size = "sm",
            status = "danger"
          ),
          if (session$userData$user()$is_admin) {
            bs4Dash::actionButton(
              inputId = NS("polished", "go_to_admin_panel"),
              label = "Admin panel",
              # icon = icon("sign-out-alt"),
              # width = "100px",
              size = "sm",
              status = "secondary"
            )
          }
        )
      )
    )
  })

  tab_selected <- reactive(
    input$tab_selected
  )

  # # Initialize waiter when loading course
  # w <- waiter::Waiter$new(
  #   html = shiny::tagList(
  #     waiter::spin_pixel(),
  #     shiny::tags$br(),
  #     shiny::h5("Loading course...")
  #   ),
  #   color = "#343a40",
  #   fadeout = 500
  # )
  r <- shiny::reactiveValues(
    unidadvecinal_downloaded = FALSE,
    manzanacensal_downloaded = FALSE
  )

  mod_unidadvecinal_server("unidadvecinal_1", tab_selected, r)
  mod_manzanacensal_server("manzanacensal_1", tab_selected, r)
}
