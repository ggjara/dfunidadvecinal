#' @import polished
#' @import shiny

# Polished configuration options
# app_name, and api_key are retrieved from the .Renv file
# through the config package (./config.yml)
polished::polished_config(
  app_name = config::get("polished_app_name"),
  api_key =  config::get("polished_api_key")
)

#' Polished custom sign in
#'
#' @description Polished Custom sign in style to pass as parameter to
#' `polished::secure_ui` as `sign_in_page_ui` = `polished_custom_sign_in``
#'
#' @return A polished::sign_in_ui_default()
#'
#' @noRd
polished_custom_sign_in <- polished::sign_in_ui_default(
  color = "#FFFFFF",
  button_color = "#17153a",
  company_name = "dfunidadvecinal",
  logo_top = tagList(
    golem_add_external_resources(),
    tags$img(src = "www/img/DFL_icono_Color_ALTA.png",
             alt = "DFundamental Logo",
             style = "width: 200px; margin-top: 30px; margin-bottom: 30px;")
  ),
  logo_bottom = tags$img(src = "www/img/DFL_RGB_color_ALTA.png",
                         alt = "DFundamental logo",
                         style = "width: 300px; margin-bottom: 15px; padding-top: 15px;"),
  icon_href = "favicon.ico",
)


polished_custom_sign_in_2 <- polished::sign_in_ui_default(
  color = "#FFFFFF",
  button_color = "#17153a",
  company_name = "DFundamental",
  logo_top = tagList(
    tags$img(src = "www/img/DFL_icono_Color_ALTA.png",
             alt = "DFundamental Logo",
             style = "width: 125px; margin-top: 30px; margin-bottom: 30px;"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r121/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/vanta@latest/dist/vanta.birds.min.js"),
    tags$script(
      '
        VANTA.BIRDS({
        el: "body",
        mouseControls: true,
        touchControls: true,
        gyroControls: false,
        minHeight: 200.00,
        minWidth: 200.00,
        scale: 1.00,
        scaleMobile: 1.00,
        backgroundColor: 0xffffff,
        color1: 0x594e4e,
        wingSpan: 24.00,
        cohesion: 1.00,
        backgroundAlpha: 0.39
})
      '
    )
  ),
  icon_href = "www/favicon.ico"#,
  #background_image = "images/milky_way.jpeg"
)



#' Polished custom admin button
#'
#' @description Polished custom admin button to pass as parameter to
#' `polished::secure_ui`as `custom_admin_button_ui`= `polished_custom_admin_button``
#'
#' @return A tagList containing a shiny::actionButton
#'
#' @noRd
polished_custom_admin_button <-
  shiny::tagList(
    shiny::actionButton(
      shiny::NS("polished", "go_to_admin_panel"),
      "",
      icon = shiny::icon("cog"),
      class = "btn-primary",
      style = paste0(
        "position: fixed; ",
        "bottom",
        ": 15px; ",
        "left",
        ": 15px; z-index: 9999;"
      )
    )
  )
