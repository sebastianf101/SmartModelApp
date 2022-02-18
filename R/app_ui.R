
version_app <- 1.0

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyauthr loginUI logoutUI
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      h1("SmartModelApp"), 
      
      # authentication module
      # must turn shinyjs on
      shinyjs::useShinyjs(),
      # add logout button UI
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
      # add login panel UI function
      shinyauthr::loginUI(id = "login",
                          title = 'Por favor regístrese',
                          user_title = 'Usuario', pass_title = 'Contraseña', login_title = 'Ingresar'),
      tags$head(
        tags$style(
          HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
        )
      ),
      
      mod_Lectura_ui("Lectura_ui_1")
                  
      
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'SmartModelApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

