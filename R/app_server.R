#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import gt
#' 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom shinyauthr login logout
#' @importFrom vroom vroom
#' @importFrom readxl read_excel
#' @importFrom plyr empty
#' 
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 

  # golem::cat_dev("Verifico que leyó user_base \n")
  # golem::cat_dev(user_base$user, "\n")

  r <- reactiveValues()
  # auth
  # mod_authentication_00_server("authentication_00_ui_1")
  # call the logout module with reactive trigger to hide/show
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password, 
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  observeEvent(credentials()$user_auth, {
    r$auth <- credentials()$user_auth
  })
  
  mod_Lectura_server("Lectura_ui_1", r)
  
}
