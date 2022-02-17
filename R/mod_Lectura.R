#' Lectura UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Lectura_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Lectura Server Functions
#'
#' @noRd 
mod_Lectura_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Lectura_ui("Lectura_ui_1")
    
## To be copied in the server
# mod_Lectura_server("Lectura_ui_1")
