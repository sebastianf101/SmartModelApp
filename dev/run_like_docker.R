
options( "golem.app.prod" = FALSE)
options('shiny.port'=85,shiny.host='0.0.0.0')

rlang::is_installed('SmartModelApp')
remotes::install_local(path = "C:/Users/sferro/Documents/Trabajo/R/Projects/SmartModelApp_0.90.tar.gz",
                       upgrade = "never", force = TRUE)
warnings()
SmartModelApp::run_app()
