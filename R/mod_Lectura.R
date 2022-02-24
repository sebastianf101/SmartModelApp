#' Lectura UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
mod_Lectura_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = "menu", header = version_app,
    tabPanel("Subida", br(), 
             titlePanel("Subida de Archivos"), br(),
             fluidRow(
               column(4, 
                      fileInput(ns("upload_datos"), "Seleccionar archivos .csv de datos ", buttonLabel = "Subir...",
                       multiple = TRUE, accept = ".csv", placeholder = "archivos no seleccionados" ),
                      fileInput(ns("upload_param"), "Seleccionar el archivo .xlsx de parámetros ", buttonLabel = "Subir...",
                       multiple = FALSE, accept = ".xlsx", placeholder = "archivo no seleccionado" ), 
                      fileInput(ns("template"), "Seleccionar el template .Rmd para generar el Reporte ", buttonLabel = "Subir...",
                       multiple = FALSE, accept = ".Rmd", placeholder = "archivo no seleccionado" ), 
                      fileInput(ns("func_aux"), "Seleccionar funciones auxiliares ", buttonLabel = "Subir...",
                                multiple = FALSE, accept = ".R", placeholder = "archivo no seleccionado" ),
                      fileInput(ns("func_utils"), "Seleccionar utilidades para reportes ", buttonLabel = "Subir...",
                                multiple = FALSE, accept = ".R", placeholder = "archivo no seleccionado" )
                      ), 
               column(8, br(), gt_output(ns("file_datos")), br(), gt_output(ns("file_param")), 
                         br(), gt_output(ns("file_template")), br(), gt_output(ns("file_aux")), 
                         br(), gt_output(ns("file_utils"))
                      )  
               ) # fluidRow
             ), # tabPanel
    tabPanel("Lectura", br(),
           titlePanel("Lectura de Archivos"), br(),
           fluidRow(
             column(4,
                    p("Se asume que todos los archivos de datos son csv separados por ',', con nombres de columna."), br(), 
                    p("Si los archivos no cumplen los formatos especificados se advierte a continuación."), br(),
                    textOutput(ns("read_res_txt")), br(), br(), 
                    p("Nro. de filas leídas del archivo de datos"),
                    gt_output(ns("read_res_datos")), br(), 
                    p("Apretar el botón para generar y descargar el Reporte de Validación del modelo"), br(), 
                    downloadButton(ns('downloadReport'), label = "Descargar Reporte")
             ),
             column(8,
                    p("Parámetros cargados"), 
                    gt_output(ns("read_res_param"))
             )
           )
           ) # tabPanel
    ) # tabsetPanel
  ) # tagList
}
    
#' Lectura Server Functions
#' 
#' @noRd 
mod_Lectura_server <- function(id, r){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    r$reading_success <- FALSE
    ses_env <- rlang::current_env()
    
    output$file_datos <- render_gt({
      req(r$auth)
      req(input$upload_datos)
#      golem::cat_dev("Estoy en output$files_datos", r$auth, "\n")
      input$upload_datos |> select(name, size) |> gt() |>
        tab_header("Archivos de Datos") |>
        cols_label(name="Nombre", size="Tamaño")
    })
    
    output$file_param <- render_gt({
      req(r$auth)
      req(input$upload_param)
      input$upload_param |> select(name, size) |> gt() |>
        tab_header("Archivo de Parámetros") |>
        cols_label(name="Nombre", size="Tamaño")
    })
    
    output$file_template <- render_gt({
      req(r$auth)
      req(input$template)
      input$template |> select(name, size) |> gt() |>
        tab_header("Template") |>
        cols_label(name="Nombre", size="Tamaño")
    })

    output$file_aux <- render_gt({
      req(r$auth)
      req(input$func_aux)
      input$func_aux |> select(name, size) |> gt() |>
        tab_header("Funciones auxiliares") |>
        cols_label(name="Nombre", size="Tamaño")
    })
    
    output$file_utils <- render_gt({
      req(r$auth)
      req(input$func_utils)
      input$func_utils |> select(name, size) |> gt() |>
        tab_header("Utilidades para Reportes") |>
        cols_label(name="Nombre", size="Tamaño")
    })
    
    func_aux_success <- reactive({
      req(r$auth)
      req(input$func_aux$datapath)
      id <- showNotification("Cargando funciones auxiliares.  Espere!", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      input$func_aux$datapath |> sys.source(keep.source = F, envir = ses_env)
      TRUE      
    }) 

    func_utils_success <- reactive({
      req(r$auth)
      req(input$func_utils$datapath)
      id <- showNotification("Cargando utilidades para reportes.  Espere!", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      input$func_utils$datapath |> sys.source(keep.source = F, envir = ses_env)
      TRUE
    })
    
    df_work <- reactive({
      req(r$auth)
      req(input$upload_datos)
      id <- showNotification("Leyendo Archivos.  Espere!", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      input$upload_datos$datapath |> 
        vroom::vroom(delim = data_source_delim, escape_double = FALSE, trim_ws = TRUE, 
                     show_col_types = F)
    })
    
    reading_problems <- reactive({df_work() |> vroom::problems()})
    
    df_Param <- reactive({
      req(r$auth)
      req(input$upload_param)
      id <- showNotification("Leyendo Archivos.  Espere!", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      input$upload_param$datapath |> readxl::read_excel(sheet = 'Valid', range = "A6:D20") |> 
        rename(parameter=`Parámetro`, value=Valor, type=Tipo) |> 
        filter(!is.na(parameter) & !is.na(type)) 
    })
    
    load_Param_success <- reactive({
      req(df_Param()) 
      req(func_aux_success())
      df_Param() |> 
        select(parameter, value, type) |> 
        purrr::pwalk(par_init, ses_env)
      exists("data_source_delim")
    })

    tab_niv <- reactive({
      req(r$auth)
      req(load_Param_success())
      input$upload_param$datapath |> 
        load_range('Valid', 
                   df_Param() |> filter(parameter == "par_rango_niveles") |> pull(value), 
                   c("Nombre Nivel", "Regla", "Tasa de malos máxima"), 
                   c("level_name", "rule", "mx_allwd_br")) |> 
        arrange(level_order) |> mutate(mn_allwd_br = lag(mx_allwd_br, default = 0)) |> 
        rename(TM_min = mn_allwd_br, TM_max = mx_allwd_br) 
    })
    
    tab_seg <- reactive({
      req(r$auth)
      req(load_Param_success())
      req(func_aux_success())
      input$upload_param$datapath |> 
        load_range('Valid', 
                   df_Param() |> filter(parameter == "par_rango_segmentos") |> pull(value), 
                   c("Nombre Segmento", "Regla"), c("level_name", "rule"))
    })
    
    tab_rep <- reactive({
      req(r$auth)
      req(load_Param_success())
      req(func_aux_success())
      input$upload_param$datapath |> 
        load_range('Valid', 
                   df_Param() |> filter(parameter == "par_rango_reportes") |> pull(value), 
                   c("Variables de corte"), c("report_name"))
    })
    
    reading_success <- reactive({
      req(r$auth)   
      req(func_aux_success())
      req(func_utils_success())
      req(tab_niv())      
      req(tab_rep())
      req(tab_seg())
      validate(need(df_Param() |> filter(! type %in% c('list', 'numeric', 'string')) |> plyr::empty(), 
                    label = "Los Tipos admitidos son list, numeric o string."))
      validate(need(df_Param() |> filter(! type %in% c('list', 'numeric', 'string')) |> plyr::empty(), 
                    label = "Los Tipos admitidos son list, numeric o string."))
      validate(need(df_Param() |> filter(parameter %in% c('project_title', 'par_ids', 'par_target', 
                                                          'par_perf_bins', 'par_rango_niveles', 'par_rango_segmentos', 'par_rango_reportes', 
                                                          'par_cant_reportes', 'par_times')) |> summarise(q=n()) |> mutate(check = (q == 9)) |> 
                      pull(check), label = 'Parámetros incorrectos o faltantes.'))
      validate(need(tab_niv() |> pull(TM_max) |> check_sorted_score_levels(), 
                    label = "tabla de niveles de score no ordenada!"))
      validate(need(reading_problems() |> plyr::empty(), 
                    label = paste("Problemas en lectura de archivo de datos", reading_problems())))
      r$src <- normalizePath(input$template$datapath)
      r$df_Param <- df_Param()
      r$df_work <- df_work()
      r$tab_niv <- tab_niv() 
      r$tab_rep <- tab_rep()
      r$tab_seg <- tab_seg()
      r$reading_success <- TRUE
      TRUE
    })
    
    output$read_res_txt <- renderText({
      if (!reading_success()) "Hubo problemas leyendo los archivos!"
      else "Lectura exitosa! Cantidad de registros leídos y tabla de parámetros cargados"
    })
    
    output$read_res_param <- render_gt({
      req(reading_success())
      df_Param() |> gt() |> 
        cols_label(parameter="Parámetro", value="Valor") 
    })
    
    output$read_res_datos <- render_gt({
      req(reading_success())
      df_Param() |> filter(parameter == "par_target") |> pull(value) -> target_var
      df_work() |> 
        group_by(.data[[target_var]]) |> 
        count() |> 
        gt(groupname_col = NA) |> 
        cols_label(n="# Mediciones") |> 
        grand_summary_rows(columns = n, fns = list(Total = ~sum(.)), 
                           formatter = fmt_number, decimals = 0)
    })
 
    output$downloadReport <- downloadHandler(
      filename = function() { 'Reporte_Valid.html' },
      content = function(file) {
        req(reading_success())
        id <- showNotification("Generando documento. Espere!", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id), add = TRUE)
        src <- input$template$datapath |> fs::path_tidy()
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        knit_dir <- tempdir()
        owd <- setwd(knit_dir)
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        out <- rmarkdown::render(
          input = 'report.Rmd',
          params = list("knit_dir" = knit_dir, "df_Param" = df_Param(), "df_work" = df_work(),
                        "tab_niv" = tab_niv(), "tab_rep" = tab_rep(), "tab_seg" = tab_seg()),
          output_format = "html_document")
        file.rename(out, file)
        
        showNotification("Reporte Generado!", duration = 5, closeButton = FALSE)
        removeNotification(id)
      }
    )
    
  }) # mod_Lectura_server

}
    
## To be copied in the UI
# mod_Lectura_ui("Lectura_ui_1")
    
## To be copied in the server
# mod_Lectura_server("Lectura_ui_1")
