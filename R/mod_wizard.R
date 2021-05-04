#' wizard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wizard_ui <- function(id){
  ns <- NS(id)
  codigo.bayes <- list(tags$div(style = "display:none;", options.run(ns("runBayes"))), tags$hr(style = "display:none;",))
  library(shinyglide)

  opc_bayes <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(20),
                           tabs.content = list(codigo.bayes))
  tagList(

tabBoxPrmdt(

  id = "BoxBayes", opciones = opc_bayes,
  tabPanel(title = labelInput("generatem"), value = "tabBayesModelo",id = ns("modelo"),
           verbatimTextOutput(ns("txtbayes")), hr(),
           fluidRow(col_11(),
                    col_1(actionButton(ns("modelobtn"), labelInput("next"), width = "100%",
                        icon = icon("forward")))), hr(),
           buttonsWizard(ids = list(ns("modelobtn2"),ns("modelobtn3")))),

  tabPanel(title = labelInput("predm"), value = "tabBayesPred",
           DT::dataTableOutput(ns("bayesPrediTable")), hr(),
           fluidRow(col_1(actionButton (ns("predback"), labelInput("next"), width = "100%",
                                        icon = icon("backward"))),col_10(),
                    col_1(actionButton(ns("predforw"), labelInput("next"), width = "100%",
                                       icon = icon("forward")))), hr()),

  tabPanel(title = labelInput("mc"), value = "tabBayesMC",
           plotOutput(ns('plot_bayes_mc'), height = "45vh"), hr(),
           fluidRow(col_1(actionButton (ns("mcback"), labelInput("next"), width = "100%",
                                        icon = icon("backward"))),col_10(),
                    col_1(actionButton(ns("mcforw"), labelInput("next"), width = "100%",
                                       icon = icon("forward")))), hr()),
  
  tabPanel(title = labelInput("indices"), value = "tabBayesIndex",
           fluidRow(col_6(flexdashboard::gaugeOutput(ns("bayesPrecGlob"), width = "100%")),
                    col_6(flexdashboard::gaugeOutput(ns("bayesErrorGlob"), width = "100%"))),
           fluidRow(col_12(shiny::tableOutput(ns("bayesIndPrecTable")))),
           fluidRow(col_12(shiny::tableOutput(ns("bayesIndErrTable")))), hr(),
           fluidRow(col_1(actionButton (ns("indback"), labelInput("next"), width = "100%",
                                        icon = icon("backward"))),col_10()), hr())

)
  )
}
    
#' wizard Server Function
#'
#' @noRd 
mod_wizard_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  mostrar.tabs <- function(mostrar = FALSE, menu.values){
    # menu.values <- c(" a[data-value=tabBayesPred]")
    # mostrar.tabs(FALSE, menu.values)
    
    element <- "#BoxBayes li"
    lapply(menu.values, function(i){
      if(mostrar) {
        shinyjs::enable(selector = paste0(element, i))
        shinyjs::show(selector = paste0(element, i))
        
      } else {
        shinyjs::disable(selector = paste0(element, i))
        shinyjs::hide(selector = paste0(element, i))
        
      }
    })
  }

  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.bayes()
  })
  
  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     ejecutar.bayes.mc()
  #     ejecutar.bayes.ind()
  #   }
  # })
  # 
  observeEvent(input$runBayes, {
    if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
      limpia.bayes.run()
      default.codigo.bayes()
      bayes.full()
    }
  }, priority =  -5)
  
  
  default.codigo.bayes <- function() {
    
    codigo <- bayes.modelo(updateData$variable.predecir)
    cod.bayes.modelo <<- codigo
    
    codigo <- bayes.prediccion()
    cod.bayes.pred <<- codigo
    
    codigo <- bayes.MC()
    cod.bayes.mc <<- codigo
    
    codigo <- extract.code("indices.generales")
    cod.bayes.ind <<- codigo
  }
  
  
  # Ejecuta el modelo, prediccion, mc e indices de bayyes
  bayes.full <- function() {
    ejecutar.bayes()
  }
  
  # Genera el modelo
  ejecutar.bayes <- function() {
    tryCatch({
      exe(cod.bayes.modelo)
      output$txtbayes <- renderPrint(exe("modelo.bayes"))
      
      nombres.modelos <<- c(nombres.modelos, "modelo.bayes")
      menu.values <- c(" a[data-value=tabBayesPred]")
      mostrar.tabs(FALSE, menu.values)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.bayes(1)
      showNotification(paste0("Error (BAYES-01) : ", e), duration = 15, type = "error")
    })
  }
  # Limpia los datos segun el proceso donde se genera el error
  limpia.bayes <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.bayes    <<- NULL
        output$txtbayes <- renderPrint(invisible(""))
      }, {
        prediccion.bayes       <<- NULL
        output$bayesPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.bayes             <<- NULL
        output$plot_bayes_mc <- renderPlot(NULL)
        output$txtbayesMC    <- renderPrint(invisible(NULL))
      }, {
        indices.bayes            <<- NULL
        IndicesM[["Bayes"]]      <<- indices.bayes
        output$bayesIndPrecTable <- shiny::renderTable(NULL)
        output$bayesIndErrTable  <- shiny::renderTable(NULL)
        output$bayesPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$bayesErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }
  # Limpia los datos segun el proceso donde se genera el error
  limpia.bayes.run <- function() {
    output$txtbayes          <- renderPrint(invisible(""))
    output$bayesPrediTable   <- DT::renderDataTable(NULL)
    output$plot_bayes_mc     <- renderPlot(NULL)
    output$txtbayesMC        <- renderPrint(invisible(NULL))
    output$bayesIndPrecTable <- shiny::renderTable(NULL)
    output$bayesIndErrTable  <- shiny::renderTable(NULL)
    output$bayesPrecGlob     <- flexdashboard::renderGauge(NULL)
    output$bayesErrorGlob    <- flexdashboard::renderGauge(NULL)
  }
  
  limpiar <- function(){
    limpia.bayes(1)
    limpia.bayes(2)
    limpia.bayes(3)
    limpia.bayes(4)
  }
}
    
## To be copied in the UI
# mod_wizard_ui("wizard_ui_1")
    
## To be copied in the server
# callModule(mod_wizard_server, "wizard_ui_1")
 
