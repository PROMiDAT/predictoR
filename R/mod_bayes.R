#' bayes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bayes_ui <- function(id){
  ns <- NS(id)
  codigo.bayes <- list(tags$div(style = "display:none;", options.run(ns("runBayes"))), tags$hr(style = "display:none;",),
                       conditionalPanel("input.BoxBayes == 'tabBayesModelo'",
                                        codigo.monokai(ns("fieldCodeBayes"), height = "10vh")),
                       conditionalPanel("input.BoxBayes == 'tabBayesPred'",
                                        codigo.monokai(ns("fieldCodeBayesPred"), height = "10vh")),
                       conditionalPanel("input.BoxBayes == 'tabBayesMC'",
                                        codigo.monokai(ns("fieldCodeBayesMC"), height = "10vh")),
                       conditionalPanel("input.BoxBayes == 'tabBayesIndex'",
                                        codigo.monokai(ns("fieldCodeBayesIG"), height = "10vh")))
  
  opc_bayes <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(95),
                            tabs.content = list(codigo.bayes))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxBayes", opciones = opc_bayes,
      tabPanel(title = labelInput("generatem"), value = "tabBayesModelo",
               withLoader(verbatimTextOutput(ns("txtbayes")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabBayesPred",
               withLoader(DT::dataTableOutput(ns("bayesPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabBayesMC",
               withLoader(plotOutput(ns('plot_bayes_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtbayesMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabBayesIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("bayesPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("bayesErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("bayesIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("bayesIndErrTable")))))
    )
  )
}
    
#' bayes Server Function
#'
#' @noRd 
mod_bayes_server <- function(input, output, session, updateData){
  ns <- session$ns

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
    updateAceEditor(session, "fieldCodeBayes", value = codigo)
    cod.bayes.modelo <<- codigo
    
    codigo <- bayes.prediccion()
    updateAceEditor(session, "fieldCodeBayesPred", value = codigo)
    cod.bayes.pred <<- codigo

    codigo <- bayes.MC()
    updateAceEditor(session, "fieldCodeBayesMC", value = codigo)
    cod.bayes.mc <<- codigo

    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeBayesIG", value = codigo)
    cod.bayes.ind <<- codigo
  }

  
  # Ejecuta el modelo, prediccion, mc e indices de bayyes
  bayes.full <- function() {
     ejecutar.bayes()
     ejecutar.bayes.pred()
     ejecutar.bayes.mc()
     ejecutar.bayes.ind()
  }
  
  # Genera el modelo
  ejecutar.bayes <- function() {
    tryCatch({
      exe(cod.bayes.modelo)
      output$txtbayes <- renderPrint(exe("modelo.bayes"))
 
      nombres.modelos <<- c(nombres.modelos, "modelo.bayes")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.bayes(1)
      showNotification(paste0("Error (BAYES-01) : ", e), duration = 15, type = "error")
    })
  }
  
  ejecutar.bayes.pred <- function() {
    tryCatch({ 
      idioma <- updateData$idioma
      
      exe(cod.bayes.pred)
      pred <- predict(modelo.bayes, datos.prueba, type = "prob")
      scores[["Bayes"]] <<- pred$prediction[,2]
      
      output$bayesPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.bayes"),idioma = idioma), server = FALSE)
 
      nombres.modelos <<- c(nombres.modelos, "prediccion.bayes")
      updateData$roc  <- !updateData$roc #graficar otra vez la curva roc
    },
    error = function(e) { 
      limpia.bayes(2)
      showNotification(paste0("Error (BAYES-02) : ", e), duration = 15, type = "error")
    })
  }
  
  ejecutar.bayes.mc <- function() {
    if(exists("prediccion.bayes")){
      tryCatch({ 
        idioma <- updateData$idioma
        
        exe(cod.bayes.mc)
        output$txtbayesMC <- renderPrint(print(exe("MC.bayes")))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot_bayes_mc <- renderPlot(exe("plot.MC(MC.bayes)"))

        nombres.modelos <<- c(nombres.modelos, "MC.bayes")
      },
      error = function(e) { 
        limpia.bayes(3)
        showNotification(paste0("Error (BAYES-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Genera los indices
  ejecutar.bayes.ind <- function(){
    if(exists("MC.bayes")){
      tryCatch({ 
        idioma <- updateData$idioma
        
        isolate(exe(cod.bayes.ind))
        indices.bayes <<- indices.generales(exe("MC.bayes"))

        output$bayesPrecGlob  <-  fill.gauges(indices.bayes[[1]], tr("precG",idioma))
        output$bayesErrorGlob <-  fill.gauges(indices.bayes[[2]], tr("errG",idioma))
        
        output$bayesIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.bayes,"BAYES", idioma = idioma)), spacing = "xs",
                                                       bordered = T, width = "100%", align = "c", digits = 2)
        output$bayesIndErrTable  <- shiny::renderTable(xtable(indices.error.table(indices.bayes,"BAYES")), spacing = "xs",
                                                      bordered = T, width = "100%", align = "c", digits = 2)
        nombres.modelos     <<- c(nombres.modelos, "indices.bayes")
        IndicesM[["Bayes"]] <<- indices.bayes
        updateData$selector.comparativa <- actualizar.selector.comparativa()
        },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.bayes(4)
        showNotification(paste0("Error (BAYES-04) : ",e), duration = 15, type = "error")
      })
    }
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
        output$bayesIndPrecTable <-  shiny::renderTable(NULL)
        output$bayesIndErrTable  <-  shiny::renderTable(NULL)
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
# mod_bayes_ui("bayes_ui_1")
    
## To be copied in the server
# callModule(mod_bayes_server, "bayes_ui_1")
 
