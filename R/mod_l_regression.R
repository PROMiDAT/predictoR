#' l_regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_l_regression_ui <- function(id){
  ns <- NS(id)
  codigo.rl <- list(tags$div(style = "display:none;", options.run(ns("runRl"))), tags$hr(style = "margin-top: 0px;"),
                    conditionalPanel("input.BoxRl == 'tabRlModelo'",
                                     codigo.monokai(ns("fieldCodeRl"), height = "10vh")),
                    conditionalPanel("input.BoxRl == 'tabRlPred'",
                                     codigo.monokai(ns("fieldCodeRlPred"), height = "10vh")),
                    conditionalPanel("input.BoxRl == 'tabRlMC'",
                                     codigo.monokai(ns("fieldCodeRlMC"), height = "10vh")),
                    conditionalPanel("input.BoxRl == 'tabRlIndex'",
                                     codigo.monokai(ns("fieldCodeRlIG"), height = "10vh")))
  
  opc_rl <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(95),
                         tabs.content = list(codigo.rl))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxRl",opciones = opc_rl,
      tabPanel(title = labelInput("generatem"), value = "tabRlModelo",
               withLoader(verbatimTextOutput(ns("txtrl")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabRlPred",
               withLoader(DT::dataTableOutput(ns("rlPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabRlMC",
               withLoader(plotOutput(ns('plot_rl_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtrlMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabRlIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("rlPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("rlErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("rlIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("rlIndErrTable")))))
    )
  )
}
    
#' l_regression Server Function
#'
#' @noRd 
mod_l_regression_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.rl()
  })
  
  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     ejecutar.rl.mc()
  #     ejecutar.rl.ind()
  #   }
  # })

  #Cuando se ejecuta el botón run
  observeEvent(input$runRl, {
    idioma <- updateData$idioma
    if (length(levels(datos[, variable.predecir])) == 2) {
      if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
        limpia.rl.run()
        rl.full()
      }
    }else{
      if (isFALSE(getOption("shiny.testmode")) || is.null(getOption("shiny.testmode"))) {
        showModal(modalDialog(
          title = "Regresión Logística", tr("limitModel", idioma),
          footer = modalButton("Cerrar"), easyClose = T
        ))
      }
    }
  }, priority =  -5)

  # Actualiza el código a la versión por defecto
  default.codigo.rl <- function() {
    # Se actualiza el código del modelo
    codigo <- rl.modelo(updateData$variable.predecir)
    updateAceEditor(session, "fieldCodeRl", value = codigo)
    cod.rl.modelo <<- codigo
    
    # Se genera el código de la prediccion
    codigo <- rl.prediccion()
    updateAceEditor(session, "fieldCodeRlPred", value = codigo)
    cod.rl.pred <<- codigo

    # Se genera el código de la matriz
    codigo <- rl.MC()
    updateAceEditor(session, "fieldCodeRlMC", value = codigo)
    cod.rl.mc <<- codigo

    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlIG", value = codigo)
    cod.rl.ind <<- codigo
  }
  
  # Ejecuta el modelo, predicción, mc e indices de rl
  rl.full <- function() {
    if (length(levels(datos[, variable.predecir])) == 2) {
       ejecutar.rl()
       ejecutar.rl.pred()
       ejecutar.rl.mc()
       ejecutar.rl.ind()
    }
  }
  
  #Genera el modelo
  ejecutar.rl <- function() {
    tryCatch({
      exe(cod.rl.modelo)
      output$txtrl    <- renderPrint(exe("modelo.rl"))
      nombres.modelos <<- c(nombres.modelos, "modelo.rl")
    },
    error = function(e) { 
      limpia.rl(1)
      showNotification(paste0("Error (RL-01) : ", e), duration = 15, type = "error")
    })
  }
  
  #Genera la predicción
  ejecutar.rl.pred <- function() {
    idioma <- updateData$idioma
    
    tryCatch({ 
      exe(cod.rl.pred)
      pred <- predict(modelo.rl, datos.prueba, type = 'prob')
      scores[["rl"]] <<- pred$prediction[,2]
      
      output$rlPrediTable <- DT::renderDataTable(obj.predic(prediccion.rl, idioma = idioma), server = FALSE)
 
      nombres.modelos <<- c(nombres.modelos, "prediccion.rl")
      #gráfica otra vez la curva roc
      updateData$roc  <- !updateData$roc 
    },
    error = function(e) { 
      limpia.rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  #Genera la matriz de confusión
  ejecutar.rl.mc <- function() {
    if(exists("prediccion.rl")){
      idioma <- updateData$idioma
      
      tryCatch({ 
        exe(cod.rl.mc)
        output$txtrlMC <- renderPrint(print(MC.rl))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot_rl_mc <- renderPlot(plot.MC(MC.rl))
        
        nombres.modelos   <<- c(nombres.modelos, "MC.rl")
      },
      error = function(e) { 
        limpia.rl(3)
        showNotification(paste0("Error (RL-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  #Genera los índices generales
  ejecutar.rl.ind <- function(){
    if(exists("MC.rl")){
      idioma <- updateData$idioma
      
      tryCatch({ 
        isolate(exe(cod.rl.ind))
        indices.rl <<- indices.generales(MC.rl)
        
        nombres <- c("rlPrecGlob", "rlErrorGlob")
        output$rlPrecGlob  <-  fill.gauges(indices.rl[[1]], tr("precG",idioma))
        output$rlErrorGlob <-  fill.gauges(indices.rl[[2]], tr("errG",idioma))
        
        output$rlIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.rl,"RL", idioma = idioma)), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$rlIndErrTable  <- shiny::renderTable(xtable(indices.error.table(indices.rl,"RL")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        nombres.modelos  <<- c(nombres.modelos, "indices.rl")
        IndicesM[["rl"]] <<- indices.rl
        updateData$selector.comparativa <- actualizar.selector.comparativa()
        },
      error = function(e) { 
        limpia.rl(4)
        showNotification(paste0("Error (RL-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Limpia los datos según el proceso donde se genera el error
  limpia.rl <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.rl    <<- NULL
        output$txtrl <- renderPrint(invisible(""))
      }, {
        prediccion.rl       <<- NULL
        output$rlPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.rl             <<- NULL
        output$plot_rl_mc <- renderPlot(NULL)
        output$txtrlMC    <- renderPrint(invisible(NULL))
      }, {
        indices.rl            <<- NULL
        IndicesM[["rl"]]      <<- NULL
        output$rlIndPrecTable <- shiny::renderTable(NULL)
        output$rlIndErrTable  <- shiny::renderTable(NULL)
        output$rlPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$rlErrorGlob    <-  flexdashboard::renderGauge(NULL)
        
      })
    }
  }
  
  # Limpia los datos al ejecutar el botón run
  limpia.rl.run <- function() {
        output$txtrl          <- renderPrint(invisible(""))
        output$rlPrediTable   <- DT::renderDataTable(NULL)
        output$plot_rl_mc     <- renderPlot(NULL)
        output$txtrlMC        <- renderPrint(invisible(NULL))
        output$rlIndPrecTable <- shiny::renderTable(NULL)
        output$rlIndErrTable  <- shiny::renderTable(NULL)
        output$rlPrecGlob     <- flexdashboard::renderGauge(NULL)
        output$rlErrorGlob    <- flexdashboard::renderGauge(NULL)
  }
  
  # Limpia todos los datos
  limpiar <- function(){
        limpia.rl(1)
        limpia.rl(2)
        limpia.rl(3)
        limpia.rl(4)
  }
}
    
## To be copied in the UI
# mod_l_regression_ui("l_regression_ui_1")
    
## To be copied in the server
# callModule(mod_l_regression_server, "l_regression_ui_1", updateData)
 
