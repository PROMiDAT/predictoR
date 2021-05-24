#' neural_net UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_neural_net_ui <- function(id){
  ns <- NS(id)
  opciones.nn <- list(options.run(ns("runNn")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(col_6(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                                   min = 0, step = 0.01, value = 0.05)),
                               col_6(numericInput(ns("stepmax.nn"),labelInput("stepmax"),
                                                   min = 100, step = 100, value = 5000))),
                      fluidRow(col_12(sliderInput(inputId = ns("cant.capas.nn"), min = 1, max = 10,
                                                  label = labelInput("selectCapas"), value = 2))),
                      fluidRow(id = ns("capasFila"),lapply(1:10, function(i) tags$span(col_2(numericInput(ns(paste0("nn.cap.",i)), NULL,
                                                                                                min = 1, step = 1, value = 2),
                                                                                   class = "mini-numeric-select")))))
  
  codigo.nn <- list(conditionalPanel("input.BoxNn == 'tabNnModelo'",
                                     codigo.monokai(ns("fieldCodeNn"), height = "10vh")),
                    conditionalPanel("input.BoxNn == 'tabNnPlot'",
                                     codigo.monokai(ns("fieldCodeNnPlot"), height = "10vh")),
                    conditionalPanel("input.BoxNn == 'tabNnPred'",
                                     codigo.monokai(ns("fieldCodeNnPred"), height = "10vh")),
                    conditionalPanel("input.BoxNn == 'tabNnMC'",
                                     codigo.monokai(ns("fieldCodeNnMC"), height = "10vh")),
                    conditionalPanel("input.BoxNn == 'tabNnIndex'",
                                     codigo.monokai(ns("fieldCodeNnIG"), height = "10vh")))
  
  opc_nn <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(75,100), heights = c(95, 95),
                         tabs.content = list(opciones.nn, codigo.nn))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxNn", opciones = opc_nn,
      tabPanel(title = labelInput("generatem"), value = "tabNnModelo",
               withLoader(verbatimTextOutput(ns("txtnn")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("redPlot"), value = "tabNnPlot",
               withLoader(plotOutput(ns('plot_nn'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabNnPred",
               withLoader(DT::dataTableOutput(ns("nnPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabNnMC",
               withLoader(plotOutput(ns('plot_nn_mc'), height = "45vh"), 
               type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtnnMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabNnIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("nnPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("nnErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("nnIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("nnIndErrTable")))))
    )
  )
}
    
#' neural_net Server Function
#'
#' @noRd 
mod_neural_net_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  #Actualiza la cantidad de capas ocultas
  observeEvent(c(input$cant.capas.nn, updateData$datos.aprendizaje), {
    if(!is.null(updateData$datos.aprendizaje) && !is.null(input$cant.capas.nn)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn) {
          shinyjs::show(paste0("nn.cap.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.", i))
        }
      }
    }
  })
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.nn()
  })
  
  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     ejecutar.nn.mc()
  #     ejecutar.nn.ind()
  #   }
  # })

  #Cuando se ejecuta el botón run
  observeEvent(input$runNn, {
    if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { 
        limpia.nn.run()
        default.codigo.nn()
        nn.full()
    }
  }, priority =  -5)

  # Actualiza el código a la versión por defecto
  default.codigo.nn <- function(){
    #Modelo
    codigo <- nn.modelo(updateData$variable.predecir,
                        input$threshold.nn,
                        input$stepmax.nn,
                        input$cant.capas.nn,
                        input$nn.cap.1,input$nn.cap.2,
                        input$nn.cap.3,input$nn.cap.4,
                        input$nn.cap.5,input$nn.cap.6,
                        input$nn.cap.7,input$nn.cap.8,
                        input$nn.cap.9,input$nn.cap.10)

    updateAceEditor(session, "fieldCodeNn", value = codigo)
    cod.nn.modelo <<- codigo

    #Neuralnet PLot
    updateAceEditor(session, "fieldCodeNnPlot", value = nn.plot())

    #Predicción
    codigo <- nn.prediccion()
    updateAceEditor(session, "fieldCodeNnPred", value = codigo)
    cod.nn.pred <<- codigo

    #Matriz de Confusión
    codigo <- nn.MC()
    updateAceEditor(session, "fieldCodeNnMC", value = codigo)
    cod.nn.mc <<- codigo

    #Indices Generales
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeNnIG", value = codigo)
    cod.nn.ind <<- codigo
  }
  
  # Ejecuta el modelo, predicción, mc e indices de nn
  nn.full <- function() {
     ejecutar.nn()
     if(NN_EXECUTION){
        ejecutar.nn.pred()
        ejecutar.nn.mc()
        ejecutar.nn.ind()
     }
  }
  
  #Genera el modelo
  ejecutar.nn <- function() {
    idioma <- updateData$idioma
    tryCatch({ 
      isolate(exe(cod.nn.modelo))
      output$txtnn <- renderPrint(print(modelo.nn))
      plotear.red()
      nombres.modelos <<- c(nombres.modelos,"modelo.nn")
      NN_EXECUTION <<- TRUE
    },
    error = function(e) { 
      limpia.nn(1)
      showNotification(paste0("Error (NN-01) : ",e), duration = 15, type = "error")
    },
    warning = function(w){
      limpia.nn(1)
      NN_EXECUTION <<- FALSE
      showNotification(paste0(tr("nnWar", idioma)," (NN-01) : ",w), duration = 10, type = "warning")
    })
  }
  
  #Genera la predicción
  ejecutar.nn.pred <- function() {
    idioma <- updateData$idioma
    tryCatch({ 
      isolate(exe(cod.nn.pred))
      pred   <- predict(exe("modelo.nn"), datos.prueba, type = "prob")
      scores[["nn"]] <<- pred$prediction[,2]

      output$nnPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.nn"),idioma = idioma),server = FALSE)
      
       
      nombres.modelos <<- c(nombres.modelos,"prediccion.nn")
      #gráfica otra vez la curva roc
      updateData$roc  <- !updateData$roc 
      
    },
    error = function(e) { 
      limpia.nn(2)
      showNotification(paste0("Error (NN-02) : ",e), duration = 15, type = "error")
    })
  }
  
  #Matriz de Confusión
  ejecutar.nn.mc <- function() {
    idioma <- updateData$idioma
    if(exists("prediccion.nn")){
      
      tryCatch({ 
        exe(cod.nn.mc)
        output$txtnnMC    <- renderPrint(print(exe("MC.nn")))
        exe(plot.MC.code(idioma = idioma))
        output$plot_nn_mc <- renderPlot(exe("plot.MC(MC.nn)"))
         
        nombres.modelos <<- c(nombres.modelos, "MC.nn")
      },
      error = function(e){ 
        limpia.nn(3)
        showNotification(paste0("Error (NN-03) : ", e), duration = 15, type = "error")
      })
    }
  }
  
  #Indices Generales
  ejecutar.nn.ind <- function() {
    idioma <- updateData$idioma
    if(exists("MC.nn")){
      tryCatch({ 
        isolate(exe(cod.nn.ind))
        indices.nn <<- indices.generales(exe("MC.nn"))
        
        output$nnPrecGlob  <-  fill.gauges(indices.nn[[1]], tr("precG",idioma))
        output$nnErrorGlob <-  fill.gauges(indices.nn[[2]], tr("errG",idioma))
        
        output$nnIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.nn,"Redes Neuronales", idioma = idioma)), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$nnIndErrTable  <- shiny::renderTable(xtable(indices.error.table(indices.nn,"Redes Neuronales")), spacing = "xs",
                                                   bordered = T, width = "100%", align = "c", digits = 2)
        
        IndicesM[["nn"]] <<- indices.nn
        updateData$selector.comparativa <- actualizar.selector.comparativa()
      },
      error = function(e) { 
        limpia.nn(4)
        showNotification(paste0("Error (NN-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  #Genera el gráfico de la red neuronal
  plotear.red <- function(){
    idioma <- updateData$idioma
    tryCatch({
      capas <- c(input$nn.cap.1, input$nn.cap.2,
                 input$nn.cap.3, input$nn.cap.4,
                 input$nn.cap.5, input$nn.cap.6,
                 input$nn.cap.7, input$nn.cap.8,
                 input$nn.cap.9, input$nn.cap.10)
      
      capas <- capas[1:input$cant.capas.nn]
      if(input$cant.capas.nn * sum(capas) <= 1500 & ncol(modelo.nn$covariate) <= 20){
        output$plot_nn <- renderPlot(isolate(exe(input$fieldCodeNnPlot)))
        cod <- ifelse(input$fieldCodeNnPlot == "", nn.plot(), input$fieldCodeNnPlot)
      }else{
        showNotification(tr("bigPlot",idioma), duration = 10, type = "message")
        output$plot_nn <- renderPlot(NULL)
      }
    },
    error = function(e){
      output$plot_nn <- renderPlot(NULL)
    })
  }
  
  # Limpia los datos según el proceso donde se genera el error
  limpia.nn <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.nn      <<- NULL
        output$txtnn   <- renderPrint(invisible(""))
        output$plot_nn <- renderPlot(NULL)
      }, {
        prediccion.nn       <<- NULL
        output$nnPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.nn             <<- NULL
        output$plot_nn_mc <- renderPlot(NULL)
        output$txtNnMC    <- renderPrint(invisible(NULL))
      }, {
        indices.nn            <<- rep(0, 10)
        IndicesM[["nn"]]      <<- NULL
        output$nnIndPrecTable <- shiny::renderTable(NULL)
        output$nnIndErrTable  <- shiny::renderTable(NULL)
        output$nnPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$nnErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }
  
  # Limpia los datos al ejecutar el botón run
  limpia.nn.run <- function() {
        output$txtnn          <- renderPrint(invisible(""))
        output$plot_nn        <- renderPlot(NULL)
        output$nnPrediTable   <- DT::renderDataTable(NULL)
        output$plot_nn_mc     <- renderPlot(NULL)
        output$txtNnMC        <- renderPrint(invisible(NULL))
        output$nnIndPrecTable <- shiny::renderTable(NULL)
        output$nnIndErrTable  <- shiny::renderTable(NULL)
        output$nnPrecGlob     <- flexdashboard::renderGauge(NULL)
        output$nnErrorGlob    <- flexdashboard::renderGauge(NULL)
  }
  
  # Limpia todos los datos
  limpiar <- function(){
        limpia.nn(1)
        limpia.nn(2)
        limpia.nn(3)
        limpia.nn(4)
  }
  
}
    
## To be copied in the UI
# mod_neural_net_ui("neural_net_ui_1")
    
## To be copied in the server
# callModule(mod_neural_net_server, "neural_net_ui_1", updateData)
 
