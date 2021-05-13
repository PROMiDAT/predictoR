#' r_forest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_r_forest_ui <- function(id){
  ns <- NS(id)
  opciones.rf <- list(options.run(ns("runRf")), tags$hr(style = "margin-top: 0px;"),
                      conditionalPanel("input.BoxRf != 'tabRfRules'",
                                       fluidRow(col_6(numericInput(ns("ntree.rf"), labelInput("numTree"), 20, width = "100%", min = 0)),
                                                col_6(numericInput(ns("mtry.rf"),labelInput("numVars"),1, width = "100%", min = 1)))),
                      conditionalPanel("input.BoxRf == 'tabRfRules'",
                                       numericInput(ns("rules.rf.n"),labelInput("ruleNumTree"),1, width = "100%", min = 1)))
  
  codigo.rf  <- list(conditionalPanel("input.BoxRf == 'tabRfModelo'",
                                      codigo.monokai(ns("fieldCodeRf"), height = "10vh")),
                     conditionalPanel("input.BoxRf == 'tabRferror'",
                                      codigo.monokai(ns("fieldCodeRfPlotError"), height = "10vh")),
                     conditionalPanel("input.BoxRf == 'tabRfImp'",
                                      codigo.monokai(ns("fieldCodeRfPlot"), height = "10vh")),
                     conditionalPanel("input.BoxRf == 'tabRfPred'",
                                      codigo.monokai(ns("fieldCodeRfPred"), height = "10vh")),
                     conditionalPanel("input.BoxRf == 'tabRfMC'",
                                      codigo.monokai(ns("fieldCodeRfMC"), height = "10vh")),
                     conditionalPanel("input.BoxRf == 'tabRfIndex'",
                                      codigo.monokai(ns("fieldCodeRfIG"), height = "10vh")),
                     conditionalPanel("input.BoxRf == 'tabRfRules'",
                                      codigo.monokai(ns("fieldCodeRfRules"), height = "10vh")))
  
  opc_rf  <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(65, 95),
                          tabs.content = list(opciones.rf, codigo.rf))
  

  tagList(
    tabBoxPrmdt(
      id = "BoxRf",opciones = opc_rf,
      tabPanel(title = labelInput("generatem"),value = "tabRfModelo",
               withLoader(verbatimTextOutput(ns("txtRf")),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("evolerror"), value = "tabRferror",
               withLoader(echarts4rOutput(ns('plot_error_rf'), height = "55vh"),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("varImp"), value = "tabRfImp",
               withLoader(echarts4rOutput(ns('plot_rf'), height = "55vh"),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabRfPred",
               withLoader(DT::dataTableOutput(ns("rfPrediTable")),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabRfMC",
               withLoader(plotOutput(ns('plot_rf_mc'), height = "45vh"),
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtRfMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabRfIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("rfPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("rfErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("rfIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("rfIndErrTable"))))),
      
      tabPanel(title = labelInput("reglas"), value = "tabRfRules",
               withLoader(verbatimTextOutput(ns("rulesRf")),
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' r_forest Server Function
#'
#' @noRd 
mod_r_forest_server <- function(input, output, session, updateData){
  ns <- session$ns
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.rf(rf.def = TRUE)
  })

  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     ejecutar.rf.mc()
  #     ejecutar.rf.ind()
  #   }
  # })

  # Ejecuta el modelo, prediccion, mc e indices de rf
  rf.full <- function() {
    ejecutar.rf()
    ejecutar.rf.pred()
    ejecutar.rf.mc()
    ejecutar.rf.ind()
  }

  # Cuando se genera el modelo rf
  observeEvent(input$runRf, {
    if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
      limpia.rf.run()
      default.codigo.rf()
      rf.full()
    }
  }, priority =  -5)
  
  # Genera el modelo
  ejecutar.rf <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.modelo))
      output$txtRf <- renderPrint(print(modelo.rf))
      
      plotear.rf.imp()
      plotear.rf.error()
      updateAceEditor(session, "fieldCodeRfPlotError", value = plot.rf.error())
      
      mostrar.reglas.rf(input$rules.rf.n)
      nombres.modelos <<- c(nombres.modelos, "modelo.rf")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(1)
      showNotification(paste0("Error (RF-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Genera la prediccion
  ejecutar.rf.pred <- function() {
    tryCatch({ 
      idioma <- updateData$idioma
      exe(cod.rf.pred)
      pred   <-  predict(exe("modelo.rf"), datos.prueba, type = "prob")
      scores[["rfl"]]     <<- pred$prediction[,2]
      output$rfPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.rf"),idioma = idioma), server = FALSE)

      nombres.modelos <<- c(nombres.modelos, "prediccion.rf")
      updateData$roc  <- !updateData$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }

  ejecutar.rf.mc <- function() {
    idioma <- updateData$idioma
    
    if(exists("prediccion.rf")){
      tryCatch({ 
        exe(cod.rf.mc)
        output$txtRfMC    <- renderPrint(print(exe("MC.rf")))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot_rf_mc <- renderPlot(isolate(exe("plot.MC(MC.rf)")))
       
        nombres.modelos   <<- c(nombres.modelos, "MC.rf")
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rf(3)
        showNotification(paste0("Error (RF-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  ejecutar.rf.ind <- function() {
    idioma <- updateData$idioma
    if(exists("MC.rf")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rf.ind))
        indices.rf <<- indices.generales(exe("MC.rf"))
        
        output$rfPrecGlob  <-  fill.gauges(indices.rf[[1]], tr("precG",idioma))
        output$rfErrorGlob <-  fill.gauges(indices.rf[[2]], tr("errG",idioma))
        
        
        output$rfIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.rf,"Bosques Aleatorios", idioma = idioma)), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$rfIndErrTable  <- shiny::renderTable(xtable(indices.error.table(indices.rf,"Bosques Aleatorios")), spacing = "xs",
                                                   bordered = T, width = "100%", align = "c", digits = 2)
        
        nombres.modelos   <<- c(nombres.modelos, "indices.rf")
        IndicesM[["rfl"]] <<- indices.rf
        updateData$selector.comparativa <- actualizar.selector.comparativa()
        
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rf(4)
        showNotification(paste0("Error (RF-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  default.codigo.rf <- function(rf.def = FALSE){
    if((!is.null(datos.aprendizaje) & rf.def) | is.na(input$mtry.rf)){
      mtry.value <- ifelse(rf.def || is.na(input$mtry.rf), round(sqrt(ncol(datos.aprendizaje))), input$mtry.rf)
      if(!is.na(input$mtry.rf)){
        updateNumericInput(session,"mtry.rf",value = mtry.value)
      }
    }else{
      mtry.value <- input$mtry.rf
    }
    
    # Se actualiza el codigo del modelo
    codigo <- rf.modelo(variable.pr = updateData$variable.predecir,
                        ntree = input$ntree.rf,
                        mtry = mtry.value)
    
    updateAceEditor(session, "fieldCodeRf", value = codigo)
    cod.rf.modelo <<- codigo
    
    # Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlotError", value = plot.rf.error())
    updateAceEditor(session, "fieldCodeRfPlot", value = rf.plot())
     

    # Se genera el codigo de la prediccion
    codigo <- rf.prediccion()
    updateAceEditor(session, "fieldCodeRfPred", value = codigo)
    cod.rf.pred <<- codigo


    # Se genera el codigo de la matriz
    codigo <- rf.MC()
    updateAceEditor(session, "fieldCodeRfMC", value = codigo)
    cod.rf.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRfIG", value = codigo)
    cod.rf.ind <<- codigo
  }
  
  # Grafico de importancia
  plotear.rf.imp <- function() {
    tryCatch({
      output$plot_rf <- renderEcharts4r(isolate(exe(input$fieldCodeRfPlot)))
      cod            <- ifelse(input$fieldCodeRfPlot == "", rf.plot(), input$fieldCodeRfPlot)
    }, error = function(e) {
      output$plot_rf <- renderEcharts4r(NULL)
    })
  }
  
  plotear.rf.error <- function(){
    tryCatch({
      dataplot     <<- data.frame(x = c(1:length(modelo.rf$err.rate[,1])),cbind(modelo.rf$err.rate))
      output$plot_error_rf <- renderEcharts4r(e_rf_error(dataplot))
      cod                  <- ifelse(input$fieldCodeRfPlotError == "", plot.rf.error(),input$fieldCodeRfPlotError)
    }, error = function(e){
      limpia.rf(1)
    })
  }
  
  #Mostrar Reglas
  mostrar.reglas.rf <- function(n){
    idioma <- updateData$idioma
    output$rulesRf <- renderPrint({
      tryCatch({
        updateAceEditor(session,"fieldCodeRfRules",paste0("printRandomForests(modelo.rf, ",n,")"))
        rattle::printRandomForests(modelo.rf, n)
      },error = function(e){
        ifelse(idioma == "es",
               stop("No se pueden mostrar las reglas para el árbol seleccionado"), 
               stop("Cannot display rules for selected tree"))
        })
    })
  }
  
  # Limpia los datos segun el proceso donde se genera el error
  limpia.rf <- function(capa = NULL){
    for(i in capa:4){
      switch(i, {
        modelo.rf    <<- NULL
        output$txtRf <- renderPrint(invisible(""))
      }, {
        prediccion.rf       <<- NULL
        output$rfPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.rf             <<- NULL
        output$plot_rf_mc <- renderPlot(NULL)
        output$txtRfMC    <- renderPrint(invisible(NULL))
      }, {
        indices.rf            <<- rep(0, 10)
        IndicesM[["rfl"]]     <<- NULL
        output$rfIndPrecTable <- shiny::renderTable(NULL)
        output$rfIndErrTable  <- shiny::renderTable(NULL)
        output$rfPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$rfErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }
  # Limpia los datos 
  limpia.rf.run <- function(capa = NULL){
        output$txtRf          <- renderPrint(invisible(""))
        output$rfPrediTable   <- DT::renderDataTable(NULL)
        output$plot_rf_mc     <- renderPlot(NULL)
        output$txtRfMC        <- renderPrint(invisible(NULL))
        output$rfIndPrecTable <- shiny::renderTable(NULL)
        output$rfIndErrTable  <- shiny::renderTable(NULL)
        output$rfPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$rfErrorGlob    <-  flexdashboard::renderGauge(NULL)
  }
  
  limpiar <- function(){
    limpia.rf(1)
    limpia.rf(2)
    limpia.rf(3)
    limpia.rf(4)
  }
}
    
## To be copied in the UI
# mod_r_forest_ui("r_forest_ui_1")
    
## To be copied in the server
# callModule(mod_r_forest_server, "r_forest_ui_1")
 
