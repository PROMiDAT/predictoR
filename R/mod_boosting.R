#' boosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boosting_ui <- function(id){
  ns <- NS(id)
  #HAY QUE QUITAR OPCION DE ALGORITMO 
  #PORQUE YA NO SE VA A USAR CON LA NUEVA VERSION DE TRAINER 
  opciones.b <- list(options.run(ns("runBoosting")), tags$hr(style = "margin-top: 0px;"),
                     conditionalPanel("input.BoxB != 'tabBRules'",
                                      fluidRow(col_6(numericInput(ns("iter.boosting"), labelInput("numTree"), 50, width = "100%",min = 1)),
                                               col_6(numericInput(ns("maxdepth.boosting"), labelInput("maxdepth"), 15, width = "100%",min = 1))),
                                      
                                      fluidRow(col_6(numericInput(ns("minsplit.boosting"), labelInput("minsplit"), 20, width = "100%",min = 1)))),
                     
                     conditionalPanel("input.BoxB == 'tabBRules'",
                                      numericInput(ns("rules.b.n"),labelInput("ruleNumTree"),1, width = "100%", min = 1)))
  
  codigo.b  <- list(conditionalPanel("input.BoxB == 'tabBModelo'",
                                     codigo.monokai(ns("fieldCodeBoosting"),     height = "10vh")),
                    conditionalPanel("input.BoxB == 'tabBError'",
                                     codigo.monokai(ns("fieldCodeBoostingPlot"), height = "10vh")),
                    conditionalPanel("input.BoxB == 'tabBImp'",
                                     codigo.monokai(ns("fieldCodeBoostingPlotImport"), height = "10vh")),
                    conditionalPanel("input.BoxB == 'tabBPred'",
                                     codigo.monokai(ns("fieldCodeBoostingPred"), height = "10vh")),
                    conditionalPanel("input.BoxB == 'tabBMC'",
                                     codigo.monokai(ns("fieldCodeBoostingMC"),   height = "10vh")),
                    conditionalPanel("input.BoxB == 'tabBIndex'",
                                     codigo.monokai(ns("fieldCodeBoostingIG"),   height = "10vh")),
                    conditionalPanel("input.BoxB == 'tabBRules'",
                                     codigo.monokai(ns("fieldCodeBoostingRules"),height = "10vh")))
  
  opc_b  <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(63, 95),
                         tabs.content = list(opciones.b, codigo.b))
  tagList(
    
    tabBoxPrmdt(
      id = "BoxB", opciones = opc_b,
      tabPanel(title = labelInput("generatem"), value = "tabBModelo",
               verbatimTextOutput(ns("txtBoosting"))),
      
      tabPanel(title = labelInput("evolerror"), value = "tabBError",
               plotOutput(ns('plot.boosting'), height = "55vh")),
      
      tabPanel(title = labelInput("varImp"), value = "tabBImp",
               plotOutput(ns('plot.boosting.import'), height = "55vh")),
      
      tabPanel(title = labelInput("predm"), value = "tabBPred",
               DT::dataTableOutput(ns("boostingPrediTable"))),
      
      tabPanel(title = labelInput("mc"), value = "tabBMC",
               plotOutput(ns('plot.boosting.mc'), height = "45vh"),
               verbatimTextOutput(ns("txtBoostingMC"))),
      
      tabPanel(title = labelInput("indices"),value = "tabBIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("boostingPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("boostingErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("boostingIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("boostingIndErrTable"))))),
      
      tabPanel(title = labelInput("reglas"), value = "tabBRules",
               verbatimTextOutput(ns("rulesB")))
    )
  )
}
    
#' boosting Server Function
#'
#' @noRd 
mod_boosting_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.boosting()
  })

  # # observeEvent(updateData$idioma, {
  # #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  # #     default.codigo.boosting()
  # #     boosting.full(TRUE)
  # #   }
  # # })
  # 
  # # Cuando se genera el modelo boosting
  # observeEvent(input$runBoosting, {
  #   if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
  #     default.codigo.boosting()
  #     boosting.full()
  #   }
  # }, priority =  -5)
  # 
  # observeEvent(input$rules.b.n,{
  #   if(validar.datos(print = FALSE)){
  #     mostrar.reglas.boosting(input$rules.b.n)
  #   }
  # }, priority =  5)
  # 
  
  # Ejecuta el modelo, prediccion, mc e indices de knn
  boosting.full <- function() {
      ejecutar.boosting()
      ejecutar.boosting.pred()
      ejecutar.boosting.mc()
      ejecutar.boosting.ind()
    
  }
  

  # Genera el modelo
  ejecutar.boosting <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.b.modelo))
      output$txtBoosting <- renderPrint(exe("print(modelo.boosting)"))
      plotear.boosting()
      plotear.boosting.imp()
      mostrar.reglas.boosting(input$rules.b.n)
 
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(1)
      showNotification(paste0("Error (B-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Genera la prediccion
  ejecutar.boosting.pred <- function(){
    tryCatch({ # Se corren los codigo
      idioma <- updateData$idioma
      
      isolate(exe(cod.b.pred))
      pred <- predict(modelo.boosting, datos.prueba, type = "prob")
      scores[[paste0("bl")]] <<- pred$prediction[,2]
      
      # Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.boosting"),idioma = idioma),server = FALSE)
 
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting"))
      updateData$roc <- !updateData$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(2)
      showNotification(paste0("Error (B-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Genera la matriz de confusion
  ejecutar.boosting.mc <- function() {
    if(exists(paste0("prediccion.boosting"))){
      idioma <- updateData$idioma
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.mc))
        output$txtBoostingMC <- renderPrint(exe("print(MC.boosting)"))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot.boosting.mc <- renderPlot(exe("plot.MC(MC.boosting)"))

        
        nombres.modelos <<- c(nombres.modelos, paste0("MC.boosting"))
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.boosting(3)
        showNotification(paste0("Error (B-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Genera los indices
  ejecutar.boosting.ind <- function() {
    if(exists(paste0("MC.boosting"))){
      tryCatch({ # Se corren los codigo
        idioma <- updateData$idioma
        
        isolate(exe(cod.b.ind))
        
        indices.boosting <<- indices.generales(exe("MC.boosting"))

        output$boostingPrecGlob  <-  fill.gauges(indices.boosting[[1]], tr("precG",idioma))
        output$boostingErrorGlob <-  fill.gauges(indices.boosting[[2]], tr("errG",idioma))
        

        # Cambia la tabla con la indices de boosting
        output$boostingIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.boosting,"ADA-BOOSTING", idioma = idioma)), spacing = "xs",
                                                          bordered = T, width = "100%", align = "c", digits = 2)
        
        output$boostingIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.boosting,"ADA-BOOSTING")), spacing = "xs",
                                                         bordered = T, width = "100%", align = "c", digits = 2)
        
        nombres.modelos <<- c(nombres.modelos, paste0("indices.boosting"))
        IndicesM[[paste0("bl")]] <<- indices.boosting
        updateData$selector.comparativa <- actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.boosting(4)
        showNotification(paste0("Error (B-04) : ", e), duration = 15, type = "error")
      })
    }
  }
  # Acualiza el codigo a la version por defecto
  default.codigo.boosting <- function() {

    # Se acualiza el codigo del modelo
    codigo <- boosting.modelo(variable.pr = updateData$variable.predecir,
                              iter = input$iter.boosting,
                              maxdepth = input$maxdepth.boosting,
                              minsplit = input$minsplit.boosting)
    
    updateAceEditor(session, "fieldCodeBoosting", value = codigo)
    cod.b.modelo <<- codigo
    
    # Se genera el codigo de la prediccion
    codigo <- boosting.prediccion()
    updateAceEditor(session, "fieldCodeBoostingPred", value = codigo)
    cod.b.pred <<- codigo
    
    # # Cambia el codigo del grafico del modelo
    # updateAceEditor(session, "fieldCodeBoostingPlot", value = boosting.plot())
    # 
    # # Cambia el codigo del grafico de importancia
    # updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting.plot.import())
    # 
    # Se genera el codigo de la matriz
    codigo <- boosting.MC()
    updateAceEditor(session, "fieldCodeBoostingMC", value = codigo)
    cod.b.mc <<- codigo
    
    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeBoostingIG", value = codigo)
    cod.b.ind <<- codigo
  }
  
  # Limpia los datos segun el proceso donde se genera el error
  limpia.boosting <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        exe("modelo.boosting <<- NULL")
        output$txtBoosting <- renderPrint(invisible(""))
        output$plot.boosting <- renderPlot(NULL)
        output$plot.boosting.import <- renderPlot(NULL)
      }, {
        exe("prediccion.boosting <<- NULL")
        output$boostingPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.boosting <<- NULL")
        output$plot.boosting.mc <- renderPlot(NULL)
        output$txtBoostingMC <- renderPrint(invisible(NULL))
      }, {
        exe("indices.boosting <<- NULL")
        IndicesM[[paste0("bl")]]    <<- NULL
        output$boostingIndPrecTable <- shiny::renderTable(NULL)
        output$boostingIndErrTable  <- shiny::renderTable(NULL)
        output$boostingPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$boostingErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }
  # Limpia los datos segun el proceso donde se genera el error
  limpia.boosting.run <- function() {
        output$txtBoosting <- renderPrint(invisible(""))
        output$plot.boosting <- renderPlot(NULL)
        output$plot.boosting.import <- renderPlot(NULL)
        output$boostingPrediTable <- DT::renderDataTable(NULL)
        output$plot.boosting.mc <- renderPlot(NULL)
        output$txtBoostingMC <- renderPrint(invisible(NULL))
        output$boostingIndPrecTable <- shiny::renderTable(NULL)
        output$boostingIndErrTable  <- shiny::renderTable(NULL)
        output$boostingPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$boostingErrorGlob    <-  flexdashboard::renderGauge(NULL)
  }
  
  
  limpiar <- function(){
    limpia.boosting(1)
    limpia.boosting(2)
    limpia.boosting(3)
    limpia.boosting(4)
  }
  
}
    
## To be copied in the UI
# mod_boosting_ui("boosting_ui_1")
    
## To be copied in the server
# callModule(mod_boosting_server, "boosting_ui_1")
 
