#' xgboosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_xgboosting_ui <- function(id){
  ns <- NS(id)
  opciones.xgb <- list(options.run(ns("runXgb")), tags$hr(style = "margin-top: 0px;"),
                       fluidRow(col_12(selectInput(inputId = ns("boosterXgb"), label = labelInput("selbooster"),selected = 1,
                                                   choices = c("gbtree", "gblinear", "dart")))),
                       fluidRow(col_6(numericInput(ns("maxdepthXgb"), labelInput("maxdepth"), min = 1,step = 1, value = 6)),
                                col_6(numericInput(ns("nroundsXgb"), labelInput("selnrounds"), min = 0,step = 1, value = 50))))
  
  codigo.xgb <- list(conditionalPanel("input.BoxXgb == 'tabXgbModelo'",
                                      codigo.monokai(ns("fieldCodeXgb"), height = "10vh")),
                     conditionalPanel("input.BoxXgb == 'tabXgbImp'",
                                      codigo.monokai(ns("fieldCodeXgbImp"), height = "10vh")),
                     conditionalPanel("input.BoxXgb == 'tabXgbPred'",
                                      codigo.monokai(ns("fieldCodeXgbPred"), height = "10vh")),
                     conditionalPanel("input.BoxXgb == 'tabXgbMC'",
                                      codigo.monokai(ns("fieldCodeXgbMC"), height = "10vh")),
                     conditionalPanel("input.BoxXgb == 'tabXgbIndex'",
                                      codigo.monokai(ns("fieldCodeXgbIG"), height = "10vh")))
  
  opc_xgb <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(55,100), heights = c(70, 95),
                          tabs.content = list(opciones.xgb, codigo.xgb))
  tagList(
    tabBoxPrmdt(
      id = "BoxXgb", opciones = opc_xgb,
      tabPanel(title = labelInput("generatem"), value = "tabXgbModelo",
               withLoader(verbatimTextOutput(ns("txtxgb")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("varImp"), value = "tabXgbImp",
               withLoader(echarts4rOutput(ns('plot_xgb'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabXgbPred",
               withLoader(DT::dataTableOutput(ns("xgbPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabXgbMC",
               withLoader(plotOutput(ns('plot_xgb_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtxgbMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabXgbIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("xgbPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("xgbErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("xgbIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("xgbIndErrTable")))))
    )
  )
}
    
#' xgboosting Server Function
#'
#' @noRd 
mod_xgboosting_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.xgb()
  })
  
  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     ejecutar.xgb.mc()
  #     ejecutar.xgb.ind()
  #   }
  # })
  
  # Ejecuta el modelo, predicción, mc e indices de XGBoosting
  xgb.full <- function() {
     ejecutar.xgb()
     ejecutar.xgb.pred()
     ejecutar.xgb.mc()
     ejecutar.xgb.ind()
  }

  #Cuando se ejecuta el botón run
  observeEvent(input$runXgb, {
    if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { 
      # Si se tiene los datos entonces :
      limpia.xgb.run()
      default.codigo.xgb()
      xgb.full()
    }
  }, priority =  -5)
  
  #Genera el modelo
  ejecutar.xgb <- function(){
    tryCatch({
      tipo <- input$boosterXgb
      exe(cod.xgb.modelo)
      output$txtxgb <- renderPrint(exe("modelo.xgb.",tipo))
      
      plotear.xgb.imp()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.xgb.",tipo))
    },
    error = function(e) { 
      limpia.xgb(1)
      showNotification(paste0("Error (XGB-01) : ", e), duration = 15, type = "error")
    })
  }
  
  #Genera la predicción
  ejecutar.xgb.pred <- function(){
    tryCatch({ 
      exe(cod.xgb.pred)
      tipo   <- input$boosterXgb
      idioma <- updateData$idioma
      pred   <- predict(exe("modelo.xgb.",tipo), datos.prueba, type = "prob")
      scores[[paste0("xgb-",tipo)]] <<-pred$prediction[,2] 

      # Cambia la tabla con la predicción de xgb
      output$xgbPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.xgb.",tipo),idioma = idioma),server = FALSE)

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.xgb.",tipo))

      #Gráfica otra vez la curva roc
      updateData$roc  <- !updateData$roc
    },
    error = function(e) { 
      limpia.xgb(2)
      showNotification(paste0("Error (XGB-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Genera la matriz de confusión
  ejecutar.xgb.mc <- function() {
    idioma <- updateData$idioma
    tipo   <- isolate(input$boosterXgb)
    if(exists(paste0("prediccion.xgb.",tipo))){
      tryCatch({ 
        exe(cod.xgb.mc)
        output$txtxgbMC <- renderPrint(print(exe("MC.xgb.",tipo)))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot_xgb_mc <- renderPlot(exe("plot.MC(MC.xgb.",tipo,")"))
        
        nombres.modelos    <<- c(nombres.modelos, paste0("MC.xgb.",tipo))
      },
      error = function(e){ 
        # Regresamos al estado inicial y mostramos un error
        limpia.xgb(3)
        showNotification(paste0("Error (XGB-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Genera los indices
  ejecutar.xgb.ind <- function(){
    idioma <- updateData$idioma
    tipo   <- isolate(input$boosterXgb)
    
    if(exists(paste0("MC.xgb.",tipo))){
      tryCatch({ 
        # Se corren los códigos
        isolate(exe(cod.xgb.ind))
        indices.xgb <- indices.generales(exe("MC.xgb.",tipo))
        eval(parse(text = paste0("indices.xgb.",tipo, "<<- indices.xgb")))
        
        output$xgbPrecGlob  <-  fill.gauges(indices.xgb[[1]], tr("precG",idioma))
        output$xgbErrorGlob <-  fill.gauges(indices.xgb[[2]], tr("errG",idioma))
        
        # Cambia la tabla con los indices de xgb
        output$xgbIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.xgb,"XGB", idioma = idioma)), spacing = "xs",
                                                     bordered = T, width = "100%", align = "c", digits = 2)
        
        output$xgbIndErrTable  <- shiny::renderTable(xtable(indices.error.table(indices.xgb,"XGB")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        
        nombres.modelos <<- c(nombres.modelos, paste0("indices.xgb.",tipo))
        IndicesM[[paste0("xgb-",tipo)]] <<- indices.xgb
        updateData$selector.comparativa <- actualizar.selector.comparativa()
      },
      error = function(e) { 
        # Regresamos al estado inicial y mostramos un error
        limpia.xgb(4)
        showNotification(paste0("Error (XGB-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  default.codigo.xgb <- function() {
    tipo   <- input$boosterXgb

    #Modelo
    codigo <- xgb.modelo(updateData$variable.predecir,
                              booster = tipo,
                              max.depth = input$maxdepthXgb,
                              n.rounds = input$nroundsXgb)
    updateAceEditor(session, "fieldCodeXgb", value = codigo)
    cod.xgb.modelo <<- codigo
    
    #Código de importancia de variables
    updateAceEditor(session, "fieldCodeXgbImp", value = e_xgb_varImp(booster = tipo))
    
    #Predicción 
    codigo <- xgb.prediccion(booster = tipo)
    updateAceEditor(session, "fieldCodeXgbPred", value = codigo)
    cod.xgb.pred <<- codigo

    #Matriz de confusión
    codigo <- xgb.MC(booster = tipo)
    updateAceEditor(session, "fieldCodeXgbMC", value = codigo)
    cod.xgb.mc <<- codigo

    #Indices Generales
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeXgbIG", value = codigo)
    cod.xgb.ind <<- codigo
  }
  
  # Gráfico de importancia
  plotear.xgb.imp <- function() {
    tryCatch({
      tipo   <- input$boosterXgb
      codigo <- input$fieldCodeXgbImp
      output$plot_xgb <- renderEcharts4r(exe(codigo))
      cod    <- ifelse(codigo == "", e_xgb_varImp(booster = tipo), codigo)
    }, error = function(e) {
      output$plot_xgb <- renderEcharts4r(NULL)
    })
  }
  
  # Limpia los datos según el proceso donde se genera el error
  limpia.xgb <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        exe("modelo.xgb.",input$boosterXgb," <<- NULL")
        output$txtxgb <- renderPrint(invisible(""))
      }, {
        exe("prediccion.xgb.",input$boosterXgb," <<- NULL")
        output$xgbPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.xgb.",input$boosterXgb," <<- NULL")
        output$plot_xgb_mc <- renderPlot(NULL)
        output$txtxgbMC    <- renderPrint(invisible(NULL))
      }, {
        exe("indices.xgb.",input$boosterXgb," <<- NULL")
        IndicesM[[paste0("xgb-",input$boosterXgb)]] <<- NULL
        output$xgbIndPrecTable <- shiny::renderTable(NULL)
        output$xgbIndErrTable  <- shiny::renderTable(NULL)
        output$xgbPrecGlob     <- flexdashboard::renderGauge(NULL)
        output$xgbErrorGlob    <- flexdashboard::renderGauge(NULL)
      })
    }
  }
  
  # Limpia los datos al ejecutar el botón run
  limpia.xgb.run <- function() {
        output$txtxgb          <- renderPrint(invisible(""))
        output$xgbPrediTable   <- DT::renderDataTable(NULL)
        output$plot_xgb_mc     <- renderPlot(NULL)
        output$txtxgbMC        <- renderPrint(invisible(NULL))
        output$xgbIndPrecTable <- shiny::renderTable(NULL)
        output$xgbIndErrTable  <- shiny::renderTable(NULL)
        output$xgbPrecGlob     <- flexdashboard::renderGauge(NULL)
        output$xgbErrorGlob    <- flexdashboard::renderGauge(NULL)
  }
  
  # Limpia todos los datos
  limpiar <- function(){
        limpia.xgb(1)
        limpia.xgb(2)
        limpia.xgb(3)
        limpia.xgb(4)
  }
}
    
## To be copied in the UI
# mod_xgboosting_ui("xgboosting_ui_1")
    
## To be copied in the server
# callModule(mod_xgboosting_server, "xgboosting_ui_1")
 
