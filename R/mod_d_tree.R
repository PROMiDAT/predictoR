#' d_tree UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_d_tree_ui <- function(id){
  ns <- NS(id)
  opciones.dt <- list(options.run(ns("runDt")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(col_6(numericInput(ns("minsplit.dt"), labelInput("minsplit"), 2, width = "100%",min = 1)),
                               col_6(numericInput(ns("maxdepth.dt"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1))),
                      fluidRow(col_12(selectInput(inputId = ns("split.dt"), label = labelInput("splitIndex"),selected = 1,
                                                  choices =  list("gini" = "gini", "Entropía" = "information")))))
  
  codigo.dt <- list(conditionalPanel("input.BoxDt == 'tabDtModelo'",
                                     codigo.monokai(ns("fieldCodeDt"),height = "10vh")),
                    conditionalPanel("input.BoxDt == 'tabDtPlot'",
                                     codigo.monokai(ns("fieldCodeDtPlot"),height = "10vh")),
                    conditionalPanel("input.BoxDt == 'tabDtPred'",
                                     codigo.monokai(ns("fieldCodeDtPred"),height = "10vh")),
                    conditionalPanel("input.BoxDt == 'tabDtMC'",
                                     codigo.monokai(ns("fieldCodeDtMC"),height = "10vh")),
                    conditionalPanel("input.BoxDt == 'tabDtIndex'",
                                     codigo.monokai(ns("fieldCodeDtIG"),height = "10vh")),
                    conditionalPanel("input.BoxDt == 'tabDtReglas'",
                                     codigo.monokai(ns("fieldCodeDtRule"),height = "10vh")))
  
  opc_dt <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(60, 60),
                         tabs.content = list(opciones.dt, codigo.dt))
  tagList(
    tabBoxPrmdt(
      id = "BoxDt", opciones = opc_dt,
      tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
               withLoader(verbatimTextOutput(ns("txtDt")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
               withLoader(plotOutput(ns('plot_dt'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabDtPred",
               withLoader(DT::dataTableOutput(ns("dtPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabDtMC",
               withLoader(plotOutput(ns('plot_dt_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtDtMC"))),
      
      tabPanel(title = labelInput("indices"),value = "tabDtIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("dtPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("dtErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("dtIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("dtIndErrTable"))))),
      
      tabPanel(title = labelInput("reglas"),value = "tabDtReglas",
               withLoader(verbatimTextOutput(ns("rulesDt")), 
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' d_tree Server Function
#'
#' @noRd 
mod_d_tree_server <- function(input, output, session, updateData){
  ns <- session$ns
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.dt()
  })
  # 
  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     ejecutar.dt.mc()
  #     ejecutar.dt.ind()
  #   }
  # })


  # Cuando se genera el modelo knn
  observeEvent(input$runDt, {
    if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
      limpia.dt.run()
      default.codigo.dt()
      dt.full()
    }
  }, priority =  -5)
  
  # Ejecuta el modelo, prediccion, mc e indices de dt
  dt.full <- function() {
    ejecutar.dt()
    ejecutar.dt.pred()
    ejecutar.dt.mc()
    ejecutar.dt.ind()
  }
  
  # Genera el modelo
  ejecutar.dt <- function() {
    tryCatch({ 
      isolate(exe(cod.dt.modelo))
      
      tipo         <- isolate(input$split.dt)
      output$txtDt <- renderPrint(print(exe("modelo.dt.", tipo)))
      plotear.arbol()
      mostrar.reglas.dt()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.dt.", tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.dt(1)
      showNotification(paste0("Error (DT-01) : ",e), duration = 15, type = "error")
    })
  }
  
  
  # Genera la prediccion
  ejecutar.dt.pred <- function() {
    tryCatch({ 
      isolate(exe(cod.dt.pred))
      tipo   <- isolate(input$split.dt)
      idioma <- updateData$idioma
      pred   <- exe("predict(modelo.dt.",tipo,", datos.prueba, type = 'prob')")
      scores[[paste0("dtl-", tipo)]] <<- pred$prediction[,2]
      output$dtPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.dt.",tipo),idioma = idioma),server = FALSE)
      
      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.dt.",tipo))
      updateData$roc  <- !updateData$roc #graficar otra vez la curva roc
    },
    error = function(e) { 
      limpia.dt(2)
      showNotification(paste0("Error (DT-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Genera la matriz de confusion
  ejecutar.dt.mc <- function() {
    idioma <- updateData$idioma
    tipo   <- isolate(input$split.dt)
    if(exists(paste0("prediccion.dt.",tipo))){
      tryCatch({ 
        isolate(exe(cod.dt.mc))
        output$txtDtMC <- renderPrint(print(exe("MC.dt.",tipo)))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot_dt_mc <- renderPlot(isolate(exe("plot.MC(MC.dt.",tipo,")")))
        nombres.modelos   <<- c(nombres.modelos, paste0("MC.dt.",tipo))
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.dt(3)
        showNotification(paste0("Error (DT-03) : ", e), duration = 15, type = "error")
      })
    }
  }
  
  # Genera los indices
  ejecutar.dt.ind <- function() {
    idioma <- updateData$idioma
    tipo   <- isolate(input$split.dt)
    if(exists(paste0("MC.dt.",tipo))){
      tryCatch({ 
        isolate(exe(cod.dt.ind))
        indices.dt <<- indices.generales(exe("MC.dt.",tipo))
        eval(parse(text = paste0("indices.dt.",tipo, "<<- indices.dt")))
   
        output$dtPrecGlob  <-  fill.gauges(indices.dt[[1]], tr("precG",idioma))
        output$dtErrorGlob <-  fill.gauges(indices.dt[[2]], tr("errG",idioma))

        # Cambia la tabla con la indices de dt
        output$dtIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.dt,"Árboles de Decisión", idioma = idioma)), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$dtIndErrTable  <- shiny::renderTable(xtable(indices.error.table(indices.dt,"Árboles de Decisión")), spacing = "xs",
                                                   bordered = T, width = "100%", align = "c", digits = 2)
        
        IndicesM[[paste0("dtl-",tipo)]] <<- indices.dt
        updateData$selector.comparativa <- actualizar.selector.comparativa()
        
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.dt(4)
        showNotification(paste0("Error (DT-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Actualiza el codigo a la version por defecto
  default.codigo.dt <- function() {
    
    tipo   <- isolate(input$split.dt)
    codigo <- dt.modelo(variable.pr = updateData$variable.predecir,
                        minsplit = input$minsplit.dt,
                        maxdepth = input$maxdepth.dt,
                        split = tipo)
    
    updateAceEditor(session, "fieldCodeDt", value = codigo)
    cod.dt.modelo <<- codigo
    
    # Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot(tipo))
    
    # Se genera el codigo de la prediccion
    codigo <- dt.prediccion(tipo)
    updateAceEditor(session, "fieldCodeDtPred", value = codigo)
    cod.dt.pred <<- codigo
    
    # Se genera el codigo de la matriz
    codigo <- dt.MC(tipo)
    updateAceEditor(session, "fieldCodeDtMC", value = codigo)
    cod.dt.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeDtIG", value = codigo)
    cod.dt.ind <<- codigo
  }
  
  
  #Plotear el arbol
  plotear.arbol <- function(){
    tryCatch({
      tipo   <- isolate(input$split.dt)
      output$plot_dt <- renderPlot(isolate(exe(input$fieldCodeDtPlot)))
      cod    <- ifelse(input$fieldCodeDtPlot == "", dt.plot(tipo), input$fieldCodeDtPlot)
    },
    error = function(e){
      output$plot_dt <- renderPlot(NULL)
    })
  }
  
  #Mostrar Reglas
  mostrar.reglas.dt <- function(){
    tipo           <- isolate(input$split.dt)
    output$rulesDt <- renderPrint(rattle::asRules(exe("modelo.dt.",tipo)))
    updateAceEditor(session, "fieldCodeDtRule", paste0("asRules(modelo.dt.",tipo,")"))
    
  }
  
  # Limpia los datos segun el proceso donde se genera el error
  limpia.dt <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.dt      <<- NULL
        output$txtDt   <- renderPrint(invisible(""))
        output$plot_dt <- renderPlot(NULL)
      }, {
        prediccion.dt       <<- NULL
        output$dtPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.dt             <<- NULL
        output$plot_dt_mc <- renderPlot(NULL)
        output$txtDtMC    <- renderPrint(invisible(NULL))
      }, {
        indices.dt <<- rep(0, 10)
        IndicesM[[paste0("dtl-",input$split.dt)]] <<- NULL
        output$dtIndPrecTable <- shiny::renderTable(NULL)
        output$dtIndErrTable  <- shiny::renderTable(NULL)
        output$dtPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$dtErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }  
  
  # Limpia los outputs 
  limpia.dt.run <- function(capa = NULL) {
        output$txtDt          <- renderPrint(invisible(""))
        output$plot_dt        <- renderPlot(NULL)
        output$dtPrediTable   <- DT::renderDataTable(NULL)
        output$plot_dt_mc     <- renderPlot(NULL)
        output$txtDtMC        <- renderPrint(invisible(NULL))
        output$dtIndPrecTable <- shiny::renderTable(NULL)
        output$dtIndErrTable  <- shiny::renderTable(NULL)
        output$dtPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$dtErrorGlob    <-  flexdashboard::renderGauge(NULL)
  }
  
  limpiar <- function(){
    limpia.dt(1)
    limpia.dt(2)
    limpia.dt(3)
    limpia.dt(4)
  }
}
    
## To be copied in the UI
# mod_d_tree_ui("d_tree_ui_1")
    
## To be copied in the server
# callModule(mod_d_tree_server, "d_tree_ui_1")
 
