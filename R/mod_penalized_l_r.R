#' penalized_l_r UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penalized_l_r_ui <- function(id){
  ns <- NS(id)
  opciones.rlr <- list(options.run(ns("runRlr")), tags$hr(style = "margin-top: 0px;"),
                       fluidRow(col_6(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"),selected = 1,
                                                   choices = list("Ridge" = 0, "Lasso" = 1))),
                                col_6(br(), switchInput(inputId = ns("switch.scale.rlr"), onStatus = "success", offStatus = "danger", value = T,
                                                         label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"))),
                       fluidRow(col_6(id = ns("colManualLanda"),numericInput(ns("landa"), labelInput("landa"),value = 2, min = 0, "NULL", width = "100%")), br(),
                                col_6(switchInput(inputId = ns("permitir.landa"), onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                              label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"))))
  
  codigo.rlr  <- list(conditionalPanel("input.BoxRlr == 'tabRlrModelo'",
                                       codigo.monokai(ns("fieldCodeRlr"), height = "10vh")),
                      conditionalPanel("input.BoxRlr == 'tabRlrLanda'",
                                       codigo.monokai(ns("fieldCodeRlrLanda"), height = "10vh")),
                      conditionalPanel("input.BoxRlr == 'tabRlrPosibLanda'",
                                       codigo.monokai(ns("fieldCodeRlrPosibLanda"), height = "10vh")),
                      conditionalPanel("input.BoxRlr == 'tabRlrPred'",
                                       codigo.monokai(ns("fieldCodeRlrPred"), height = "10vh")),
                      conditionalPanel("input.BoxRlr == 'tabRlrMC'",
                                       codigo.monokai(ns("fieldCodeRlrMC"), height = "10vh")),
                      conditionalPanel("input.BoxRlr == 'tabRlrIndex'",
                                       codigo.monokai(ns("fieldCodeRlrIG"), height = "10vh")))
  
  opc_rlr  <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                           tabs.content = list(opciones.rlr, codigo.rlr))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxRlr", opciones = opc_rlr,
      tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
               verbatimTextOutput(ns("txtRlr"))),
      
      tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
               plotOutput(ns('plot.rlr.posiblanda'), height = "55vh")),
      
      tabPanel(title = labelInput("gcoeff"),value = "tabRlrLanda",
               plotOutput(ns('plot.rlr.landa'), height = "55vh")),
      
      tabPanel(title = labelInput("predm"), value = "tabRlrPred",
               DT::dataTableOutput(ns("rlrPrediTable"))),
      
      tabPanel(title = labelInput("mc"), value = "tabRlrMC",
               plotOutput(ns('plot.rlr.mc'), height = "45vh"),
               verbatimTextOutput(ns("txtrlrMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabRlrIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("rlrPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("rlrErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("rlrIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("rlrIndErrTable")))))
    )
  )
}
    
#' penalized_l_r Server Function
#'
#' @noRd 
mod_penalized_l_r_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  # observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
  #   limpiar()
  #   default.codigo.rlr()
  # })
  # # 
  # # observeEvent(updateData$idioma, {
  # #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  # #              ejecutar.rlr.mc()
  #               ejecutar.rlr.ind()
  # #   }
  # # })
  # 
  # observeEvent(input$runRlr, {
  #   if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
  #     default.codigo.rlr()
  #     rlr.full()
  #   }
  # }, priority =  -5)
  # 

  # Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.landa, {
    if (input$permitir.landa) {
      shinyjs::enable("landa")
    } else {
      shinyjs::disable("landa")
    }
  })

  get_landa_rlr <- function(){
    landa <- NULL
    if (!is.na(input$landa) && input$permitir.landa) {
      if (input$landa > 0) {
        landa <- input$landa
      }
    }
    return(landa)
  }
  

  
  # Ejecuta el modelo, prediccion, mc e indices de rlr
  rlr.full <- function(){
    ejecutar.rlr()
    ejecutar.rlr.pred()
    ejecutar.rlr.mc()
    ejecutar.rlr.ind()
  }
  
  # Genera el modelo
  ejecutar.rlr <- function() {
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rlr.type())
      isolate(exe(cod.rlr.modelo))
      
      output$txtRlr <- renderPrint(print(exe("modelo.rlr.",tipo)))
  
      #plot.posib.landa.rlr()
      #plot.coeff()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.rlr.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rlr(1)
      showNotification(paste0("Error (R/L-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Genera la prediccion
  ejecutar.rlr.pred <- function() {
    tryCatch({ # Se corren los codigo
      exe(cod.rlr.pred)
      idioma <- updateData$idioma
      landa <- get_landa_rlr()
      tipo <- rlr.type()
      landa <- ifelse(is.null(landa),paste0("cv.glm.",tipo,"$lambda.min"), landa)
      
      pred   <- predict(exe("modelo.rlr.",tipo), datos.prueba, type = "prob")
      scores[[paste0("rlr-",tipo)]] <<- pred$prediction[,2]
      # Cambia la tabla con la prediccion de rlr
      output$rlrPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.rlr.",tipo),idioma = idioma), server = FALSE)
      
      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.rlr.",tipo))
      updateData$roc <- !updateData$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Genera la matriz de confusion
  ejecutar.rlr.mc <- function() {
    tipo <- rlr.type()
    idioma <- updateData$idioma
    
    if(exists(paste0("prediccion.rlr.",tipo))){
      tryCatch({ # Se corren los codigo
        exe(cod.rlr.mc)
        output$txtrlrMC <- renderPrint(print(exe("MC.rlr.",tipo)))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot.rlr.mc <- renderPlot(exe("plot.MC(MC.rlr.",tipo,")"))
        
        nombres.modelos <<- c(nombres.modelos, paste0("MC.rlr.",tipo))
      },
      error = function(e){ # Regresamos al estado inicial y mostramos un error
        limpia.rlr(3)
        showNotification(paste0("Error (RLR-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Genera los indices
  ejecutar.rlr.ind <- function() {
    idioma <- updateData$idioma
    tipo <- rlr.type()
    if(exists(paste0("prediccion.rlr.",tipo))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rlr.ind))
        indices.rlr <- exe("indices.generales(MC.rlr.",tipo,")")
        eval(parse(text = paste0("indices.rlr.",tipo, "<<- indices.rlr")))
        
        output$rlrPrecGlob  <-  fill.gauges(indices.rlr[[1]], tr("precG",idioma))
        output$rlrErrorGlob <-  fill.gauges(indices.rlr[[2]], tr("errG",idioma))
        
        # Cambia la tabla con la indices de rl
        output$rlrIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.rlr,"RLR", idioma = idioma)), spacing = "xs",
                                                     bordered = T, width = "100%", align = "c", digits = 2)
        output$rlrIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.rlr,"RLR")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        
        nombres.modelos <<- c(nombres.modelos, paste0("indices.rlr.",tipo))
        IndicesM[[paste0("rlr-",tipo)]] <<- indices.rlr
        updateData$selector.comparativa <- actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rlr(4)
        showNotification(paste0("Error (R/L-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  # Acualiza el codigo a la version por defecto
  default.codigo.rlr <- function(){
    landa <- get_landa_rlr()
    tipo  <- rlr.type()
    # Se acualiza el codigo del modelo
    codigo <- rlr.modelo(variable.pr = variable.predecir,
                         tipo,
                         input$alpha.rlr,
                         input$switch.scale.rlr)
    
    updateAceEditor(session, "fieldCodeRlr", value = codigo)
    cod.rlr.modelo <<- codigo
    
    # Se genera el codigo del posible landa
    codigo <- select.landa(variable.predecir,
                           input$alpha.rlr,
                           input$switch.scale.rlr)
    
    updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)
    cod.select.landa <<- codigo
    
    #Se genera el codigo de los coeficientes con el mejor landa
    codigo <- plot.coeff.landa(landa)
    updateAceEditor(session, "fieldCodeRlrLanda", value = codigo)
    
    # Se genera el codigo de la prediccion
    codigo <- rlr.prediccion(tipo)
    updateAceEditor(session, "fieldCodeRlrPred", value = codigo)
    cod.rlr.pred <<- codigo
    
    # Se genera el codigo de la matriz
    codigo <- rlr.MC(tipo)
    updateAceEditor(session, "fieldCodeRlrMC", value = codigo)
    cod.rlr.mc <<- codigo
    
    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlrIG", value = codigo)
    cod.rlr.ind <<- codigo
  }
  
  plot.posib.landa.rlr <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.select.landa))
      isolate(tipo <- rlr.type())
      output$plot.rlr.posiblanda <- renderPlot(exe("plot(cv.glm.",tipo,")"))
     },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  plot.coeff <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rlr.type())
      isolate(codigo <- input$fieldCodeRlrLanda)
      output$plot.rlr.landa <- renderPlot(isolate(exe(codigo)))
    },
    error = function(e){ # Regresamos al estado inicial y mostramos un error
      limpia.rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  rlr.type <- function(){
    ifelse(input$alpha.rlr == 0, "ridge", "lasso")
  }
  
  # Limpia los datos segun el proceso donde se genera el error
  limpia.rlr <- function(capa = NULL){
    tipo <- rlr.type()
    for(i in capa:4){
      switch(i, {
        modelo.rlr <<- NULL
        output$txtRlr <- renderPrint(invisible(""))
      }, {
        prediccion.rlr <<- NULL
        output$rlrPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.rlr.",tipo," <<- NULL")
        output$plot.rlr.mc <- renderPlot(NULL)
        output$txtrlrMC <- renderPrint(invisible(NULL))
      },{
        indices.rlr <<- rep(0, 10)
        output$rlrIndPrecTable <- shiny::renderTable(NULL)
        output$rlrIndErrTable  <- shiny::renderTable(NULL)
        output$rlrPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$rlrErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }
  limpiar <- function(){
    limpia.rlr(1)
    limpia.rlr(2)
    limpia.rlr(3)
    limpia.rlr(4)
  }
}
    
## To be copied in the UI
# mod_penalized_l_r_ui("penalized_l_r_ui_1")
    
## To be copied in the server
# callModule(mod_penalized_l_r_server, "penalized_l_r_ui_1", updateData)
 
