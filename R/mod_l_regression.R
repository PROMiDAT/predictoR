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
  codigo.rl <- list(tags$div(options.run(ns("runRl"))), tags$hr(style = "margin-top: 0px;"),
                    conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlModelo'",
                                     codigo.monokai(ns("fieldCodeRl"), height = "10vh")),
                    conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlPred'",
                                     codigo.monokai(ns("fieldCodeRlPred"), height = "10vh")),
                    conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlMC'",
                                     codigo.monokai(ns("fieldCodeRlMC"), height = "10vh")),
                    conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlIndex'",
                                     codigo.monokai(ns("fieldCodeRlIG"), height = "10vh")))
  
  opc_rl <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(95),
                         tabs.content = list(codigo.rl))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxRl"),opciones = opc_rl,
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
mod_l_regression_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    updateTabsetPanel(session, "BoxRl",selected = "tabRlModelo")
    default.codigo.rl()
  })
# 
#   #Cuando se ejecuta el botón run
#   observeEvent(input$runRl, {
#     idioma <- updateData$idioma
#     if (length(levels(datos[, variable.predecir])) == 2) {
#       if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
#         # limpia.rl.run()
#         # rl.full()
#       }
#     }else{
#       if (isFALSE(getOption("shiny.testmode")) || is.null(getOption("shiny.testmode"))) {
#         showModal(modalDialog(
#           title = "Regresión Logística", tr("limitModel", idioma),
#           footer = modalButton("Cerrar"), easyClose = T
#         ))
#       }
#     }
#   }, priority =  -5)

  output$txtrl <- renderPrint({
    input$runRl
    default.codigo.rl()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("modelo.rl")
    modelo <- traineR::train.glmnet(as.formula(var), data = train)
    pred   <- predict(modelo , test, type = 'class')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$rl[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)
  })
  
  output$rlPrediTable <- DT::renderDataTable({
    idioma <- updateData$idioma
    obj.predic(modelos$rl[[nombre.modelo$x]]$pred,idioma = idioma)
    
  },server = FALSE)
  
  output$txtrlMC    <- renderPrint({
    print(modelos$rl[[nombre.modelo$x]]$mc)
  })
  
  output$plot_rl_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$rl[[nombre.modelo$x]]$mc)
  })
  
  output$rlIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.rl <- indices.generales(modelos$rl[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.rl,"rl", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  output$rlIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.rl <- indices.generales(modelos$rl[[nombre.modelo$x]]$mc)
    output$rlPrecGlob  <-  fill.gauges(indices.rl[[1]], tr("precG",idioma))
    output$rlErrorGlob <-  fill.gauges(indices.rl[[2]], tr("errG",idioma))
    xtable(indices.error.table(indices.rl,"rl"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Actualiza el código a la versión por defecto
  default.codigo.rl <- function() {
    # Se actualiza el código del modelo
    codigo <- rl.modelo(updateData$variable.predecir)
    updateAceEditor(session, "fieldCodeRl", value = codigo)

    # Se genera el código de la prediccion
    codigo <- rl.prediccion()
    updateAceEditor(session, "fieldCodeRlPred", value = codigo)

    # Se genera el código de la matriz
    codigo <- rl.MC()
    updateAceEditor(session, "fieldCodeRlMC", value = codigo)

    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlIG", value = codigo)
  }
  
  # Limpia los outputs
  limpia.rl <- function() {
        output$txtrl          <- renderPrint(invisible(""))
        output$rlPrediTable   <- DT::renderDataTable(NULL)
        output$plot_rl_mc     <- renderPlot(NULL)
        output$txtrlMC        <- renderPrint(invisible(NULL))
        output$rlIndPrecTable <- shiny::renderTable(NULL)
        output$rlIndErrTable  <- shiny::renderTable(NULL)
        output$rlPrecGlob     <- flexdashboard::renderGauge(NULL)
        output$rlErrorGlob    <- flexdashboard::renderGauge(NULL)
  }
  

}
    
## To be copied in the UI
# mod_l_regression_ui("l_regression_ui_1")
    
## To be copied in the server
# callModule(mod_l_regression_server, "l_regression_ui_1", updateData)
 
