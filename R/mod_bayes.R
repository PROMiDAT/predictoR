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
  codigo.bayes <- list(tags$div(options.run(ns("runBayes"))), tags$hr(style = "display:none;",),
                       conditionalPanel("input['bayes_ui_1-BoxBayes'] == 'tabBayesModelo'",
                                        codigo.monokai(ns("fieldCodeBayes"), height = "10vh")),
                       conditionalPanel("input['bayes_ui_1-BoxBayes'] == 'tabBayesPred'",
                                        codigo.monokai(ns("fieldCodeBayesPred"), height = "10vh")),
                       conditionalPanel("input['bayes_ui_1-BoxBayes'] == 'tabBayesMC'",
                                        codigo.monokai(ns("fieldCodeBayesMC"), height = "10vh")),
                       conditionalPanel("input['bayes_ui_1-BoxBayes'] == 'tabBayesIndex'",
                                        codigo.monokai(ns("fieldCodeBayesIG"), height = "10vh")))
  
  opc_bayes <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(95),
                            tabs.content = list(codigo.bayes))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxBayes"), opciones = opc_bayes,
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
mod_bayes_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    updateTabsetPanel(session, "BoxBayes",selected = "tabBayesModelo")
    default.codigo.bayes()
  })

  output$txtbayes <- renderPrint({
    input$runBayes
    default.codigo.bayes()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("modelo.bayes")
    modelo <- traineR::train.bayes(as.formula(var), data = train)
    pred   <- predict(modelo , test, type = 'class')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$bayes[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)
  })
  
  output$bayesPrediTable <- DT::renderDataTable({
    idioma <- updateData$idioma
    obj.predic(modelos$bayes[[nombre.modelo$x]]$pred,idioma = idioma)
    
  },server = FALSE)
  
  output$txtbayesMC    <- renderPrint({
    print(modelos$bayes[[nombre.modelo$x]]$mc)
  })
  
  output$plot_bayes_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$bayes[[nombre.modelo$x]]$mc)
  })
  
  output$bayesIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.bayes <- indices.generales(modelos$bayes[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.bayes,"bayes", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  output$bayesIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.bayes <- indices.generales(modelos$bayes[[nombre.modelo$x]]$mc)
    output$bayesPrecGlob  <-  fill.gauges(indices.bayes[[1]], tr("precG",idioma))
    output$bayesErrorGlob <-  fill.gauges(indices.bayes[[2]], tr("errG",idioma))
    xtable(indices.error.table(indices.bayes,"bayes"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  #Código por defecto de bayes
  default.codigo.bayes <- function() {

    #Modelo
    codigo <- bayes.modelo(updateData$variable.predecir)
    updateAceEditor(session, "fieldCodeBayes", value = codigo)

    #Predicción
    codigo <- bayes.prediccion()
    updateAceEditor(session, "fieldCodeBayesPred", value = codigo)

    #Matríz de Confusión
    codigo <- bayes.MC()
    updateAceEditor(session, "fieldCodeBayesMC", value = codigo)

    #Indices generales
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeBayesIG", value = codigo)
  }
  
  # Limpia los datos al ejecutar el botón run
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

}
    
## To be copied in the UI
# mod_bayes_ui("bayes_ui_1")
    
## To be copied in the server
# callModule(mod_bayes_server, "bayes_ui_1")
 
