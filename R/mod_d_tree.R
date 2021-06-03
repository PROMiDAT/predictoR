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
  
  codigo.dt <- list(conditionalPanel("input['d_tree_ui_1-BoxDt']  == 'tabDtPlot'",
                                     codigo.monokai(ns("fieldCodeDtPlot"),height = "10vh")),
                    conditionalPanel("input['d_tree_ui_1-BoxDt']  == 'tabDtPred'",
                                     codigo.monokai(ns("fieldCodeDtPred"),height = "10vh")),
                    conditionalPanel("input['d_tree_ui_1-BoxDt']  == 'tabDtMC'",
                                     codigo.monokai(ns("fieldCodeDtMC"),height = "10vh")),
                    conditionalPanel("input['d_tree_ui_1-BoxDt']  == 'tabDtIndex'",
                                     codigo.monokai(ns("fieldCodeDtIG"),height = "10vh")),
                    conditionalPanel("input['d_tree_ui_1-BoxDt']  == 'tabDtReglas'",
                                     codigo.monokai(ns("fieldCodeDtRule"),height = "10vh")))  
  codigo.dt.run<- list(conditionalPanel("input['d_tree_ui_1-BoxDt']  == 'tabDtModelo'",
                                     codigo.monokai(ns("fieldCodeDt"),height = "10vh")))
  
  opc_dt <- fluidRow(conditionalPanel(
                        "input['d_tree_ui_1-BoxDt']   == 'tabDtModelo'",
                        tabsOptions(heights = c(70, 30), tabs.content = list(
                          list(options.run(ns("runDt")), tags$hr(style = "margin-top: 0px;"),
                               fluidRow(col_6(numericInput(ns("minsplit.dt"), labelInput("minsplit"), 2, width = "100%",min = 1)),
                                        col_6(numericInput(ns("maxdepth.dt"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1))),
                               fluidRow(col_12(selectInput(inputId = ns("split.dt"), label = labelInput("splitIndex"),selected = 1,
                                                           choices =  list("gini" = "gini", "Entropía" = "information"))))),
                          codigo.dt.run))),
                      conditionalPanel(
                        "input['d_tree_ui_1-BoxDt']   != 'tabDtModelo'",
                        tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
                          codigo.dt))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxDt"), opciones = opc_dt,
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
mod_d_tree_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    updateTabsetPanel(session, "BoxDt",selected = "tabDtModelo")
    default.codigo.dt()
  })

  
  # Genera el texto del modelo, predicción y mc de DT
  output$txtDt <- renderPrint({
    input$runDt
    tryCatch({
    default.codigo.dt()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    tipo   <- isolate(input$split.dt)
    minsplit<-isolate(input$minsplit.dt)
    maxdepth<-isolate(input$maxdepth.dt)
    nombre <- paste0("dtl-",tipo)
    modelo <- traineR::train.rpart(as.formula(var), data = train,
                                   control = rpart.control(minsplit = minsplit, maxdepth = maxdepth),parms = list(split = tipo))
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$mdls$dt[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , prob = prob, mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)
  },error = function(e){
    return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$dtPrediTable <- DT::renderDataTable({
    idioma <- updateData$idioma
    obj.predic(modelos$mdls$dt[[nombre.modelo$x]]$pred,idioma = idioma)
    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtDtMC    <- renderPrint({
    print(modelos$mdls$dt[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_dt_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$mdls$dt[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$dtIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.dt <- indices.generales(modelos$mdls$dt[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.dt,"dt", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$dtIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.dt <- indices.generales(modelos$mdls$dt[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$dtPrecGlob  <-  fill.gauges(indices.dt[[1]], tr("precG",idioma))
    output$dtErrorGlob <-  fill.gauges(indices.dt[[2]], tr("errG",idioma))
    xtable(indices.error.table(indices.dt,"dt"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  #Plotear el árbol
  output$plot_dt <- renderPlot({
    tryCatch({
      tipo   <- isolate(input$split.dt)
      datos  <- updateData$datos
      var    <- updateData$variable.predecir
      num    <- length(levels(datos[,var]))
      modelo <- modelos$mdls$dt[[nombre.modelo$x]]$modelo
      updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot(tipo))
      prp(modelo, type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
          fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
          box.col = gg_color_hue(num)[modelo$frame$yval], roundint=FALSE)
    },
    error = function(e){
      output$plot_dt <- renderPlot(NULL)
    })
    
  })
  
  #Mostrar Reglas
  output$rulesDt <- renderPrint({
    tipo           <- isolate(input$split.dt)
    updateAceEditor(session, "fieldCodeDtRule", paste0("asRules(modelo.dt.",tipo,")"))
    rattle::asRules(modelos$mdls$dt[[nombre.modelo$x]]$modelo)
  })
  
  # Actualiza el código a la versión por defecto
  default.codigo.dt <- function() {
    
    tipo   <- isolate(input$split.dt)
    codigo <- dt.modelo(variable.pr = updateData$variable.predecir,
                        minsplit = isolate(input$minsplit.dt),
                        maxdepth = isolate(input$maxdepth.dt),
                        split = tipo)
    
    updateAceEditor(session, "fieldCodeDt", value = codigo)

    # Cambia el código del gráfico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot(tipo))
    
    # Se genera el código de la predicción
    codigo <- dt.prediccion(tipo)
    updateAceEditor(session, "fieldCodeDtPred", value = codigo)

    # Se genera el código de la matriz
    codigo <- dt.MC(tipo)
    updateAceEditor(session, "fieldCodeDtMC", value = codigo)

    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeDtIG", value = codigo)
  }

  # Limpia los datos al ejecutar el botón run
  limpia.dt <- function() {
        output$txtDt          <- renderPrint(invisible(""))
        output$plot_dt        <- renderPlot(NULL)
        output$dtPrediTable   <- DT::renderDataTable(NULL)
        output$plot_dt_mc     <- renderPlot(NULL)
        output$txtDtMC        <- renderPrint(invisible(NULL))
        output$dtIndPrecTable <- shiny::renderTable(NULL)
        output$dtIndErrTable  <- shiny::renderTable(NULL)
        output$dtPrecGlob     <- flexdashboard::renderGauge(NULL)
        output$dtErrorGlob    <- flexdashboard::renderGauge(NULL)
  }
  
}
    
## To be copied in the UI
# mod_d_tree_ui("d_tree_ui_1")
    
## To be copied in the server
# callModule(mod_d_tree_server, "d_tree_ui_1")
 
