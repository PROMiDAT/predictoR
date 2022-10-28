#' qda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_qda_ui <- function(id){
  ns <- NS(id)
  
  opciones <-   
    div(
      conditionalPanel(
        "input['qda_ui_1-Boxqda'] == 'tabqdaModelo' || input['qda_ui_1-Boxqda']  == 'tabqdaPlot' ",
        tabsOptions(heights = c(70), tabs.content = list(
          list(
            conditionalPanel(
            "input['qda_ui_1-Boxqda'] == 'tabqdaModelo'",
            options.run(ns("runqda")), tags$hr(style = "margin-top: 0px;")),
            conditionalPanel(
              "input['qda_ui_1-Boxqda']  == 'tabqdaPlot'",
              options.base(), tags$hr(style = "margin-top: 0px;"),
              selectizeInput(ns("select_var_qda_plot"),NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                             options = list(maxItems = 2, placeholder = ""), width = "100%"))
          
        ))))
    )
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxqda"), opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabqdaModelo",
               withLoader(verbatimTextOutput(ns("txtqda")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("gclasificacion"), value = "tabqdaPlot",
               withLoader(plotOutput(ns('plot_qda'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabqdaPred",
               withLoader(DT::dataTableOutput(ns("qdaPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabqdaMC",
               withLoader(plotOutput(ns('plot_qda_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtqdaMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabqdaIndex",
               fluidRow(col_6(echarts4rOutput(ns("qdaPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("qdaErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("qdaIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("qdaIndErrTable")))))
    )
  )
}
    
#' qda Server Function
#'
#' @noRd 
mod_qda_server <- function(input, output, session, updateData, modelos, codedioma){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    nombres <- colnames.empty(var.numericas(updateData$datos))
    updateSelectizeInput(session, "select_var_qda_plot", choices = nombres)
    updateTabsetPanel(session, "Boxqda",selected = "tabqdaModelo")
  })
  
  # Genera el texto del modelo, predicción y mc de qda
  output$txtqda <- renderPrint({
    input$runqda
    tryCatch({
    default.codigo.qda()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("qda")
    modelo <- traineR::train.qda(as.formula(var), data = train)
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$qda[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , prob = prob, mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)    
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$qdaPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$qda[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtqdaMC    <- renderPrint({
    print(modelos$qda[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_qda_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$qda[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$qdaIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.qda <- indices.generales(modelos$qda[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.qda,"qda", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$qdaIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.qda <- indices.generales(modelos$qda[[nombre.modelo$x]]$mc)

    #Gráfico de Error y Precisión Global
    output$qdaPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.qda[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$qdaErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.qda[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.qda,"qda"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Update qda plot
  output$plot_qda <- renderPlot({
    tryCatch({
      idioma    <- codedioma$idioma
      train     <- updateData$datos.aprendizaje
      variable  <- isolate(updateData$variable.predecir)
      variables <- input$select_var_qda_plot
      # cod       <- svm.plot(variable, train, variables, colnames(datos[, -which(colnames(datos) == variable)]), k)
      # cod       <- paste0("### gclasificacion\n",cod)
      modelo <- modelos$qda[[nombre.modelo$x]]$modelo
      if (length(variables) == 2){
        #isolate(codedioma$code <- append(codedioma$code, cod))
        plot(modelo, col = as.numeric(train[, variable]))
      }
      else{
        return(NULL)
      }
    },error = function(e){
      showNotification(e,
                       duration = 10,
                       type = "error")
      return(NULL)
    })
  })
  #Código por defecto de qda
  default.codigo.qda <- function() {
    
    #Modelo
    codigo <- qda.modelo(updateData$variable.predecir)
    cod    <- paste0("### qda\n",codigo)

    #Predicción
    codigo <- qda.prediccion()
    cod  <- paste0(cod,codigo)
    
    #Matríz de Confusión
    codigo <- qda.MC()
    cod  <- paste0(cod,codigo)
    
    #Indices generales
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.qda)\n")
    cod  <- paste0(cod,codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}
    
## To be copied in the UI
# mod_qda_ui("qda_ui_1")
    
## To be copied in the server
# callModule(mod_qda_server, "qda_ui_1")
 
