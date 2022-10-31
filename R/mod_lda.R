#' lda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_lda_ui <- function(id){
  ns <- NS(id)
  
  opciones <-   
    div(
      conditionalPanel(
        "input['lda_ui_1-Boxlda'] == 'tabldaModelo' || input['lda_ui_1-Boxlda']  == 'tabldaPlot' ",
        tabsOptions(heights = c(70), tabs.content = list(
          list(
            conditionalPanel(
            "input['lda_ui_1-Boxlda'] == 'tabldaModelo'",
            options.run(ns("runlda")), tags$hr(style = "margin-top: 0px;")),
            conditionalPanel(
              "input['lda_ui_1-Boxlda']  == 'tabldaPlot'",
              options.base(), tags$hr(style = "margin-top: 0px;"),
              selectizeInput(ns("select_var_lda_plot"),NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                             options = list(maxItems = 2, placeholder = ""), width = "100%"))
          
        ))))
    )
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxlda"), opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabldaModelo",
               withLoader(verbatimTextOutput(ns("txtlda")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("gclasificacion"), value = "tabldaPlot",
               withLoader(plotOutput(ns('plot_lda'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabldaPred",
               withLoader(DT::dataTableOutput(ns("ldaPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabldaMC",
               withLoader(plotOutput(ns('plot_lda_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtldaMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabldaIndex",
               fluidRow(col_6(echarts4rOutput(ns("ldaPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("ldaErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("ldaIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("ldaIndErrTable")))))
    )
  )
}
    
#' lda Server Function
#'
#' @noRd 
mod_lda_server <- function(input, output, session, updateData, modelos, codedioma){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    nombres <- colnames.empty(var.numericas(updateData$datos))
    updateSelectizeInput(session, "select_var_lda_plot", choices = nombres)
    updateTabsetPanel(session, "Boxlda",selected = "tabldaModelo")
  })
  
  # Genera el texto del modelo, predicción y mc de lda
  output$txtlda <- renderPrint({
    input$runlda
    tryCatch({
    default.codigo.lda()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("lda")
    modelo <- traineR::train.lda(as.formula(var), data = train)
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$lda[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , prob = prob, mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)    
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$ldaPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$lda[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtldaMC    <- renderPrint({
    print(modelos$lda[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_lda_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$lda[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$ldaIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.lda <- indices.generales(modelos$lda[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.lda,"lda", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$ldaIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.lda <- indices.generales(modelos$lda[[nombre.modelo$x]]$mc)

    #Gráfico de Error y Precisión Global
    output$ldaPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.lda[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$ldaErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.lda[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.lda,"lda"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Update LDA plot
  output$plot_lda <- renderPlot({
    tryCatch({
      idioma    <- codedioma$idioma
      train     <- updateData$datos.aprendizaje
      variable  <- isolate(updateData$variable.predecir)
      variables <- input$select_var_lda_plot
      # cod       <- svm.plot(variable, train, variables, colnames(datos[, -which(colnames(datos) == variable)]), k)
      # cod       <- paste0("### gclasificacion\n",cod)
      modelo <- modelos$lda[[nombre.modelo$x]]$modelo
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
  #Código por defecto de lda
  default.codigo.lda <- function() {
    
    #Modelo
    codigo <- codigo.modelo("lda", updateData$variable.predecir)
    cod    <- paste0("### adl\n",codigo)

    #Predicción
    codigo <- codigo.prediccion("lda")
    cod  <- paste0(cod,codigo)
    
    #Matríz de Confusión
    codigo <- codigo.MC("lda")
    cod  <- paste0(cod,codigo)
    
    #Indices generales
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.lda)\n")
    cod  <- paste0(cod,codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}
    
## To be copied in the UI
# mod_lda_ui("lda_ui_1")
    
## To be copied in the server
# callModule(mod_lda_server, "lda_ui_1")
 
