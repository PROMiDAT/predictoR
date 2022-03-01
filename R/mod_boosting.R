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

  opciones <-   
    div(
      conditionalPanel(
        "input['boosting_ui_1-BoxB'] == 'tabBModelo' || input['boosting_ui_1-BoxB'] == 'tabBRules'",
        tabsOptions(heights = c(70), tabs.content = list(
          list(
            conditionalPanel(
              "input['boosting_ui_1-BoxB'] == 'tabBModelo'",
              options.run(ns("runBoosting")), tags$hr(style = "margin-top: 0px;"),
              fluidRow(col_6(numericInput(ns("iter.boosting"), labelInput("numTree"), 20, width = "100%",min = 1)),
                       col_6(numericInput(ns("maxdepth.boosting"), labelInput("maxdepth"), 15, width = "100%",min = 1))),
              
              fluidRow(col_6(numericInput(ns("minsplit.boosting"), labelInput("minsplit"), 20, width = "100%",min = 1)))           ),
            conditionalPanel(
              "input['boosting_ui_1-BoxB'] == 'tabBRules'",
              options.base(), tags$hr(style = "margin-top: 0px;"),
              numericInput(ns("rules.b.n"),labelInput("ruleNumTree"),1, width = "100%", min = 1)))
          
        )))
    )

  tagList(
    tabBoxPrmdt(
      id = ns("BoxB"), opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabBModelo",
               withLoader(verbatimTextOutput(ns("txtBoosting")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("evolerror"), value = "tabBError",
               withLoader(echarts4rOutput(ns('plot_boosting_error'), height = "55vh"), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("varImp"), value = "tabBImp",
               withLoader(echarts4rOutput(ns('plot_boosting_import'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabBPred",
               withLoader(DT::dataTableOutput(ns("boostingPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabBMC",
               withLoader(plotOutput(ns('plot_boosting_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtBoostingMC"))),
      
      tabPanel(title = labelInput("indices"),value = "tabBIndex",
               fluidRow(col_6(echarts4rOutput(ns("boostingPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("boostingErrorGlob"), width = "100%"))),
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
mod_boosting_server <- function(input, output, session, updateData, modelos, codedioma){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    updateTabsetPanel(session, "BoxB",selected = "tabBModelo")
    #default.codigo.boosting()
  })
  
  # Genera el texto del modelo, predicción y mc de boosting
  output$txtBoosting <- renderPrint({
    input$runBoosting
    tryCatch({
    default.codigo.boosting()
    train   <- updateData$datos.aprendizaje
    test    <- updateData$datos.prueba
    var     <- paste0(updateData$variable.predecir, "~.")
    iter    <- isolate(input$iter.boosting)
    maxdepth<-isolate(input$maxdepth.boosting)
    minsplit<-isolate(input$minsplit.boosting)
    nombre  <- paste0("bl")
    modelo  <- traineR::train.adabag(as.formula(var), data = train, mfinal = iter,
                                    control = rpart.control(minsplit =minsplit, maxdepth = maxdepth))
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$boosting[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)
    }, error = function(e) {
      return(invisible(""))
    })
  })

  #Tabla de la predicción
  output$boostingPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$boosting[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtBoostingMC    <- renderPrint({
    print(modelos$boosting[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_boosting_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$boosting[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$boostingIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.boosting <- indices.generales(modelos$boosting[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.boosting,"boosting", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$boostingIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.boosting <- indices.generales(modelos$boosting[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$boostingPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.boosting[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$boostingErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.boosting[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.boosting,"boosting"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Mostrar Reglas
  output$rulesB <- renderPrint({
    n <- input$rules.b.n
    isolate(train   <- updateData$datos.aprendizaje)
    isolate(var.pred<- updateData$variable.predecir)
    tryCatch({
      isolate(codedioma$code <- append(codedioma$code, rules.boosting(n)))
      rules(modelos$boosting[[nombre.modelo$x]]$modelo$trees[[n]], train, var.pred)
    },error = function(e) {
      stop(tr("NoDRule", codedioma$idioma))
    }
  )})
  
  # Actualiza el código a la versión por defecto
  default.codigo.boosting <- function() {

    # Se actualiza el código del modelo
    codigo <- boosting.modelo(variable.pr = updateData$variable.predecir,
                              iter        = isolate(input$iter.boosting),
                              maxdepth    = isolate(input$maxdepth.boosting),
                              minsplit    = isolate(input$minsplit.boosting))
    
    cod  <- paste0("### docpot\n",codigo)
    
    # Se genera el código de la predicción
    codigo <- boosting.prediccion()
    cod  <- paste0(cod,codigo)
    

    # Se genera el código de la matriz
    codigo <- boosting.MC()
    cod  <- paste0(cod,codigo)
    
    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    codigo <- paste0(codigo,"\nindices.generales(MC.boosting)\n")
    
    
    cod  <- paste0(cod, codigo)
    # Cambia el código del gráfico del modelo
    cod  <- paste0(cod, "### evolerror\n",boosting.plot())
    # Cambia el código del gráfico de importancia
    cod  <- paste0(cod, "### docImpV\n",boosting.plot.import())
    
    isolate(codedioma$code <- append(codedioma$code, cod))
    
  }
  
  # Gráfico de importancia boosting
  output$plot_boosting_import <- renderEcharts4r({
    cod <- ifelse(input$fieldCodeBoostingPlotImport == "",boosting.plot.import(),input$fieldCodeBoostingPlotImport)
    tryCatch({
      imp   <- modelos$boosting[[nombre.modelo$x]]$modelo$importance
      color <- gg_color_hue(length(imp))
      aux   <- data.frame(importancia = imp, color = color) 
      aux$nombre      <- row.names(aux) 
      aux$importancia <- abs(aux$importancia) 
      aux <- aux[order(aux$importancia, decreasing = T), ]
      aux |>  e_charts(nombre) |>  e_bar(importancia, name = var) |>   
        e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|>
        e_add_nested("itemStyle", color) |>   
        e_flip_coords() |>  
        e_y_axis(inverse = TRUE)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Gráfico de evolución del error boosting
  output$plot_boosting_error <- renderEcharts4r({
    train  <- updateData$datos.aprendizaje
    modelo <- modelos$boosting[[nombre.modelo$x]]$modelo
    cod <- ifelse(input$fieldCodeBoostingPlot == "",boosting.plot(),input$fieldCodeBoostingPlot)
    tryCatch({
      error(modelo, train) -> evol.train
      e_evol_error(evol.train,strsplit(tr("numTree", codedioma$idioma), ':')[[1]])
    }, error = function(e) {
      return(NULL)
    })
  })

}
    
## To be copied in the UI
# mod_boosting_ui("boosting_ui_1")
    
## To be copied in the server
# callModule(mod_boosting_server, "boosting_ui_1")
 
