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
                       conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrModelo'",
                                        fluidRow(col_6(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"),selected = 1,
                                                                     choices = list("Ridge" = 0, "Lasso" = 1))),
                                                 col_6(radioSwitch(ns("switch.scale.rlr"), "escal", c("si", "no")))),
                                        fluidRow(col_6(id = ns("colManualLanda"),br(),
                                                        numericInput(ns("landa"), labelInput("landa"),value = 2, min = 0, "NULL", width = "100%")), br(),
                                                 col_6(radioSwitch(ns("permitir.landa"), "", c("manual", "automatico"))))),
                       conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda'",
                                        fluidRow(col_12(selectInput(inputId = ns("coeff.sel"),label = labelInput("selectCat"),
                                                                    choices =  "", width = "100%")))))
  
  codigo.rlr.run<- list(conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda'",
                                         codigo.monokai(ns("fieldCodeRlrLanda"), height = "10vh")),
                        conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrModelo'",
                                       codigo.monokai(ns("fieldCodeRlr"), height = "10vh")))
  
  codigo.rlr  <- list(conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrPosibLanda'",
                                       codigo.monokai(ns("fieldCodeRlrPosibLanda"), height = "10vh")),
                      conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrPred'",
                                       codigo.monokai(ns("fieldCodeRlrPred"), height = "10vh")),
                      conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrMC'",
                                       codigo.monokai(ns("fieldCodeRlrMC"), height = "10vh")),
                      conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrIndex'",
                                       codigo.monokai(ns("fieldCodeRlrIG"), height = "10vh")))
  
  opc_rlr  <-   fluidRow(
    conditionalPanel(
      "input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrModelo' || input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda'",
      tabsOptions(heights = c(70, 30), tabs.content = list(
        list(
          conditionalPanel(
            "input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrModelo'",
            options.run(ns("runRlr")), tags$hr(style = "margin-top: 0px;"),
            fluidRow(col_6(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"),selected = 1,
                                       choices = list("Ridge" = 0, "Lasso" = 1))),
                     col_6(radioSwitch(ns("switch.scale.rlr"), "escal", c("si", "no")))),
            fluidRow(col_6(id = ns("colManualLanda"),br(),
                           numericInput(ns("landa"), labelInput("landa"),value = 2, min = 0, "NULL", width = "100%")), br(),
                     col_6(radioSwitch(ns("permitir.landa"), "", c("manual", "automatico") )))),
          conditionalPanel(
            "input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda'",
            options.base(), tags$hr(style = "margin-top: 0px;"),
            fluidRow(col_12(selectInput(inputId = ns("coeff.sel"),label = labelInput("selectCat"),
                                        choices =  "", width = "100%"))))),
        codigo.rlr.run
      ))),
    conditionalPanel(
      "input['penalized_l_r_ui_1-BoxRlr'] != 'tabRlrModelo' && input['penalized_l_r_ui_1-BoxRlr'] != 'tabRlrLanda'",
      tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
        codigo.rlr
      )))
  )
  tagList(
    tabBoxPrmdt(
      id = ns("BoxRlr"), opciones = opc_rlr,
      tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
               withLoader(verbatimTextOutput(ns("txtRlr")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
               withLoader(echarts4rOutput(ns('plot_rlr_posiblanda'), height = "55vh"), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("gcoeff"),value = "tabRlrLanda",
               withLoader(echarts4rOutput(ns('plot_rlr_landa'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabRlrPred",
               withLoader(DT::dataTableOutput(ns("rlrPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabRlrMC",
               withLoader(plotOutput(ns('plot_rlr_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
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
mod_penalized_l_r_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  cv            <- rv(cv.glm = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    choices      <- unique(datos[, variable])
    updateSelectInput(session, "coeff.sel", choices = choices, selected = choices[1])
    updateTabsetPanel(session, "BoxRlr",selected = "tabRlrModelo")
    default.codigo.rlr()
  })
  
  # Genera el texto del modelo, predicción y mc de RLR
  output$txtRlr <- renderPrint({
    input$runRlr
    tryCatch({
    default.codigo.rlr()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    scales <- isolate(input$switch.scale.rlr)
    tipo   <- rlr.type()
    alpha  <- isolate(input$alpha.rlr)
    nombre <- paste0("rlr-",tipo)
    modelo <- traineR::train.glmnet(as.formula(var), data = train, standardize = as.logical(scales), alpha = alpha, family = 'multinomial' )
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$mdls$rlr[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc))
    nombre.modelo$x <- nombre
    x         <- model.matrix(as.formula(var), train)[, -1]
    y         <- train[,updateData$variable.predecir]
    cv$cv.glm <- glmnet::cv.glmnet(x, y, standardize = as.logical(scales), alpha = alpha ,family = 'multinomial')
    
    print(modelo)
  },error = function(e){
    return(invisible(""))
  })
  })
  
  #Tabla de la predicción
  output$rlrPrediTable <- DT::renderDataTable({
    idioma <- updateData$idioma
    obj.predic(modelos$mdls$rlr[[nombre.modelo$x]]$pred,idioma = idioma)
    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtrlrMC    <- renderPrint({
    print(modelos$mdls$rlr[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_rlr_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$mdls$rlr[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$rlrIndPrecTable <- shiny::renderTable({
    idioma      <- updateData$idioma
    indices.rlr <- indices.generales(modelos$mdls$rlr[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.rlr,"rlr", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$rlrIndErrTable  <- shiny::renderTable({
    idioma      <- updateData$idioma
    indices.rlr <- indices.generales(modelos$mdls$rlr[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$rlrPrecGlob  <-  fill.gauges(indices.rlr[[1]], tr("precG",idioma))
    output$rlrErrorGlob <-  fill.gauges(indices.rlr[[2]], tr("errG",idioma))
    xtable(indices.error.table(indices.rlr,"rlr"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Habilita o deshabilita la semilla
  observeEvent(input$permitir.landa, {
    if (input$permitir.landa) {
      shinyjs::enable("landa")
    } else {
      shinyjs::disable("landa")
    }
  })

  #Obtiene el lambda seleccionado
  get_landa_rlr <- function(){
    landa <- NULL
    if (!is.na(isolate(input$landa)) && (isolate(input$permitir.landa)=="TRUE")) {
      if (isolate(input$landa) > 0) {
        landa <- isolate(input$landa)
      }
    }
    return(landa)
  }
  
  # Actualiza el código a la versión por defecto
  default.codigo.rlr <- function(){
    tipo  <- rlr.type()


    # Se actualiza el código del modelo
    codigo <- rlr.modelo(variable.pr = updateData$variable.predecir,
                         type        = tipo,
                         isolate(input$alpha.rlr),
                         isolate(input$switch.scale.rlr))
    
    updateAceEditor(session, "fieldCodeRlr", value = codigo)

    # Se genera el código del posible lambda
    codigo <- select.landa(updateData$variable.predecir,
                           isolate(input$alpha.rlr),
                           isolate(input$switch.scale.rlr),
                           tipo)

    updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)

    # Se genera el código de la predicción
    codigo <- rlr.prediccion(tipo)
    updateAceEditor(session, "fieldCodeRlrPred", value = codigo)

    # Se genera el código de la matriz
    codigo <- rlr.MC(tipo)
    updateAceEditor(session, "fieldCodeRlrMC", value = codigo)

    # Se genera el código de los indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlrIG", value = codigo)
  }
  
  #Gráfica de los Lambdas
  output$plot_rlr_posiblanda <- renderEcharts4r({

    tryCatch({  
      e_posib_lambda(cv$cv.glm)
    },
    error = function(e) { 
      showNotification(paste0("Error (R/L) : ", e), duration = 15, type = "error")
      return(NULL)
    })

  })
  
  #Gráfica de los coeficientes Lambdas
  output$plot_rlr_landa <- renderEcharts4r({
    tryCatch({  
      lambda <- get_landa_rlr()
      tipo   <- rlr.type()
      cv.glm <- cv$cv.glm
      coeff  <- input$coeff.sel
      modelo <- modelos$mdls$rlr[[nombre.modelo$x]]$modelo
      updateAceEditor(session, "fieldCodeRlrLanda", value = paste0("e_coeff_landa(modelo.rlr.",tipo,", '",coeff,"', ",lambda,", cv.glm.",tipo,")"))
      e_coeff_landa(modelo, coeff, log(lambda), cv.glm)
    },
    error = function(e){ 
      showNotification(paste0("Error (R/L) : ", e), duration = 15, type = "error")
    })
  })

  #Obtiene el algortimo a utilizar
  rlr.type <- function(){
    ifelse(isolate(input$alpha.rlr) == 0, "ridge", "lasso")
  }
  
  # Limpia los datos al ejecutar el botón run
  limpia.rlr <- function() {
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
# mod_penalized_l_r_ui("penalized_l_r_ui_1")
    
## To be copied in the server
# callModule(mod_penalized_l_r_server, "penalized_l_r_ui_1", updateData)
 
