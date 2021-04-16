#' svm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_svm_ui <- function(id){
  ns <- NS(id)
  opciones.svm <- list(options.run(ns("runSvm")), tags$hr(style = "margin-top: 0px;"),
                       conditionalPanel("input.BoxSvm != 'tabSvmPlot'",
                                        fluidRow(col_6(
                                                       radioSwitch(ns("switch.scale.svm"), "escal", c("si", "no"))),
                                                 col_6(
                                                        selectInput(inputId = ns("kernel.svm"), label = labelInput("selkernel"), selected = "radial",
                                                                    choices =  c("linear", "polynomial", "radial", "sigmoid"))))),
                       conditionalPanel("input.BoxSvm == 'tabSvmPlot'",
                                        selectizeInput(ns("select_var_svm_plot"),NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                                       options = list(maxItems = 2, placeholder = ""), width = "100%")))
  
  codigo.svm <- list(conditionalPanel("input.BoxSvm == 'tabSvmModelo'",
                                      codigo.monokai(ns("fieldCodeSvm"),height = "10vh")),
                     conditionalPanel("input.BoxSvm == 'tabSvmPlot'",
                                      codigo.monokai(ns("fieldCodeSvmPlot"),height = "10vh")),
                     conditionalPanel("input.BoxSvm == 'tabSvmPred'",
                                      codigo.monokai(ns("fieldCodeSvmPred"),height = "10vh")),
                     conditionalPanel("input.BoxSvm == 'tabSvmMC'",
                                      codigo.monokai(ns("fieldCodeSvmMC"),height = "10vh")),
                     conditionalPanel("input.BoxSvm == 'tabSvmIndex'",
                                      codigo.monokai(ns("fieldCodeSvmIG"),height = "10vh")))
  
  opc_svm <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(60, 60),
                          tabs.content = list(opciones.svm, codigo.svm))
  tagList(
    tabBoxPrmdt(
      id = "BoxSvm", opciones = opc_svm,
      tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
               withLoader(verbatimTextOutput(ns("txtSvm")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("gclasificacion"), value = "tabSvmPlot",
               withLoader(plotOutput(ns('plot_svm'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabSvmPred",
               withLoader(DT::dataTableOutput(ns("svmPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabSvmMC",
               withLoader(plotOutput(ns('plot_svm_mc'), height = "45vh"), 
               type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtSvmMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabSvmIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("svmPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("svmErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("svmIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("svmIndErrTable")))))
    )
  )
}
    
#' svm Server Function
#'
#' @noRd 
mod_svm_server <- function(input, output, session, updateData){
  ns <- session$ns
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    nombres <- colnames.empty(var.numericas(updateData$datos))
    updateSelectizeInput(session, "select_var_svm_plot", choices = nombres)
    limpiar()
    default.codigo.svm()

  })
  # 
  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     kernel <- isolate(input$kernel.svm)
  #     if(exists(paste0("prediccion.svm.",kernel))){
  #     ejecutar.svm.mc()
  #     ejecutar.svm.ind()
  #     updateData$selector.comparativa <- actualizar.selector.comparativa()
  #     }
  #   }
  # })


  # Cuando se genera el modelo knn
  observeEvent(input$runSvm, {
    if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
      #limpia.svm.run()
      default.codigo.svm()
      svm.full()
    }
  }, priority =  -5)
  
  
 
  svm.full <- function() {
     ejecutar.svm()
     ejecutar.svm.pred()
     ejecutar.svm.mc()
     ejecutar.svm.ind()
  }
  
  ejecutar.svm <- function() {
    tryCatch({ 
      isolate(exe(cod.svm.modelo))
      kernel <- isolate(input$kernel.svm)
      output$txtSvm <- renderPrint(exe("print(modelo.svm.",kernel,")"))
      
      updateAceEditor(session, "fieldCodeSvm", value = cod.svm.modelo)
      
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.svm.",kernel))
    },
    error = function(e) { 
      limpia.svm(1)
      showNotification(paste0("Error (SVM-01) : ",e), duration = 15, type = "error")
    })
  }
  
  ejecutar.svm.pred <- function() {
    tryCatch({ 
      isolate(exe(cod.svm.pred))
      kernel <- isolate(input$kernel.svm)
      idioma <- updateData$idioma
      pred   <- predict(exe("modelo.svm.",kernel), datos.prueba, type = "prob")
      scores[[paste0("svml-",kernel)]] <<- pred$prediction[,2]
      
      output$svmPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.svm.",kernel),idioma = idioma),server = FALSE)

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.svm.",kernel))
      
      updatePlot$roc <- !updatePlot$roc 
    },
    error = function(e) { 
      limpia.svm(2)
      showNotification(paste0("Error (SVM-02) : ",e), duration = 15, type = "error")
    })
  }

  ejecutar.svm.mc <- function(){
    kernel <- isolate(input$kernel.svm)
    idioma <- updateData$idioma
    
    if(exists(paste0("prediccion.svm.",kernel))){
      tryCatch({ 
        
        exe(cod.svm.mc)
        output$txtSvmMC <- renderPrint(exe("print(MC.svm.",kernel,")"))
        exe(plot.MC.code(idioma = idioma))
        output$plot_svm_mc <- renderPlot(exe("plot.MC(MC.svm.",kernel,")"))
        
        nombres.modelos   <<- c(nombres.modelos, paste0("MC.svm.",kernel))
      },
      error = function(e) { 
        limpia.svm(3)
        showNotification(paste0("Error (SVM-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Genera los indices
  ejecutar.svm.ind <- function(){
    idioma <- updateData$idioma
    kernel <- isolate(input$kernel.svm)
    if(exists(paste0("MC.svm.",kernel))){
      tryCatch({ 
        isolate(exe(cod.svm.ind))
        indices.svm <- indices.generales(exe("MC.svm.",kernel))

        eval(parse(text =paste0("indices.svm.",kernel, "<<- indices.svm")))
        
        output$svmPrecGlob  <-  fill.gauges(indices.svm[[1]], tr("precG",idioma))
        output$svmErrorGlob <-  fill.gauges(indices.svm[[2]], tr("errG",idioma))

        # Cambia la tabla con la indices de svm
        output$svmIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.svm,"SVM", idioma = idioma)), spacing = "xs",
                                                     bordered = T, width = "100%", align = "c", digits = 2)
        
        output$svmIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.svm,"SVM")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        
        nombres.modelos <<- c(nombres.modelos, paste0("indices.svm.",kernel))
        IndicesM[[paste0("svml-",kernel)]] <<- indices.svm
        updateData$selector.comparativa <- actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.svm(4)
        showNotification(paste0("Error (SVM-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  default.codigo.svm <- function() {
    kernel <- isolate(input$kernel.svm)
    # Se actualiza el codigo del modelo
    codigo <- svm.modelo(variable.pr = updateData$variable.predecir,
                         scale = input$switch.scale.svm,
                         kernel = kernel)
    
    updateAceEditor(session, "fieldCodeSvm", value = codigo)
    cod.svm.modelo <<- codigo
    
    # Se genera el codigo de la prediccion
    codigo <- svm.prediccion(kernel)
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo)
    cod.svm.pred <<- codigo
    
    # Se genera el codigo de la matriz
    codigo <- svm.MC(kernel)
    updateAceEditor(session, "fieldCodeSvmMC", value = codigo)
    cod.svm.mc <<- codigo
    
    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo)
    cod.svm.ind <<- codigo
    
  }
  
  #Hace el grafico de svm
  output$plot_svm <- renderPlot({
    tryCatch({
      datos <- updateData$datos
      codigo <- updatePlot$svm.graf
      if(!is.null(codigo) & codigo != ""){
        exe(codigo)
      }else{
        if(!(ncol(var.numericas(datos)) >= 2)){
          error.variables(T)
        }else{
          return(NULL)
        }
      }},error = function(e){
        return(NULL)
      })
  })
  # Cuando cambia el codigo del grafico de clasificacion svm
  observeEvent(c(input$runSvm),{
    variable.predecir <- updateData$variable.predecir
    datos <- updateData$datos
    if (length(input$select_var_svm_plot) == 2){
      v <- colnames(datos)
      v <- v[v != variable.predecir]
      v <- v[!(v %in% input$select_var_svm_plot)]
      if(length(v) == 0){
        v <- input$select_var_svm_plot
      }
      isolate(kernel <- input$kernel.svm)
      updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(input$select_var_svm_plot, v, kernel))
    }else{
      updatePlot$svm.graf <- NULL
    }
  })
  
  observeEvent(c(input$fieldCodeSvmPlot),{
    updatePlot$svm.graf <- input$fieldCodeSvmPlot
  })
  
  # Limpia los datos segun el proceso donde se genera el error
  limpia.svm <- function(capa = NULL){
    for(i in capa:4){
      switch(i, {
        exe("modelo.svm.",input$kernel.svm,"<<- NULL")
        output$txtSvm <- renderPrint(invisible(""))
      }, {
        exe("prediccion.svm.",input$kernel.svm,"<<- NULL")
        output$svmPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.svm.",input$kernel.svm,"<<- NULL")
        output$txtSvmMC <- renderPrint(invisible(""))
        output$plot_svm_mc <- renderPlot(NULL)
      }, {
        IndicesM[[paste0("svml-",input$kernel.svm)]] <<- NULL
        exe("indices.svm.",input$kernel.svm,"<<- NULL")
        output$svmIndPrecTable <- shiny::renderTable(NULL)
        output$svmIndErrTable  <- shiny::renderTable(NULL)
        output$svmPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$svmErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }
  
  # Limpia los datos al momento de ejecutar run
  limpia.svm.run <- function(){
        output$txtSvm <- renderPrint(invisible(""))
  
        output$svmPrediTable <- DT::renderDataTable(NULL)
     
        output$txtSvmMC <- renderPrint(invisible(""))
        output$plot_svm_mc <- renderPlot(NULL)
 
        output$svmIndPrecTable <- shiny::renderTable(NULL)
        output$svmIndErrTable  <- shiny::renderTable(NULL)
        output$svmPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$svmErrorGlob    <-  flexdashboard::renderGauge(NULL)
}
  
  limpiar <- function(){
    limpia.svm(1)
    limpia.svm(2)
    limpia.svm(3)
    limpia.svm(4)
  }
}
    
## To be copied in the UI
# mod_svm_ui("svm_ui_1")
    
## To be copied in the server
# callModule(mod_svm_server, "svm_ui_1")
 
