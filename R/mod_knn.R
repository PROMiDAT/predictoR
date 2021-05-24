#' knn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_knn_ui <- function(id){
  ns <- NS(id)
  opciones.knn <- list(options.run(ns("runKnn")), tags$hr(style = "margin-top: 0px;"),
                       fluidRow(col_6(
                                      numericInput(ns("kmax.knn"), labelInput("kmax"), min = 1,step = 1, value = 7)),
                                col_6(
                                      selectInput(inputId = ns("kernel.knn"), label = labelInput("selkernel"),selected = 1,
                                                   choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                               "triweight", "cos","inv","gaussian")))),
                       fluidRow(col_6(
                                      radioSwitch(ns("switch.scale.knn"), "escal", c("si", "no")))))
  
  codigo.knn <- list(conditionalPanel("input.BoxKnn == 'tabKknModelo'",
                                      codigo.monokai(ns("fieldCodeKnn"), height = "10vh")),
                     conditionalPanel("input.BoxKnn == 'tabKknPred'",
                                      codigo.monokai(ns("fieldCodeKnnPred"),height = "10vh")),
                     conditionalPanel("input.BoxKnn == 'tabKknMC'",
                                      codigo.monokai(ns("fieldCodeKnnMC"),height = "10vh")),
                     conditionalPanel("input.BoxKnn == 'tabKknIndex'",
                                      codigo.monokai(ns("fieldCodeKnnIG"),height = "10vh")))
  
opc_knn <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                          tabs.content = list(opciones.knn, codigo.knn))

  tagList(
    tabBoxPrmdt(
      id = "BoxKnn", opciones = opc_knn,
      tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
               conditionalPanel(condition="($('html').hasClass('shiny-busy'))",
                                div(class = "loaderWrapper", div(class="loader"))),
               withLoader(verbatimTextOutput(ns("txtknn")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabKknPred",
               withLoader(DT::dataTableOutput(ns("knnPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabKknMC",
               withLoader(plotOutput(ns('plot_knn_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtknnMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabKknIndex",
               fluidRow(col_6(flexdashboard::gaugeOutput(ns("knnPrecGlob"), width = "100%")),
                        col_6(flexdashboard::gaugeOutput(ns("knnErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("knnIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("knnIndErrTable")))))
      )
  )
}
    
#' knn Server Function
#'
#' @noRd 
mod_knn_server <- function(input, output, session, updateData){
  ns <- session$ns

  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    limpiar()
    default.codigo.knn(k.def = TRUE)
  })
  
  #EN PREDICTOR HACEN LO SIGUIENTE PARA EL IDIOMA
  # observeEvent(input$idioma, {
  #   
  #   
  #   updatePlot$normal <- default.normal("datos", input$sel.normal, input$col.normal, tr("curvanormal"))
  #   updatePlot$dya.cat <- def.code.cat(variable = input$sel.distribucion.cat, titulox = tr("cantidadcasos"), tituloy = tr("categorias"))
  #   updatePlot$calc.normal <- default.calc.normal(labelsi = tr("positivo"),labelno=tr("negativo"),labelsin=tr("sinasimetria"))
  #   updatePlot$poder.pred <- plot.code.poder.pred(variable.predecir, label= tr("distrelvar"))
  #   updatePlot$poder.dens <- plot.numerico.dens(input$sel.density.poder,tr("denspodlab"))
  #   updatePlot$poder.cat <- plot.code.dist.porc(input$sel.distribucion.poder,variable.predecir, label=tr("distpodcat"))
  #   
  #   ejecutar.knn.mc()
  #   ejecutar.knn.ind()
  #   ejecutar.svm.mc()
  #   ejecutar.svm.ind()
  #   ejecutar.dt.mc()
  #   ejecutar.dt.ind()
  #   ejecutar.rf.mc()
  #   ejecutar.rf.ind()
  #   ejecutar.rl.mc()
  #   
  #   actualizar.selector.comparativa()
  # })
  # 
  # observeEvent(updateData$idioma, {
  #   if(!is.null(updateData$datos.aprendizaje) & !is.null(updateData$datos.prueba)){
  #     kernel <- isolate(input$kernel.knn)
  #     
  #     if(exists(paste0("prediccion.knn.",kernel))){
  #       print("SI")
  #       ejecutar.knn.mc()
  #       ejecutar.knn.ind()
  #       updateData$selector.comparativa <- actualizar.selector.comparativa()
  #     }
  #   }
  # })

  # Ejecuta el modelo, predicción, mc e indices de knn
  knn.full <- function() {
     ejecutar.knn()
     ejecutar.knn.pred()
     ejecutar.knn.mc()
     ejecutar.knn.ind()
  }

  # Cuando se genera el modelo knn
  observeEvent(input$runKnn, {
    if (validar.datos(variable.predecir = updateData$variable.predecir,datos.aprendizaje = updateData$datos.aprendizaje)) { # Si se tiene los datos entonces :
      limpia.knn.run()
      default.codigo.knn()
      knn.full()
    }
  }, priority =  -5)
  

  # Genera el modelo
  ejecutar.knn <- function() {
    tryCatch({
      kernel <- isolate(input$kernel.knn)
      exe(cod.knn.modelo)
      updateAceEditor(session, "fieldCodeKnn", value = cod.knn.modelo)
      output$txtknn   <-  renderPrint(exe("modelo.knn.",kernel))
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.knn.",kernel))
    },
    error = function(e) { 
      limpia.knn(1)
      showNotification(paste0("Error (KNN-01) : ", e), duration = 15, type = "error")
    }
    )
  }
  
  # Genera la predicción
  ejecutar.knn.pred <- function() {
    tryCatch({ 
      # Se corren los códigos
      exe(cod.knn.pred)
      idioma <- updateData$idioma
      kernel <- isolate(input$kernel.knn)
      pred   <- predict(exe("modelo.knn.",kernel), datos.prueba, type = "prob")
      scores[[paste0("knnl-",kernel)]] <<- pred$prediction[,2]
      
      # Cambia la tabla con la predicción de knn
      output$knnPrediTable <-  DT::renderDataTable(obj.predic(exe("prediccion.knn.",kernel),idioma = idioma),server = FALSE)
      nombres.modelos      <<- c(nombres.modelos, paste0("prediccion.knn.",kernel))
      updateData$roc <- !updateData$roc #graficar otra vez la curva roc
    },
    error = function(e) { 
      # Regresamos al estado inicial y mostramos un error
      limpia.knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Genera la matriz de confusión
  ejecutar.knn.mc <- function() {
    idioma <- updateData$idioma
    kernel <- isolate(input$kernel.knn)
    if(exists(paste0("prediccion.knn.",kernel))){
      tryCatch({ 
        # Se corren los códigos
        exe(cod.knn.mc)
        output$txtknnMC <- renderPrint(print(exe("MC.knn.",kernel)))
        
        exe(plot.MC.code(idioma = idioma))
        output$plot_knn_mc <-  renderPlot(exe("plot.MC(MC.knn.",kernel,")"))
        nombres.modelos    <<- c(nombres.modelos, paste0("MC.knn.",kernel))
      },
      error = function(e) { 
        # Regresamos al estado inicial y mostramos un error
        limpia.knn(3)
        showNotification(paste0("Error (KNN-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Genera los indices
  ejecutar.knn.ind <- function(){
    idioma <- updateData$idioma
    kernel <- isolate(input$kernel.knn)
    if(exists(paste0("MC.knn.",kernel))){
      tryCatch({ 
        # Se corren los códigos
        isolate(exe(cod.knn.ind))
        
        indices.knn <- indices.generales(exe("MC.knn.",kernel))
        eval(parse(text = paste0("indices.knn.",kernel, "<<- indices.knn")))
        
        output$knnPrecGlob  <-  fill.gauges(indices.knn[[1]], tr("precG",idioma))
        output$knnErrorGlob <-  fill.gauges(indices.knn[[2]], tr("errG",idioma))
        
        # Cambia la tabla con la indices de knn
        output$knnIndPrecTable <-  shiny::renderTable(xtable(indices.prec.table(indices.knn,"KNN", idioma = idioma)), spacing = "xs",
                                                     bordered = T, width = "100%", align = "c", digits = 2)
        output$knnIndErrTable  <-  shiny::renderTable(xtable(indices.error.table(indices.knn,"KNN")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        nombres.modelos        <<- c(nombres.modelos, paste0("indices.knn.",kernel))
        IndicesM[[paste0('knnl-',kernel)]] <<- indices.knn

        updateData$selector.comparativa <- actualizar.selector.comparativa()
      },
      error = function(e) { 
        # Regresamos al estado inicial y mostramos un error
        limpia.knn(4)
        showNotification(paste0("Error (KNN-04) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # Actualiza el código a la versión por defecto
  default.codigo.knn <- function(k.def = FALSE) {
    if(!is.null(datos.aprendizaje) & k.def){
      k.value <- ifelse(k.def, round(sqrt(nrow(datos.aprendizaje))), input$kmax.knn)
      updateNumericInput(session,"kmax.knn",value = k.value)
    }else{
      k.value <- input$kmax.knn
    }
    
    kernel <-  isolate(input$kernel.knn)
    codigo <<- code.kkn.modelo(updateData$variable.predecir, input$switch.scale.knn, k.value, kernel = kernel)
    updateAceEditor(session, "fieldCodeKnn", value = codigo)
    cod.knn.modelo <<- codigo
    
    # Se genera el código de la prediccion
    codigo       <- kkn.prediccion(kernel = kernel)
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo)
    cod.knn.pred <<- codigo

    # Se genera el código de la matriz
    codigo       <- knn.MC(kernel = kernel)
    updateAceEditor(session, "fieldCodeKnnMC", value = codigo)
    cod.knn.mc   <<- codigo

    # Se genera el código de la indices
    codigo       <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo)
    cod.knn.ind  <<- codigo
  }

  # Limpia los datos según el proceso donde se genera el error
  limpia.knn <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        exe("modelo.knn.",input$kernel.knn," <<- NULL")
        output$txtknn <- renderPrint(invisible(""))
      }, {
        exe("prediccion.knn.",input$kernel.knn," <<- NULL")
        output$knnPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.knn.",input$kernel.knn," <<- NULL")
        output$plot_knn_mc <- renderPlot(NULL)
        output$txtknnMC    <- renderPrint(invisible(NULL))
      }, {
        IndicesM[[paste0("knnl-",input$kernel.knn)]] <<- NULL
        exe("indices.knn.",input$kernel.knn," <<- NULL")
        output$knnIndPrecTable <-  shiny::renderTable(NULL)
        output$knnIndErrTable  <-  shiny::renderTable(NULL)
        output$knnPrecGlob     <-  flexdashboard::renderGauge(NULL)
        output$knnErrorGlob    <-  flexdashboard::renderGauge(NULL)
      })
    }
  }
  
  # Limpia los datos al ejecutar el botón run
  limpia.knn.run <- function() {
        output$txtknn          <- renderPrint(invisible(""))
        output$knnPrediTable   <- DT::renderDataTable(NULL)
        output$plot_knn_mc     <- renderPlot(NULL)
        output$txtknnMC        <- renderPrint(invisible(NULL))
        output$knnIndPrecTable <- shiny::renderTable(NULL)
        output$knnIndErrTable  <- shiny::renderTable(NULL)
        output$knnPrecGlob     <- flexdashboard::renderGauge(NULL)
        output$knnErrorGlob    <- flexdashboard::renderGauge(NULL)
  }
  
  # Limpia todos los datos
  limpiar <- function(){
        limpia.knn(1)
        limpia.knn(2)
        limpia.knn(3)
        limpia.knn(4)
        updateSelectInput(session,"kernel.knn",selected = "optimal")
  }
  
  #PARA HACERLO CON VALORES REACTIVOS (CUANDO SE HAYA MIGRADO A GOLEM)
  #output$txtknn <- renderPrint(print(modelo.knn()$modelo))
  
  # modelo.knn <- reactive({
  #   input$runknn
  #   datos.prueba      <- updateData$datos.prueba
  #   datos.aprendizaje <- updateData$datos.aprendizaje
  #   var           <- updateData$variable.predecir
  #   kernel        <- isolate(input$kernel.knn)
  #   scale         <- isolate(input$switch.scale.knn)
  #   k             <- isolate(input$kmax.knn)
  #   if(nrow(datos.aprendizaje) == 0) {
  #     return(NULL)
  #   } else {
  #     codigo <<- code.kkn.modelo(var,input$switch.scale.knn,k, kernel = kernel)
  #     updateAceEditor(session, "fieldCodeKnn", value = codigo)
  #     exe(codigo)
  #     modelo  <- exe("modelo.knn.",kernel)
  #     
  #     return(list(modelo = modelo))
  #   }
  # })
  # 
  
}   
  
## To be copied in the UI
# mod_knn_ui("knn_ui_1")
    
## To be copied in the server
# callModule(mod_knn_server, "knn_ui_1")
 
