#' ind_nuevos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ind_nuevos_ui <- function(id){
  ns <- NS(id)
  muestra.datos.pred  <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred'))), 
                             type = "html", loader = "loader4")  
  
  muestra.datos.pred2 <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred2'))), 
                             type = "html", loader = "loader4")  
  
  muestra.datos.pred4 <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred24'))), 
                             type = "html", loader = "loader4")  
  
  muestra.datos.pred3 <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred3'))), 
                             type = "html", loader = "loader4")

  cod_modelos         <- list(conditionalPanel("input.BoxModelo == 'crearModelo'",
                                      codigo.monokai(ns("fieldPredNuevos"),height = "10vh")),
                              conditionalPanel("input.BoxModelo == 'crearModelo'",
                                      codigo.monokai(ns("fieldCodePredPN"),height = "10vh")))
 
  tabs.modelos   <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(40),
                                tabs.content = list(codigo.monokai(ns("fieldPredNuevos"),height = "10vh")))
  
  tabs.modelos2  <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(40),
                                tabs.content = list(codigo.monokai(ns("fieldCodePredPN"),height = "10vh")))

  tagList(
    div(id = ns("primera"),
        fluidRow(
          col_11(
            tabBoxPrmdt(
            id = "BoxModeloq", 
            tabPanel(
            title = p(labelInput("cargar"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
            collapsible = FALSE, collapsed = FALSE, value = "Cargar",
            fluidRow(
              col_5(
                    checkboxInput(ns('headerNPred'), labelInput("header"), value = T),
                    checkboxInput(ns('rownameNPred'), labelInput("Rownames"), value = T),
                    radioButtons(
                      ns('sepNPred'), labelInput("separador"), inline = T,
                      choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
                    ),
                    radioButtons(ns('decNPred'), labelInput("separadordec"), c(',', '.'), inline = T),
                    radioSwitch(ns("deleteNAnPred"), "eliminana", c("eliminar", "imputar")),
                    fileInput(
                      ns('archivoNPred'), labelInput("cargarchivo"), width = "100%",
                      placeholder = "", buttonLabel = labelInput("subir"),
                      accept = c('text/csv', '.csv', '.txt')), hr(),
                    actionButton(ns("loadButtonNPred"), labelInput("cargar"), width = "100%"), hr()),
              col_7(br(),muestra.datos.pred)))
            )),
          col_1(
            actionButton(ns("cargarnext"), labelInput("siguiente"), width = "100%",
                         icon = icon("forward"))
          )
        )
      ),
    div(id = ns("segundo"),
        style = "display:none",
        fluidRow(
          col_1(actionButton (ns("transback"), labelInput("atras"), width = "100%",
                              icon = icon("backward"))),
          col_10(
            tabBoxPrmdt(
            id = "BoxModeloe",
            tabPanel(
              title = p(labelInput("trans"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
              collapsible = FALSE, collapsed = FALSE, value = "Trasformar",
              fluidRow(
                col_5(
                      uiOutput(ns('transDataPredN')), hr(), 
                      actionButton(ns('transButton'), labelInput("aplicar"), width = "100%"), hr()
                ),
                col_7(br(),muestra.datos.pred2)), hr())
            )),
          col_1(actionButton(ns("transnext"), labelInput("siguiente"), width = "100%",
                             icon = icon("forward")))
        )
    ),
    div(id = ns("tercera"),
        style = "display:none",
        fluidRow(col_1(actionButton(ns("modelback"), labelInput("atras"), width = "100%",
                                          icon = icon("backward"))),
                 col_10(
                   tabBoxPrmdt(
                     id = "BoxModeloa",
                     tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag") ,solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
                              list(
                                   selectInput(inputId = ns("sel.predic.var.nuevos"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                                   radioGroupButtons(ns("selectModelsPred"), labelInput("selectMod"), list("<span data-id=\"knnl\"></span>" = "knn",
                                                                                                           "<span data-id=\"dtl\"></span>" = "dt",
                                                                                                           "<span data-id=\"rfl\"></span>" = "rf",
                                                                                                           "<span data-id=\"bl\"></span>" = "ada",
                                                                                                           "<span data-id=\"svml\"></span>" = "svm",
                                                                                                           "Bayes" = "bayes",
                                                                                                           "<span data-id=\"xgb\"></span>" = "xgb",
                                                                                                           "<span data-id=\"nn\"></span>" = "nn",
                                                                                                           "<span data-id=\"rl\"></span>" = "rl",
                                                                                                           "<span data-id=\"rlr\"></span>" = "rlr"),
                                                     size = "sm", status = "primary",individual = FALSE, justified = FALSE, selected = "knn",
                                                     checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                      no = icon("remove", lib = "glyphicon")), width = "100%")),
                              uiOutput(ns('opcModelsPredN')),
                              withLoader(verbatimTextOutput(ns("txtPredNuevos")), 
                                         type = "html", loader = "loader4"),
                              
                              actionButton(ns("PredNuevosBttnModelo"), labelInput("generarM"), width  = "100%" ),br()),
                     conditionalPanel("input.BoxModelo == 'predicModelo'",tabs.modelos)

                 )),
                 col_1(actionButton(ns("modelnext"), labelInput("siguiente"), width = "100%",
                                    icon = icon("forward")))
        )),
    div(id = ns("cuarta"),
        style = "display:none",
        fluidRow(col_1(actionButton (ns("nuevosback"), labelInput("atras"), width = "100%",
                                     icon = icon("backward"))),
                 col_10(
                   tabBoxPrmdt(
                     id = "BoxModelor",
                     tabPanel(
                       title = p(labelInput("cargarNuev"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
                       collapsible = FALSE, collapsed = FALSE,value = "CargarNuevos",
                       fluidRow(
                         col_5(checkboxInput(ns('headerNPred2'), labelInput("header"), value = T),
                               checkboxInput(ns('rownameNPred2'), labelInput("Rownames"), value = T),
                               radioButtons(
                                 ns('sepNPred2'), labelInput("separador"), inline = T,
                                 choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
                               ),
                               radioButtons(ns('decNPred2'), labelInput("separadordec"), c(',', '.'), inline = T),
                               radioSwitch(ns("deleteNAnPred2"), "eliminana", c("eliminar", "imputar")),
                               fileInput(
                                 ns('archivoNPred2'), labelInput("cargarchivo"), width = "100%",
                                 placeholder = "", buttonLabel = labelInput("subir"),
                                 accept = c('text/csv', '.csv', '.txt')), hr(),
                               actionButton(ns("loadButtonNPred2"), labelInput("cargar"), width = "100%"), hr()),
                         col_7(br(), muestra.datos.pred3)),br())
                   )),
                 col_1(actionButton(ns("nuevosnext"), labelInput("siguiente"), width = "100%",
                                    icon = icon("forward")))
        )
    ),
    div(id = ns("quinta"),
        style = "display:none",
        fluidRow(col_1(actionButton (ns("predicback"), labelInput("atras"), width = "100%",
                                     icon = icon("backward"))),
                 col_10(
                   tabBoxPrmdt(
                     id = "BoxModelo",
                     tabPanel(title = p(labelInput("predicnuevos"),class = "wrapper-tag"), value = "predicModelo",
                              DT::dataTableOutput(ns("PrediTablePN")),
                              hr(),
                              downloadButton(ns("downloaDatosPred"), labelInput("descargar"), style = "width:100%;"),
                              actionButton(ns("predecirPromidat"), "preditc"),  br()),
                     conditionalPanel("input.BoxModelo == 'predicModelo'",tabs.modelos2)
                   ))
        ))
  )
}

#' ind_nuevos Server Function
#'
mod_ind_nuevos_server <- function(input, output, session, updateData, newCases){
  ns <- session$ns
  
  #' Load Button Function
  observeEvent(input$loadButtonNPred, {
    rowname    <- isolate(input$rownameNPred)
    ruta       <- isolate(input$archivoNPred)
    sep        <- isolate(input$sepNPred)
    dec        <- isolate(input$decNPred)
    encabezado <- isolate(input$headerNPred)
    deleteNA   <- isolate(input$deleteNAnPred)
    
    # menu.values <- c(" a[data-value=crearModelo]", " a[data-value=CargarNuevos]", " a[data-value=predicModelo]")
    # mostrar.tabs(FALSE, menu.values) 
    # 
    tryCatch({
      codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      newCases$variable.predecir <- NULL
      
      newCases$originales <- carga.datos(
        rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      
      if(ncol(newCases$originales) <= 1) {
        showNotification(
          "ERROR: Check Separators", duration = 10, type = "error")
        borrar.datos(newCases)
        
      } else {
        newCases$datos.aprendizaje <- newCases$originales
        newCases$datos.prueba <- NULL
        
      }
    }, error = function(e) {
      newCases$datos.aprendizaje <- NULL
      
      borrar.datos(newCases)
      showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
    })
    asignarDatos(newCases)
    borrar.datos.modelos.np()
    actualizar.texto.modelo.pn("")
    actualizar.pred.pn("")
  })
  
  #' Load Button Function
  observeEvent(input$loadButtonNPred2, {
    rowname    <- isolate(input$rownameNPred2)
    ruta       <- isolate(input$archivoNPred2)
    sep        <- isolate(input$sepNPred2)
    dec        <- isolate(input$decNPred2)
    encabezado <- isolate(input$headerNPred2)
    deleteNA   <- isolate(input$deleteNAnPred2)
    if(!is.null(newCases$variable.predecir)){
    tryCatch({
      codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      

      newCases$datos.prueba <- carga.datos.np(rowname, 
                                              ruta$datapath, 
                                              sep, 
                                              dec, 
                                              encabezado)
      asignarDatos(newCases)
      verificar.datos.pn()
      
      datos.prueba.completos[,newCases$variable.predecir] <<- NULL
      datos.prueba.completos <<- accion.NAs(datos.prueba.completos, deleteNA)
      datos.prueba.completos[,newCases$variable.predecir] <<- NA
      validar()
      unificar.factores()
      
      newCases$datos.prueba <- datos.prueba.completos
      
      # code.trans.pn <<- gsub("datos.originales.completos", "datos.prueba.completos", code.trans.pn)
      # code.trans.pn <<- gsub("datos.aprendizaje.completos", "datos.prueba.completos", code.trans.pn)
      # exe(code.trans.pn)
      
      if(ncol(newCases$datos.prueba) <= 1) {
        showNotification(
          "ERROR: Check Separators", duration = 10, type = "error")
        borrar.datos(newCases,  prueba = TRUE)
        
      } else {
        newCases$datos <- newCases$datos.prueba
        menu.values <- c( " a[data-value=predicModelo]")
        mostrar.tabs(TRUE, menu.values) 
      }
    }, error = function(e) {
      borrar.datos(newCases,  prueba = TRUE)
      datos.prueba.completos <<- NULL
      predic.nuevos <<- NULL
      showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
    })
  }
    else {
      showNotification(
        paste0("Error :", tr("ErrorModelo", updateData$idioma)), duration = 10, type = "error")
      borrar.datos(newCases,  prueba = TRUE)
      
    }
  })
  
  output$contentsPred <- DT::renderDataTable({
    
    datos  <- newCases$datos.aprendizaje
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    if(is.null(datos)){
      output$txtPredNuevos <- renderPrint(invisible(NULL))
      menu.values <- c( " a[data-value=Trasformar]", " a[data-value=crearModelo]", " a[data-value=CargarNuevos]", " a[data-value=predicModelo]")
      mostrar.tabs(FALSE, menu.values) 
    }

    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas   <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)  
  
  
  output$contentsPred2 <- DT::renderDataTable({
    datos  <- newCases$datos.aprendizaje
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas   <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  output$contentsPred3 <- DT::renderDataTable({
    datos  <- newCases$datos.prueba
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas   <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  #' Update Transform Table
  output$transDataPredN = renderUI({
    datos  <- newCases$originales
    idioma <- updateData$idioma
    
    res <- list(fluidRow(
      column(4, tags$span(tags$b("Variable"))),
      column(5, tags$b(tr("tipo", idioma))),
      column(3, tags$b(tr("activa", idioma))),
    ), hr(style = paste0("margin-top: 10px; margin-bottom: 10px;", 
                         "border-top: 1px solid black;")))
    
    if(!is.null(datos) && ncol(datos) > 0) {
      res <- list(res, lapply(colnames(datos), function(x) {
        list(fluidRow(
          column(4, tags$span(x)),
          column(5, selectInputTrans(datos, x, idioma)),
          column(3, tags$input(type = "checkbox", id = ns(paste0("del", x)), 
                               checked = T))
        ), hr(style = "margin-top: 10px; margin-bottom: 10px"))
      }))
    }
    
    res <- tags$div(
      style = "height: 40vh; overflow-y: scroll;",
      do.call(tagList, res)
    )
    return(res)
  })

  
  #' Transform Button Function
  observeEvent(input$transButton, {
    datos <- newCases$originales
    cod = ""
    borrar.datos(newCases,  prueba = TRUE)
    newCases$variable.predecir <- NULL
    borrar.datos.modelos.np()
    datos.prueba.completos <<- NULL
    actualizar.pred.pn("")
    actualizar.texto.modelo.pn("")
    
    for (var in colnames(datos)) {
      if(!input[[paste0("del", var)]]) {
        datos[, var] <- NULL
        cod <- paste0(cod, "datos[['", var, "']] <- NULL\n")
        
      } else {
        if(input[[paste0("sel", var)]] == "categorico" &
           class(datos[, var]) %in% c("numeric","integer")) {
          datos[, var] <- as.factor(datos[, var])
          cod <- paste0(cod, code.trans(var, "categorico"))
        }
        
        if(input[[paste0("sel", var)]] == "numerico" &
           !(class(datos[, var]) %in% c("numeric","integer"))) {
          datos[, var] <- as.numeric(datos[, var])
          cod <- paste0(cod, code.trans(var, "numerico"))
        }
        if(input[[paste0("sel", var)]] == "disyuntivo") {
          datos <- datos.disyuntivos(datos, var)
          datos[, var] <- NULL
          cod <- paste0(cod, code.trans(var, "disyuntivo"))
        }
      }
    }
    newCases$datos.aprendizaje  <- datos
    datos.aprendizaje.completos <<- newCases$datos.aprendizaje
    menu.values <- c( " a[data-value=CargarNuevos]", " a[data-value=predicModelo]")
    mostrar.tabs(FALSE, menu.values) 
  }) 
  
  selectInputTrans <- function(datos, var, idioma = "es") {
    tags$select(
      id = ns(paste0("sel", var)),
      tags$option(value = "categorico", tr("categorico", idioma)),
      if(class(datos[, var]) %in% c("numeric", "integer")) {
        tags$option(value = "numerico", tr("numerico", idioma), 
                    selected = 'selected')
      } else {
        tags$option(value = "numerico", tr("numerico", idioma))
      },
      tags$option(value = "disyuntivo", tr("disyuntivo", idioma))
    )
  }
  
  validar <- function() {
    cod        <- ""
    originales <-  newCases$originales
    datos      <- datos.prueba.completos
    
    tryCatch(
      
    for (var in colnames(originales)) {
      if(!input[[paste0("del", var)]]) {
        datos[, var] <- NULL
        cod <- paste0(cod, "datos[['", var, "']] <- NULL\n")
        
      } else {
        if(input[[paste0("sel", var)]] == "categorico" &
           class(datos[, var]) %in% c("numeric","integer")) {
          datos[, var] <- as.factor(datos[, var])
          cod <- paste0(cod, code.trans(var, "categorico"))
        }
        
        if(input[[paste0("sel", var)]] == "numerico" &
           !(class(datos[, var]) %in% c("numeric","integer"))) {
          datos[, var] <- as.numeric(datos[, var])
          cod <- paste0(cod, code.trans(var, "numerico"))
        }
        if(input[[paste0("sel", var)]] == "disyuntivo") {
          datos <- datos.disyuntivos(datos, var)
          datos[, var] <- NULL
          cod <- paste0(cod, code.trans(var, "disyuntivo"))
        }
      }
    }
    )
    datos.prueba.completos <<- datos 
    
  }
  
  observeEvent(c(input$cant.capas.nn.pred), {
    if(!is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  })
  
  observeEvent(newCases$datos.aprendizaje, {
    if(!is.null(newCases$datos.aprendizaje)){
      shinyjs::show("cargarnext")
    }
    else{
      shinyjs::hide("cargarnext")
    }
  },ignoreNULL = FALSE)
  
  observeEvent(input$cargarnext, {
    shinyjs::hide("primera", anim = TRUE)
    shinyjs::show("segundo", anim = TRUE)
  })

  observeEvent(input$transback, {
    shinyjs::show("primera", anim = TRUE)
    shinyjs::hide("segundo", anim = TRUE)
  })
  
  observeEvent(input$transnext, {
    shinyjs::show("tercera", anim = TRUE)
    shinyjs::hide("segundo", anim = TRUE)
  })
  
  observeEvent(input$modelback, {
    shinyjs::show("segundo", anim = TRUE)
    shinyjs::hide("tercera", anim = TRUE)
  })
  
  observeEvent(input$modelnext, {
    shinyjs::show("cuarta", anim = TRUE)
    shinyjs::hide("tercera", anim = TRUE)
  })
  
  observeEvent(input$nuevosback, {
    shinyjs::hide("cuarta",  anim = TRUE)
    shinyjs::show("tercera", anim = TRUE)
  })
  
  observeEvent(input$nuevosnext, {
    shinyjs::hide("cuarta", anim = TRUE)
    shinyjs::show("quinta", anim = TRUE)
  })  
  
  observeEvent(input$predicback, {
    shinyjs::show("cuarta", anim = TRUE)
    shinyjs::hide("quinta", anim = TRUE)
  })
  
  
  
  observeEvent(input$PredNuevosBttnModelo,{
    crear.modelo()
  })
  
  crear.modelo <- function(){
    if(!is.null(newCases$datos.aprendizaje)){
      variable.predecir.np       <- input$sel.predic.var.nuevos
      newCases$variable.predecir <- input$sel.predic.var.nuevos
      codigo <- switch (input$selectModelsPred ,
                        knn   = kkn.modelo.np(variable.pr = newCases$variable.predecir,
                                              scale = input$switch.scale.knn.pred,
                                              kmax = input$kmax.knn.pred,
                                              kernel = input$kernel.knn.pred),
                        dt    = dt.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                             minsplit = input$minsplit.dt.pred,
                                             maxdepth = input$maxdepth.dt.pred,
                                             split = input$split.dt.pred),
                        rf    = rf.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                             ntree = input$ntree.rf.pred,
                                             mtry = input$mtry.rf.pred),
                        svm   = svm.modelo.np(variable.pr =input$sel.predic.var.nuevos,
                                              scale = input$switch.scale.svm.pred,
                                              kernel = input$kernel.svm.pred),
                        bayes = bayes.modelo.np(variable.pr=input$sel.predic.var.nuevos),
                        xgb   = xgb.modelo.np(variable.pr=input$sel.predic.var.nuevos,
                                              booster = input$boosterXgb.pred,
                                              max.depth = input$maxdepthXgb.pred,
                                              n.rounds = input$nroundsXgb.pred),
                        rl    = rl.modelo.np(variable.pr=input$sel.predic.var.nuevos),
                        nn    = nn.modelo.np(variable.pr=input$sel.predic.var.nuevos,
                                             input$threshold.nn.pred,
                                             input$stepmax.nn.pred,
                                             input$cant.capas.nn.pred,
                                             input$nn.cap.pred.1,input$nn.cap.pred.2,
                                             input$nn.cap.pred.3,input$nn.cap.pred.4,
                                             input$nn.cap.pred.5,input$nn.cap.pred.6,
                                             input$nn.cap.pred.7,input$nn.cap.pred.8,
                                             input$nn.cap.pred.9,input$nn.cap.pred.10),
                        rlr    = rlr.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                               input$alpha.rlr.pred,
                                               input$switch.scale.rlr.pred),
                        ada    = boosting.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                                    iter        = input$iter.boosting.pred,
                                                    maxdepth    = input$maxdepth.boosting.pred,
                                                    minsplit    = input$minsplit.boosting.pred)
      )
      borrar.datos.modelos.np()
      borrar.datos(newCases,  prueba = TRUE)
      actualizar.pred.pn("")
      modelo.seleccionado.pn  <<- input$selectModelsPred
      datos.prueba.completos      <<- NULL
      
      tryCatch({
        if(input$selectModelsPred == "rl")
          if(num.categorias.pred.np(newCases$variable.predecir) != 2)
            stop(tr("limitModel", updateData$idioma), call. = FALSE)
        
        exe(codigo)
        actualizar.texto.modelo.pn(codigo)
        mostrar.tabs(TRUE,  c(" a[data-value=CargarNuevos]")) 
        mostrar.tabs(FALSE, c(" a[data-value=predicModelo]")) 
      },
      error =  function(e){
        showNotification(paste0(e), duration = 10, type = "error")
      },
      warning = function(w){
        if(input$selectModelsPred == "nn"){
          showNotification(paste0(tr("nnWar", updateData$idioma)," (NN-01) : ",w), duration = 20, type = "warning")
        }
        if(input$selectModelsPred == "rl"){
          exe(codigo)
          actualizar.texto.modelo.pn(codigo) 
          mostrar.tabs(TRUE, c( " a[data-value=CargarNuevos]"))           

        }
      })
    }else{
      showNotification(paste0(tr("nodata", updateData$idioma)), duration = 20, type = "error")
    }
  }
  
  observeEvent(input$predecirPromidat, {
    tryCatch({
      predecir.pn()
    },
    error =  function(e){
      showNotification(paste0("Error :", tr("ErrorDatosPN", updateData$idioma)), duration = 10, type = "error")
    })
    
  })  
  
  observeEvent(input$transback2, {

    tryCatch({
      if(!is.null(newCases$datos.aprendizaje)){
        menu.values <- c( " a[data-value=crearModelo]")
        mostrar.tabs(TRUE, menu.values) 
      }
      
    }, error = function(e) {
      showNotification(paste0("ERROR debe cargar los datos: ", e), type = "error")
    })
    
  })
  
  
  output$downloaDatosPred <- downloadHandler(
    filename = function() {
      input$archivoNPred2$name
    },
    content = function(file) {
      if(!is.null(predic.nuevos)){
        write.csv(crear.datos.np(), file, row.names = input$rownameNPred2)
      }
    }
  )
  
  predecir.pn <-function(){
    if(!is.null(datos.prueba.completos)){
      if(exists("modelo.nuevos") && !is.null(modelo.nuevos)){
        codigo <- switch(modelo.seleccionado.pn,
                         knn   = kkn.prediccion.np(),
                         dt    = dt.prediccion.np(),
                         rf    = rf.prediccion.np(),
                         ada   = boosting.prediccion.np(),
                         svm   = svm.prediccion.np(),
                         bayes = bayes.prediccion.np(),
                         xgb   = xgb.prediccion.np(),
                         nn    = nn.prediccion.np(),
                         rl    = rl.prediccion.np(),
                         rlr   = rlr.prediccion.np())
        
        tryCatch({
          exe(codigo)
          actualizar.pred.pn(codigo)
        },
        error =  function(e){
          showNotification(paste0("Error :", e), duration = 10, type = "error")
        })
      }else{
        showNotification(paste0("Error :", tr("ErrorModelo", updateData$idioma)), duration = 10, type = "error")
      }
    }else{
      showNotification(paste0("Error :", tr("ErrorDatosPN", updateData$idioma)), duration = 10, type = "error")
    }
  }
  
  mostrar.tabs <- function(mostrar = FALSE, menu.values){
    
    # element <- "#BoxModelo li"
    # lapply(menu.values, function(i){
    #   if(mostrar) {
    #     shinyjs::enable(selector = paste0(element, i))
    #     #shinyjs::show(selector = paste0(element, i))
    #     
    #   } else {
    #     shinyjs::disable(selector = paste0(element, i))
    #     shinyjs::hide(selector = paste0(element, i))
    #     
    #   }
    # })
  }
  
  
  actualizar.texto.modelo.pn <- function(codigo){
    updateAceEditor(session, "fieldPredNuevos", value = codigo)
    if(is.null(modelo.nuevos)){
      output$txtPredNuevos <- renderPrint(invisible(NULL))
    }else{
      output$txtPredNuevos <- renderPrint(print(modelo.nuevos))
    }
  }
  
  actualizar.pred.pn <- function(codigo){
    updateAceEditor(session, "fieldCodePredPN", value = codigo)
    if(!is.null(predic.nuevos) & !is.null(newCases$datos.prueba)){
      datos.aux.prueba <- crear.datos.np()
      actualizar.tabla.predic.np(datos.aux.prueba)
      }else{
      actualizar.tabla.predic.np(data.frame())
      }
  }
  
 actualizar.tabla.predic.np <- function(datos) {
   output$PrediTablePN <- DT::renderDataTable({
     tipos  <- c(
       tr("numerico",   isolate(updateData$idioma)),
       tr("categorico", isolate(updateData$idioma))
     )
     tryCatch({
       nombre.columnas <- c("ID", colnames(datos))
       tipo.columnas <- sapply(colnames(datos), function(i)
         ifelse(class(datos[,i]) %in% c("numeric", "integer"),
                paste0("<span data-id='numerico'>", tipos[1], "</span>"),
                paste0("<span data-id='categorico'>", tipos[2], "</span>")))
       sketch = htmltools::withTags(table(
         tableHeader(nombre.columnas),
         tags$tfoot(
           tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
             tags$th(shiny::HTML(i))))
         )
       ))
       DT::datatable(
         datos, selection = 'none', editable = TRUE,  container = sketch,
         options = list(dom = 'frtip', scrollY = "40vh")
       )
     }, error = function(e) {
       showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
       return(NULL)
     })
   }, server = T)
 }
 
  verificar.datos.pn <- function(){
    if(any(!(c(colnames(datos.prueba.completos),newCases$variable.predecir) %in% colnames(datos.originales.completos))))
      stop(tr("NoTamColum", updateData$idioma))
  }
  
  crear.datos.np <- function(){
    datos.aux.prueba <- datos.prueba.completos
    datos.aux.prueba[,newCases$variable.predecir]   <- predic.nuevos$prediction
    
    return(datos.aux.prueba)
  }
  
  unificar.factores <- function(){
    for(nombre in colnames(datos.prueba.completos)){
      if(class(datos.prueba.completos[,nombre]) == "factor"){
        levels(datos.prueba.completos[,nombre]) <<- unique(c(levels(datos.prueba.completos[,nombre]),
                                                             levels(datos.aprendizaje.completos[,nombre])))
      }
    }
  }
  
  # Habilitada o deshabilita la semilla RLR
  observeEvent(input$permitir.landa.pred, {
    if (input$permitir.landa.pred) {
      shinyjs::enable("landa.pred")
    } else {
      shinyjs::disable("landa.pred")
    }
  })
  
  # Habilitada o deshabilita la semilla RLR
  observeEvent(newCases$datos.aprendizaje, {

    tryCatch({
        if(!is.null(newCases$datos.aprendizaje)){
          menu.values <- c( " a[data-value=Trasformar]")
          mostrar.tabs(TRUE, menu.values) 
        }
      
    }, error = function(e) {
      showNotification(paste0("ERROR debe cargar los datos: ", e), type = "error")
    })

  })
  
  #' Update Models Opcions
  output$opcModelsPredN = renderUI({
    idioma  <- updateData$idioma
    modelo  <- input$selectModelsPred 
    
    opc_knn <- list(fluidRow(col_4(numericInput(ns("kmax.knn.pred"), tr("kmax", idioma), min = 1,step = 1, value = 7)),
                             col_4(selectInput(inputId = ns("kernel.knn.pred"), label = tr("selkernel", idioma),selected = 1,
                                           choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                       "triweight", "cos","inv","gaussian"))),
                             col_4(radioSwitchNP(ns("switch.scale.knn.pred"), "escal", c("si", "no"),idioma = idioma ))))
    
    opc_svm <- list(fluidRow(col_6(
                                  radioSwitchNP(ns("switch.scale.svm.pred"), "escal", c("si", "no"),idioma = idioma )),
                             col_6(selectInput(inputId = ns("kernel.svm.pred"), label = tr("selkernel", idioma),selected = "radial",
                                          choices = c("linear", "polynomial", "radial", "sigmoid")))))
    
    opc_rf  <- list(fluidRow(col_6(numericInput(ns("ntree.rf.pred"), tr("numTree", idioma), 20, width = "100%", min = 0)),
                             col_6(numericInput(ns("mtry.rf.pred"),  tr("numVars", idioma),1, width = "100%", min = 1))))
    
    opc_dt  <- list(fluidRow(col_4(numericInput(ns("minsplit.dt.pred"), tr("minsplit", idioma), 20, width = "100%",min = 1)),
                             col_4(numericInput(ns("maxdepth.dt.pred"), tr("maxdepth", idioma), 15, width = "100%",min = 0, max = 30, step = 1)),
                             col_4(selectInput(inputId = ns("split.dt.pred"), label = tr("splitIndex", idioma),selected = 1,
                                         choices =  list("gini" = "gini", "Entropía" = "information")))))
    opc_bayes <- list(tags$span())
    
    opc_potenciacion <- list(fluidRow(col_4(numericInput(ns("iter.boosting.pred"), tr("numTree", idioma), 50, width = "100%",min = 1)),
                                      col_4(numericInput(ns("maxdepth.boosting.pred"),tr("maxdepth", idioma), 15, width = "100%",min = 1)),
                                      col_4(numericInput(ns("minsplit.boosting.pred"),tr("minsplit", idioma), 20, width = "100%",min = 1))))
    opc_rl  <- list(tags$span())
   
    opc_rlr <- list(fluidRow(col_6(selectInput(inputId = ns("alpha.rlr.pred"), label = tr("selectAlg", idioma),selected = 1,
                                  choices = list("Ridge" = 0, "Lasso" = 1))),
                             col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no"),idioma = idioma ))),
                    fluidRow(col_6(id = ns("colManualLanda"),br(),
                                   numericInput(ns("landa.pred"), tr("landa", idioma),value = 2, min = 0, "NULL", width = "100%")), br(),
                             col_6(radioSwitchNP(ns("permitir.landa.pred"), "", c("manual", "automatico"),idioma = idioma ))))
  
    opc_xgb <- list(fluidRow(col_4(selectInput(inputId = ns("boosterXgb.pred"), label = tr("selbooster", idioma), selected = 1,
                                               choices = c("gbtree", "gblinear", "dart"))),
                             col_4(numericInput(ns("maxdepthXgb.pred"), tr("maxdepth", idioma),  min = 1,  step = 1, value = 6)),
                             col_4(numericInput(ns("nroundsXgb.pred"),  tr("selnrounds", idioma), min = 0, step = 1, value = 50))))
  
    opc_nn <- list(fluidRow(col_4(numericInput(ns("threshold.nn.pred"),tr("threshold", idioma),
                                               min = 0,   step = 0.01, value = 0.05)),
                            col_4(numericInput(ns("stepmax.nn.pred"),tr("stepmax", idioma),
                                               min = 100, step = 100,  value = 5000)),
                            col_4(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                              label = tr("selectCapas", idioma), value = 10))),
                   fluidRow(id = ns("capasFila"),lapply(1:10, function(i) tags$span(
                            col_2(numericInput(ns(paste0("nn.cap.pred.",i)), NULL, min = 1, step = 1, value = 2),
                                               class = "mini-numeric-select")))))

    res <-  switch(modelo,
                   knn   =  opc_knn,
                   svm   =  opc_svm,
                   rf    =  opc_rf,
                   bayes =  opc_bayes,
                   nn    =  opc_nn,
                   ada   =  opc_potenciacion,
                   xgb   =  opc_xgb,
                   rl    =  opc_rl,
                   rlr   =  opc_rlr,
                   dt    =  opc_dt)
    
    if(!is.null(newCases$datos.aprendizaje)){
      #output$txtPredNuevos <- renderPrint(invisible(NULL))
      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.categoricas(newCases$datos.aprendizaje))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(newCases$datos.aprendizaje))))
      updateNumericInput(session, "mtry.rf.pred",  value = round(sqrt(ncol(newCases$datos.aprendizaje) -1)))
      
    }
  
    res <-  do.call(tagList, res)

    return(res)
  })
}
    
## To be copied in the UI
# mod_ind_nuevos_ui("ind_nuevos_ui_1")
    
## To be copied in the server
# callModule(mod_ind_nuevos_server, "ind_nuevos_ui_1", updateData)
 
