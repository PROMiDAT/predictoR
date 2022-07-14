#' cross_validation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cross_validation_ui <- function(id){
  ns <- NS(id)
  title_comp <- list(conditionalPanel("input['cross_validation_ui_1-BoxCV'] == 'tabcvcvIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvcv_sel"),label = NULL,
                                                               choices =  "", width = "100%")))))
  tagList(
    tabBoxPrmdt(
      id = ns("BoxCV"), title = title_comp, 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVsvmModelo",
               fluidRow(
                 col_6(selectInput(inputId = ns("predic_var"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%")),
                 col_6(selectInput(inputId = ns("sel_methods"), label = labelInput("selectMod"),
                                   choices =  list("knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr"), width = "100%"))
               ), hr(style = "border-top: 2px solid #cccccc;" ),
               uiOutput(ns('opcModelsCV')),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv"), labelInput("generar"), width  = "100%" ),br(),br(),
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txt_cv")), 
                                                      type = "html", loader = "loader4")),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabcvcvIndices",
               div(col_6(echarts4rOutput(ns("e_cv_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_cv_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabcvcvIndicesCat",
               div(col_12(echarts4rOutput(ns("e_cv_category"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cross_validation Server Functions
#'
#' @noRd 
mod_cross_validation_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    M <- rv(MCs.cv = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(codedioma$idioma, {
      
      nombres <- list( "knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr")
      names(nombres) <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb", "nn", "rl", "rlr"),codedioma$idioma)
      
      updateSelectInput(session, "sel_methods", choices = nombres, selected = input$sel_methods)
    })
    
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      datos    <- updateData$datos
      variable <- updateData$variable.predecir
      M$times  <- 0
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateTextInput(session, "txt_cv", value = ' ' )
        updateSelectInput(session, "cvcv_sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "predic_var", choices = rev(colnames.empty(var.categoricas(updateData$datos))))
        
      }
    })
    
    observeEvent(input$btn_cv, {
      output$txt_cv <- renderPrint({
        tryCatch({
          cant.vc   <- isolate(updateData$numValC)
          datos     <- isolate(updateData$datos)
          numGrupos <- isolate(updateData$numGrupos)
          grupos    <- isolate(updateData$grupos)
          variable  <- isolate(updateData$variable.predecir)
          var_      <- as.formula(paste0(variable, "~."))
          category  <- isolate(levels(updateData$datos[,variable]))
          dim_v     <- isolate(length(category))
          
          MCs.svm          <- list()
          MCs.knn          <- list()
          MCs.arboles      <- list()
          MCs.bosques      <- list()
          MCs.potenciacion <- list()
          MCs.xgboosting      <- list()
          MCs.bayes        <- list()
          MCs.regrLog      <- list()
          MCs.regrLogP     <- list()
          MCs.redes        <- list()
          
          
          for(i in 1:cant.vc){
            MC.svm          <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.knn          <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.arboles      <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.bosques      <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.potenciacion <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.xgboosting      <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.bayes        <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.regrLog      <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.regrLogP     <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.redes        <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            
            for(k in 1:numGrupos) {
              muestra      <- grupos[[i]][[k]] 
              ttesting     <- datos[muestra, ]
              ttraining    <- datos[-muestra, ]
              
              modelo     <- train.svm(var_, data = ttraining, 
                                      kernel = "radial", probability = FALSE)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.svm     <- MC.svm + MC
              
              modelo     <- train.knn(var_, data = ttraining, 
                                      kernel = "inv", probability = FALSE)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.knn     <- MC.knn + MC
              
              modelo     <- train.rpart(var_, data = ttraining)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.arboles <- MC.arboles + MC
              
              modelo     <- train.randomForest(var_, data = ttraining, probability = FALSE)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.bosques <- MC.bosques + MC
              
              modelo     <- train.gbm(var_, data = ttraining, distribution = "multinomial")
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.potenciacion <- MC.potenciacion + MC

              modelo        <- train.xgboost(var_, data = ttraining, nrounds= 79)
              prediccion    <- predict(modelo, ttesting)
              MC            <- confusion.matrix(ttesting, prediccion)
              MC.xgboosting <- MC.xgboosting + MC


              modelo     <- train.bayes(var_, data = ttraining, probability = FALSE)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.bayes   <- MC.bayes + MC

              modelo     <- train.glm(var_, data = ttraining)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.regrLog <- MC.regrLog + MC

              modelo      <- train.glmnet(var_, data = ttraining, distribution = "multinomial")
              prediccion  <- predict(modelo, ttesting)
              MC          <- confusion.matrix(ttesting, prediccion)
              MC.regrLogP <- MC.regrLogP + MC

              modelo     <- train.neuralnet(var_, data = ttraining, hidden = c(8,6,4), 
                                            linear.output = FALSE, threshold = 0.5, stepmax = 1e+06)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.redes   <- MC.redes + MC

            }
            
            MCs.svm[[i]] <- MC.svm
            MCs.knn[[i]] <- MC.knn
            MCs.arboles[[i]] <- MC.arboles
            MCs.bosques[[i]] <- MC.bosques
            MCs.potenciacion[[i]] <- MC.potenciacion
            MCs.xgboosting[[i]]   <- MC.xgboosting
            MCs.bayes[[i]]    <- MC.bayes
            MCs.regrLog[[i]]  <- MC.regrLog
            MCs.regrLogP[[i]] <- MC.regrLogP
            MCs.redes[[i]]    <- MC.redes
          }
          
          MCs.cv <- list(MCs.svm          = MCs.svm,          MCs.knn        = MCs.knn, 
                         MCs.arboles      = MCs.arboles,      MCs.bosques    = MCs.bosques,
                         MCs.potenciacion = MCs.potenciacion, MCs.xgboosting = MCs.xgboosting,
                         MCs.bayes        = MCs.bayes,        MCs.regrLog    = MCs.regrLog,
                         MCs.regrLogP     = MCs.regrLogP,     MCs.redes      = MCs.redes)
          methods <- c("svm", "knn", "arboles", "bosques", "potenciacion", "xgboosting", "bayes", "regrLog", "regrLogP", "redes")

          M$MCs.cv     <- MCs.cv
          resultados   <- indices.cv(category, cant.vc, methods, MCs.cv)
          M$grafico    <- resultados$grafico
          M$global     <- resultados$global
          M$categories <- resultados$categories
          print(MCs.svm)
          
        },error = function(e){
          return(e)
          #return(invisible(''))
        })
        
      })
    })
    
    output$txt_cv <- renderPrint({
      if(M$times == 0){
        M$times <- 1
        return(invisible(''))
      }
      
    })
    
    
    output$e_cv_glob  <-  renderEcharts4r({
      input$btn_cv
      tryCatch({
        grafico <- M$grafico
        if(!is.null(grafico)){
          idioma    <- codedioma$idioma
          resumen.puntos(grafico, labels = c(tr("precG",idioma), unlist(strsplit(tr("generarM",idioma), '[ ]')[[1]][2])))
        }
        else
          return(NULL)
      },error = function(e){
        return(NULL)
      })

    })    
    
    output$e_cv_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      tryCatch({
        if(!is.null(M$grafico)){
          err  <- M$grafico
          err$value <- 1 - M$global
          resumen.puntos(err, labels = c(tr("errG",idioma),  unlist(strsplit(tr("generarM",idioma), '[ ]')[[1]][2])))
          
        }
        else
          return(NULL)
      },error = function(e){
        return(NULL)
      })

    })
    
    
    output$e_cv_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      tryCatch({
        cat    <- input$cvcv_sel
        if(!is.null(M$grafico)){
          graf  <- M$grafico
          graf$value <- M$categories[[cat]]
          resumen.puntos(graf, labels = c(paste0(tr("prec",idioma), " ",cat ),unlist(strsplit(tr("generarM",idioma), '[ ]')[[1]][2])))
          
        }
        else
          return(NULL)
      },error = function(e){
        return(NULL)
      })
      

    })

    # Update Models Options
    output$opcModelsCV = renderUI({
      idioma  <- codedioma$idioma
      modelo  <- input$sel_methods

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
                                                 choices =  list("gini" = "gini", "Entropia" = "information")))))
      opc_bayes <- list(tags$span())

      opc_potenciacion <- list(fluidRow(col_4(numericInput(ns("iter.boosting.pred"), tr("numTree", idioma), 20, width = "100%",min = 1)),
                                        col_4(numericInput(ns("maxdepth.boosting.pred"),tr("maxdepth", idioma), 15, width = "100%",min = 1)),
                                        col_4(numericInput(ns("minsplit.boosting.pred"),tr("minsplit", idioma), 20, width = "100%",min = 1))))
      opc_rl  <- list(tags$span())

      opc_rlr <- list(fluidRow(col_6(selectInput(inputId = ns("alpha.rlr.pred"), label = tr("selectAlg", idioma),selected = 1,
                                                 choices = list("Ridge" = 0, "Lasso" = 1))),
                               col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no"),idioma = idioma )))
      )

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

      if(!is.null(updateData$datos)){
        updateSelectInput(session, "predic_var", choices = rev(colnames.empty(var.categoricas(updateData$datos))))
        updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(updateData$datos))))
        updateNumericInput(session, "mtry.rf.pred",  value = round(sqrt(ncol(updateData$datos) -1)))

      }

      res <-  do.call(tagList, res)

      return(res)
    })

    

}
    
## To be copied in the UI
# mod_cross_validation_ui("cross_validation_1")
    
## To be copied in the server
# mod_cross_validation_server("cross_validation_1")
