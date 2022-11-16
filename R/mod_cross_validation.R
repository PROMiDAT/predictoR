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
                                                               choices =  "", width = "100%")))),
                     conditionalPanel("input['cross_validation_ui_1-BoxCV'] == 'tabcvcvIndices3'",
                                      div(id = ns("row2"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                               choices = "")))))
  
  opc_knn <- list(fluidRow(col_4(numericInput(ns("kmax.knn"), labelInput("kmax"), min = 1,step = 1, value = 7)),
                           col_4(selectInput(inputId = ns("kernel.knn.pred"), label = labelInput("selkernel"),selected = 1,
                                             choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                         "triweight", "cos","inv","gaussian"))),
                           col_4(radioSwitchNP(ns("switch.scale.knn.pred"), "escal", c("si", "no") ))))
  
  opc_svm <- list(fluidRow(col_6(radioSwitchNP(ns("switch.scale.svm.pred"), "escal", c("si", "no"))),
                           col_6(selectInput(inputId = ns("kernel.svm.pred"), label = labelInput("selkernel"),selected = "radial",
                                 choices = c("linear", "polynomial", "radial", "sigmoid")))))
  
  opc_rf  <- list(fluidRow(col_6(numericInput(ns("ntree.rf.pred"), labelInput("numTree"), 20, width = "100%", min = 0)),
                           col_6(numericInput(ns("mtry.rf.pred"),  labelInput("numVars"),1, width = "100%", min = 1))))
  
  opc_dt  <- list(fluidRow(col_4(numericInput(ns("minsplit.dt.pred"), labelInput("minsplit"), 20, width = "100%",min = 1)),
                           col_4(numericInput(ns("maxdepth.dt.pred"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)),
                           col_4(selectInput(inputId = ns("split.dt.pred"), label = labelInput("splitIndex"),selected = 1,
                                             choices =  list("gini" = "gini", "Entropia" = "information")))))
  opc_bayes <- list(tags$span())
  
  opc_potenciacion <- list(fluidRow(col_6(numericInput(ns("iter.boosting.pred"), labelInput("numTree"), 20, width = "100%",min = 1)),
                                    col_6(numericInput(ns("maxdepth.boosting.pred"),labelInput("maxdepth"), 15, width = "100%",min = 1)),
                                    col_6(numericInput(ns("minsplit.boosting.pred"),labelInput("minsplit"), 20, width = "100%",min = 1)),
                                    col_6(selectInput(inputId = ns("coeflearn"), label = labelInput("selkernel"), selected = 1,
                                                      choices = c("Breiman", "Freund", "Zhu")))))
  opc_rl  <- list(tags$span())
  
  opc_rlr <- list(fluidRow(col_6(selectInput(inputId = ns("alpha.rlr.pred"), label = labelInput("selectAlg"),selected = 1,
                                             choices = list("Ridge" = 0, "Lasso" = 1))),
                           col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no") )))
  )
  
  opc_xgb <- list(fluidRow(col_4(selectInput(inputId = ns("boosterXgb.pred"), label = labelInput("selbooster"), selected = 1,
                                             choices = c("gbtree", "gblinear", "dart"))),
                           col_4(numericInput(ns("maxdepthXgb"), labelInput("maxdepth"),  min = 1,  step = 1, value = 6)),
                           col_4(numericInput(ns("nroundsXgb"),  labelInput("selnrounds"), min = 0, step = 1, value = 50))))
  
  opc_nn <- list(fluidRow(col_4(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                             min = 0,   step = 0.01, value = 0.05)),
                          col_4(numericInput(ns("stepmax_nn"),labelInput("stepmax"),
                                             min = 100, step = 100,  value = 10000)),
                          col_4(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                            label = labelInput("selectCapas"), value = 3))),
                 fluidRow(id = ns("capasFila"),lapply(1:10, function(i) tags$span(
                          col_2(numericInput(ns(paste0("nn.cap.pred.",i)), NULL, min = 1, step = 1, value = 3),
                          class = "mini-numeric-select")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxCV"), title = title_comp, 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVsvmModelo",
               fluidRow(
                 col_6(selectInput(inputId = ns("predic_var"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%")),
                 col_6(selectInput(inputId = ns("sel_methods"), label = labelInput("selectMod"),
                                   choices =  list("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb", "rl", "rlr"), width = "100%"))
               ), hr(style = "border-top: 2px solid #cccccc;" ),
               conditionalPanel(condition =  "input.sel_methods == 'knnl'",
                                opc_knn, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'svml'",
                                opc_svm, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'dtl'",
                                opc_dt, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rfl'",
                                opc_rf, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'xgb'",
                                opc_xgb, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'bl'",
                                opc_potenciacion, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'Bayes'",
                                opc_bayes, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'nn'",
                                opc_nn, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rl'",
                                opc_rl, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rlr'",
                                opc_rlr, ns = ns),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv"), labelInput("generar"), width  = "100%" ),br(),br(),
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txt_cv")), 
                                                      type = "html", loader = "loader4")),br(),br()),
      # tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabcvcvIndices",
      #          div(col_6(echarts4rOutput(ns("e_cv_glob"), width = "100%", height = "70vh")),
      #              col_6(echarts4rOutput(ns("e_cv_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabcvcvIndices3",
               div(col_12(echarts4rOutput(ns("e_cv_precision"), width = "100%", height = "70vh")))),
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
      
      nombres <- list("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb" , "rl", "rlr")
      names(nombres) <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb" , "rl", "rlr"),codedioma$idioma)
      precision <- list(0, 1)
      names(precision) <- tr(c("errG", "precG"),codedioma$idioma)
      
      updateSelectInput(session, "cvcv_glo", choices = precision, selected = 1)
      updateSelectInput(session, "sel_methods", choices = nombres, selected = input$sel_methods)
    })

    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      datos    <- updateData$datos
      variable <- updateData$variable.predecir
      M$MCs.cv  <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      M$times      <- 0
      defaul_param_values()
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateTextInput(session, "txt_cv", value = ' ' )
        updateSelectInput(session, "cvcv_sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "predic_var", choices = rev(colnames.empty(var.categoricas(updateData$datos))))
        
      }
      
      output$txt_cv <- renderPrint({
        return(invisible(''))
        
        
      })
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
          params    <- listar_parametros()
          
          MCs.svm          <- vector(mode = "list", length = cant.vc)
          MCs.knn          <- vector(mode = "list", length = cant.vc)
          MCs.arboles      <- vector(mode = "list", length = cant.vc)
          MCs.bosques      <- vector(mode = "list", length = cant.vc)
          MCs.potenciacion <- vector(mode = "list", length = cant.vc)
          MCs.xgboosting   <- vector(mode = "list", length = cant.vc)
          MCs.bayes        <- vector(mode = "list", length = cant.vc)
          MCs.regrLog      <- vector(mode = "list", length = cant.vc)
          MCs.regrLogP     <- vector(mode = "list", length = cant.vc)
          #MCs.redes        <- vector(mode = "list", length = cant.vc)
          
          
          for(i in 1:cant.vc){
            MC.svm          <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.knn          <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.arboles      <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.bosques      <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.potenciacion <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.xgboosting   <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.bayes        <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.regrLog      <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            MC.regrLogP     <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            #MC.redes        <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            
            for(k in 1:numGrupos) {
              muestra      <- grupos[[i]][[k]] 
              ttesting     <- datos[muestra, ]
              ttraining    <- datos[-muestra, ]
              
              modelo     <- train.svm(var_, data = ttraining, 
                                      scale = as.logical(params$scal_svm), 
                                      kernel = params$kernel_svm)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.svm     <- MC.svm + MC
              
              modelo     <- train.knn(var_, data = ttraining, scale = as.logical(params$scal_kn), 
                                      kernel = params$kernel_kn, kmax = params$k_kn)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.knn     <- MC.knn + MC
              
              modelo     <- train.rpart(var_, data = ttraining,
                                        control = rpart.control(minsplit = params$minsplit_dt, maxdepth = params$maxdepth_dt),parms = list(split = params$tipo_dt))
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.arboles <- MC.arboles + MC
              
              modelo     <- train.randomForest(var_, data = ttraining, mtry = params$mtry, ntree = params$ntree, importance = TRUE)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.bosques <- MC.bosques + MC
              
              modelo     <- train.adabag(var_, data = ttraining, coeflearn = params$coeflearn_b, mfinal = 100,
                                         control = rpart.control(minsplit = params$minsplit_b, maxdepth = params$maxdepth_b))
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.potenciacion <- MC.potenciacion + MC

              modelo        <- train.xgboost(var_, data = ttraining, booster = params$tipo_xgb, 
                                             max_depth = params$maxdepth_xgb, nrounds = params$n.rounds, verbose = 0)
              prediccion    <- predict(modelo, ttesting)
              MC            <- confusion.matrix(ttesting, prediccion)
              MC.xgboosting <- MC.xgboosting + MC


              modelo     <- train.bayes(var_, data = ttraining)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.bayes   <- MC.bayes + MC

              modelo     <- train.glm(var_, data = ttraining)
              prediccion <- predict(modelo, ttesting)
              MC         <- confusion.matrix(ttesting, prediccion)
              MC.regrLog <- MC.regrLog + MC

              modelo      <- train.glmnet(var_, data = ttraining, standardize = as.logical(params$scal_rlr), alpha = params$alpha, family = 'multinomial')
              prediccion  <- predict(modelo, ttesting)
              MC          <- confusion.matrix(ttesting, prediccion)
              MC.regrLogP <- MC.regrLogP + MC
              
              # modelo     <- train.neuralnet(var_, data = ttraining,
              #                               threshold = params$threshold,
              #                               stepmax   = params$stepmax,
              #                               hidden    = params$capas.np)
              # prediccion <- predict(modelo, ttesting)
              # MC         <- confusion.matrix(ttesting, prediccion)
              # MC.redes   <- MC.redes + MC
            }
            
            MCs.svm[[i]]          <- MC.svm
            MCs.knn[[i]]          <- MC.knn
            MCs.arboles[[i]]      <- MC.arboles
            MCs.bosques[[i]]      <- MC.bosques
            MCs.potenciacion[[i]] <- MC.potenciacion
            MCs.xgboosting[[i]]   <- MC.xgboosting
            MCs.bayes[[i]]        <- MC.bayes
            MCs.regrLog[[i]]      <- MC.regrLog
            MCs.regrLogP[[i]]     <- MC.regrLogP
            #MCs.redes[[i]]        <- MC.redes
          }
          
          MCs.cv <- list(MCs.svm          = MCs.svm,          MCs.knn        = MCs.knn, 
                         MCs.arboles      = MCs.arboles,      MCs.bosques    = MCs.bosques,
                         MCs.potenciacion = MCs.potenciacion, MCs.xgboosting = MCs.xgboosting,
                         MCs.bayes        = MCs.bayes,        MCs.regrLog    = MCs.regrLog,
                         MCs.regrLogP     = MCs.regrLogP)
          
          methods <- c("knn", "arboles", "potenciacion",  "bosques", "svm", 
                       "bayes", "xgboosting", "regrLog", "regrLogP")
          M$MCs.cv     <- MCs.cv
          resultados   <- indices.cv(category, cant.vc, methods, MCs.cv)
          resultados$grafico$name <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb" , "rl", "rlr"),codedioma$idioma)
          M$grafico    <- resultados$grafico
          M$global     <- resultados$global
          M$categories <- resultados$categories
          isolate(codedioma$code <- append(codedioma$code, cv_cv_code(variable, dim_v, cant.vc, numGrupos)))
          res <<- resultados
          print("SVM")
          print(MCs.svm)
          print("KNN")
          print(MCs.knn)
          print("ARBOLES")
          print(MCs.arboles)
          print("BOSQUES")
          print(MCs.bosques)
          print("BOOSTING")
          print(MCs.potenciacion)
          print("XGBOOSTING")
          print(MCs.xgboosting)
          print("Bayes")
          print(MCs.bayes)
          print(tr("rl"))
          print(MCs.regrLog)
          print(tr("rlr"))
          print(MCs.regrLogP)
          
        },error = function(e){
          return(e)
          #return(invisible(''))
        })
        
      })
    })
    
    
    output$e_cv_precision  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      tryCatch({
        indice  <- input$cvcv_glo
        grafico <- M$grafico
        error   <- indice == "0"
        label   <- ifelse(error, tr("errG",idioma), tr("precG",idioma))
        if(!is.null(grafico)){
          if(error)
            grafico$value <- 1 - M$global
          resumen.barras.h(grafico, labels = c(label,  unlist(strsplit(tr("generarM",idioma), '[ ]')[[1]][2])), error = error)
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
          resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ),unlist(strsplit(tr("generarM",idioma), '[ ]')[[1]][2])))
          
        }
        else
          return(NULL)
      },error = function(e){
        return(NULL)
      })
    })
    #Actualiza la cantidad de capas ocultas (neuralnet)
    observeEvent(input$cant.capas.nn.pred, {
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
    
    listar_parametros <- function(){
      isolate({
        k_kn         <-  input$kmax.knn 
        scal_kn      <-  input$switch.scale.knn.pred 
        kernel_kn    <-  input$kernel.knn.pred 
        tipo_dt      <-  input$split.dt.pred 
        minsplit_dt  <-  input$minsplit.dt.pred 
        maxdepth_dt  <-  input$maxdepth.dt.pred 
        mtry         <-  input$mtry.rf.pred 
        ntree        <-  input$ntree.rf.pred 
        scal_svm     <-  input$switch.scale.svm.pred 
        kernel_svm   <-  input$kernel.svm.pred 
        tipo_xgb     <-  input$boosterXgb.pred 
        maxdepth_xgb <-  input$maxdepthXgb 
        n.rounds     <-  input$nroundsXgb 
        threshold    <-  input$threshold.nn 
        stepmax      <-  input$stepmax_nn 
        capas.np     <- c(input$nn.cap.pred.1 , input$nn.cap.pred.2 ,
                          input$nn.cap.pred.3 , input$nn.cap.pred.4 ,
                          input$nn.cap.pred.5 , input$nn.cap.pred.6 ,
                          input$nn.cap.pred.7 , input$nn.cap.pred.8 ,
                          input$nn.cap.pred.9 , input$nn.cap.pred.10 )
        cant.capas   <-  input$cant.capas.nn.pred 
        capas.np     <-  as.vector(as.numeric(capas.np[1:cant.capas] ))
        scal_rlr     <-  input$switch.scale.rlr.pred 
        alpha        <-  input$alpha.rlr.pred 
        iter         <-  input$iter.boosting.pred 
        maxdepth_b   <-  input$maxdepth.boosting.pred 
        minsplit_b   <-  input$minsplit.boosting.pred
        coeflearn_b  <- input$coeflearn
      })
      return(list(k_kn       = k_kn,       scal_kn     = scal_kn,     kernel_kn    = kernel_kn, 
                  tipo_dt    = tipo_dt,    minsplit_dt = minsplit_dt, maxdepth_dt  = maxdepth_dt, 
                  mtry       = mtry,       ntree       = ntree,       scal_svm     = scal_svm, 
                  kernel_svm = kernel_svm, tipo_xgb    = tipo_xgb,    maxdepth_xgb = maxdepth_xgb,  
                  n.rounds   = n.rounds,   threshold   = threshold,   stepmax      = stepmax, 
                  capas.np   = capas.np,   scal_rlr    = scal_rlr,    alpha        = alpha, 
                  iter       = iter,       maxdepth_b  = maxdepth_b,  minsplit_b   = minsplit_b, coeflearn_b = coeflearn_b))
    }
    
    defaul_param_values <- function(){
      updateSliderInput(session, "cant.capas.nn.pred", value = 3)
    }
}
    
## To be copied in the UI
# mod_cross_validation_ui("cross_validation_1")
    
## To be copied in the server
# mod_cross_validation_server("cross_validation_1")
