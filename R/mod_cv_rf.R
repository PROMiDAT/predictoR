#' cv_rf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_rf_ui <- function(id){
  ns <- NS(id)
  
  
  title_comp <- list(conditionalPanel("input['cv_rf_ui_1-Boxrf'] == 'tabCVrfIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvrf.sel"),label = NULL,
                                                               choices =  "", width = "100%")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxrf"), title = title_comp, 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVrfModelo",
               fluidRow(col_6(numericInput(ns("mtry"), labelInput("numVars"),1, width = "100%", min = 1)),
                        col_6(numericInput(ns("ntree"), labelInput("numTree"), 20, width = "100%", min = 5))),
               fluidRow(col_12(
                 selectizeInput(
                   ns("sel_split"), labelInput("splitIndex"), multiple = T,
                   choices =  list("gini" = "gini", "Entropia" = "information")))),
               
               fluidRow(col_6(numericInput(ns("cvrf_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1)),
                        col_6(selectInput(ns("cvrf_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvrf")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_rf"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVrfIndices",
               div(col_6(echarts4rOutput(ns("e_rf_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_rf_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVrfIndicesCat",
               div(col_12(echarts4rOutput(ns("e_rf_category"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_rf Server Functions
#'
#' @noRd 
mod_cv_rf_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.rf = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.rf  <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      M$times      <- 0
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      n.mtry   <- floor(sqrt(ncol(datos)))
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectizeInput(session, "sel_split", selected = "")
        updateNumericInput(session, "mtry", value = n.mtry)
        updateSelectInput(session, "cvrf.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvrf_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvrf_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvrf_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvrf_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvrf_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvrf <- renderPrint({
      input$btn_cv_rf
      M$MCs.rf  <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        splits    <- isolate(input$sel_split)
        cant.vc   <- isolate(updateData$numValC)
        MCs.rf    <- vector(mode = "list")
        datos     <- isolate(updateData$datos)
        numGrupos <- isolate(updateData$numGrupos)
        grupos    <- isolate(updateData$grupos)
        mtry      <- isolate(input$mtry)
        ntree     <- isolate(input$ntree)
        variable  <- updateData$variable.predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))
        dim_v     <- isolate(length(category))
        nombres   <- vector(mode = "character", length = length(splits))
        Corte     <- isolate(input$cvrf_step)
        cat_sel   <- isolate(input$cvrf_cat)
        
        if(length(splits)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un kernel")
        }
        for (kernel in 1:length(splits)){
          MCs.rf[[paste0("MCs.",splits[kernel])]] <- vector(mode = "list", length = cant.vc)
          nombres[kernel] <- paste0("MC.",splits[kernel])
        }
        
        for (i in 1:cant.vc){
          MC.rf <- vector(mode = "list", length = length(splits))
          names(MC.rf) <- nombres
          for (kernel in 1:length(splits)){
            MC.rf[[kernel]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            for (j in 1:length(splits)){
              modelo      <- train.randomForest(as.formula(var_), data = ttraining, mtry = mtry, ntree = ntree, parms = list(split = splits[j]))
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.rf[[j]]  <- MC.rf[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.rf[[j]]  <- MC.rf[[j]] + MC
              }
            }
          }
          
          for (l in 1:length(MCs.rf)){
            MCs.rf[[l]][[i]] <- MC.rf[[l]]
          }
        }
        
        M$MCs.rf  <- MCs.rf
        resultados <<- indices.cv(category, cant.vc, splits, MCs.rf)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        print(MCs.rf)
        
      },error = function(e){
        M$MCs.rf <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    output$e_rf_glob  <-  renderEcharts4r({
      input$btn_cv_rf
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        
        resumen.barras(grafico, labels = c(tr("precG",idioma), unlist(strsplit(tr("splitIndex",idioma), ":"))))
      }
      else
        return(NULL)
    })    
    
    output$e_rf_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        resumen.barras(err, labels = c(tr("errG",idioma),  unlist(strsplit(tr("splitIndex",idioma), ":"))), error = TRUE)
        
      }
      else
        return(NULL)
    })
    
    
    output$e_rf_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cvrf.sel
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ),  unlist(strsplit(tr("splitIndex",idioma), ":"))))
        
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_rf_ui("cv_rf_1")
    
## To be copied in the server
# mod_cv_rf_server("cv_rf_1")
