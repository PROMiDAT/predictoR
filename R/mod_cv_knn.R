#' cv_knn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_knn_ui <- function(id){
  ns <- NS(id)
  
  
  title_comp <- list(conditionalPanel("input['cv_knn_ui_1-BoxKnn'] == 'tabCVKnnIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvknn.sel"),label = NULL,
                                                               choices =  "", width = "100%")))),
                     conditionalPanel("input['cv_knn_ui_1-BoxKnn'] == 'tabCVKnnIndices3'",
                                      div(id = ns("row2"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvknn.glo"),label = NULL,
                                                               choices = "")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxKnn"), title = title_comp, 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVKnnModelo",
               fluidRow(col_6(numericInput(ns("kmax_cvknn"), labelInput("kmax"), min = 1,step = 1, value = 7)),
                        col_6(radioSwitch(ns("scale_cvknn"), "escal", c("si", "no")))),
               fluidRow(col_12(
                 selectizeInput(
                   ns("sel_kernel"), labelInput("selkernel"), multiple = T,
                   choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                               "triweight", "cos","inv","gaussian")))),
               
               fluidRow(col_6(numericInput(ns("cvknn_step"), labelInput("probC"), value = 0.5, width = "100%")),
                        col_6(selectInput(ns("cvknn_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvknn")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_knn"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVKnnIndices",
               div(col_6(echarts4rOutput(ns("e_knn_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_knn_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVKnnIndicesCat",
               div(col_12(echarts4rOutput(ns("e_knn_category"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_knn Server Functions
#'
#' @noRd 
mod_cv_knn_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.knn = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(codedioma$idioma, {
      
      nombres <- list(0, 1)
      names(nombres) <- tr(c("errG", "precG"),codedioma$idioma)
      
      updateSelectInput(session, "cvknn.glo", choices = nombres, selected = 1)
    })
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.knn <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      M$times <- 0
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        updateNumericInput(session,"kmax_cvknn",value = round(sqrt(nrow(datos))))
        choices      <- as.character(unique(datos[, variable]))
        updateSelectInput(session, "cvknn.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvknn_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvknn_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvknn_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvknn_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvknn_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvknn <- renderPrint({
      input$btn_cv_knn
      M$MCs.knn <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        kernels   <- isolate(input$sel_kernel)
        cant.vc   <- isolate(updateData$numValC)
        MCs.knn   <- vector(mode = "list")
        datos     <- isolate(updateData$datos)
        numGrupos <- isolate(updateData$numGrupos)
        grupos    <- isolate(updateData$grupos)
        kmax      <- isolate(input$kmax_cvknn)
        scales    <- isolate(input$scale_cvknn)
        variable  <- updateData$variable.predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))
        dim_v     <- isolate(length(category))
        nombres   <- vector(mode = "character", length = length(kernels))
        Corte     <- isolate(input$cvknn_step)
        cat_sel   <- isolate(input$cvknn_cat)
        
        if(length(kernels)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un kernel")
        }
        for (kernel in 1:length(kernels)){
          MCs.knn[[paste0("MCs.",kernels[kernel])]] <- vector(mode = "list", length = cant.vc)
          nombres[kernel] <- paste0("MC.",kernels[kernel])
        }
        
        for (i in 1:cant.vc){
          MC.knn <- vector(mode = "list", length = length(kernels))
          names(MC.knn) <- nombres
          for (kernel in 1:length(kernels)){
            MC.knn[[kernel]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            for (j in 1:length(kernels)){
              modelo      <- train.knn(as.formula(var_), data = ttraining, kernel = kernels[j], kmax = kmax, scale = as.logical(scales))
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.knn[[j]] <- MC.knn[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.knn[[j]] <- MC.knn[[j]] + MC
              }
            }
          }
          
          for (l in 1:length(MCs.knn)){
            MCs.knn[[l]][[i]] <- MC.knn[[l]]
          }
        }
        
        M$MCs.knn  <- MCs.knn
        resultados <- indices.cv(category, cant.vc, kernels, MCs.knn)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        print(MCs.knn)
        
      },error = function(e){
        M$MCs.knn <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    output$e_knn_glob  <-  renderEcharts4r({
      input$btn_cv_knn
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        resumen.puntos(grafico, labels = c(tr("precG",idioma), "Kernel"))
      }
      else
        return(NULL)
    })    
    
    output$e_knn_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        resumen.puntos(err, labels = c(tr("errG",idioma), "Kernel"))
        
      }
      else
        return(NULL)
    })
    
    
    output$e_knn_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cvknn.sel
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        resumen.puntos(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), "Kernel"))
        
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_knn_ui("cv_knn_1")
    
## To be copied in the server
# mod_cv_knn_server("cv_knn_1")
