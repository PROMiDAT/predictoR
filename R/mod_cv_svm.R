#' cv_svm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_svm_ui <- function(id){
  ns <- NS(id)
  
  
  title_comp <- list(conditionalPanel("input['cv_svm_ui_1-Boxsvm'] == 'tabCVsvmIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvsvm.sel"),label = NULL,
                                                               choices =  "", width = "100%")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxsvm"), title = title_comp, 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVsvmModelo",
               div(col_6(radioSwitch(ns("scale_cvsvm"), "escal", c("si", "no"))),
               col_6(
                 selectizeInput(
                   ns("sel_kernel_svm"), labelInput("selkernel"), multiple = T,
                   choices = c("linear", "polynomial", "radial", "sigmoid")))),
               
               div(col_6(numericInput(ns("cvsvm_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1)),
                        col_6(selectInput(ns("cvsvm_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvsvm")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_svm"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVsvmIndices",
               div(col_6(echarts4rOutput(ns("e_svm_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_svm_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVsvmIndicesCat",
               div(col_12(echarts4rOutput(ns("e_svm_category"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_svm Server Functions
#'
#' @noRd 
mod_cv_svm_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.svm = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.svm <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectizeInput(session, "sel_kernel_svm", selected = "")
        updateSelectInput(session, "cvsvm.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvsvm_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvsvm_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvsvm_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvsvm_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvsvm_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvsvm <- renderPrint({
      input$btn_cv_svm
      M$MCs.svm <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        kernels   <- isolate(input$sel_kernel_svm)
        cant.vc   <- isolate(updateData$numValC)
        MCs.svm   <- vector(mode = "list")
        datos     <- isolate(updateData$datos)
        numGrupos <- isolate(updateData$numGrupos)
        grupos    <- isolate(updateData$grupos)
        scales    <- isolate(input$scale_cvsvm)
        variable  <- updateData$variable.predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))
        dim_v     <- isolate(length(category))
        nombres   <- vector(mode = "character", length = length(kernels))
        Corte     <- isolate(input$cvsvm_step)
        cat_sel   <- isolate(input$cvsvm_cat)
        
        if(length(kernels)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un kernel")
        }
        for (kernel in 1:length(kernels)){
          MCs.svm[[paste0("MCs.",kernels[kernel])]] <- vector(mode = "list", length = cant.vc)
          nombres[kernel] <- paste0("MC.",kernels[kernel])
        }
        
        for (i in 1:cant.vc){
          MC.svm <- vector(mode = "list", length = length(kernels))
          names(MC.svm) <- nombres
          for (kernel in 1:length(kernels)){
            MC.svm[[kernel]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            for (j in 1:length(kernels)){
              modelo      <- train.svm(as.formula(var_), data = ttraining, kernel = kernels[j],  scale = as.logical(scales))
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.svm[[j]] <- MC.svm[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.svm[[j]] <- MC.svm[[j]] + MC
              }
            }
          }
          
          for (l in 1:length(MCs.svm)){
            MCs.svm[[l]][[i]] <- MC.svm[[l]]
          }
        }
        
        M$MCs.svm  <- MCs.svm
        resultados <- indices.cv(category, cant.vc, kernels, MCs.svm)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        isolate(codedioma$code <- append(codedioma$code, cv_svm_code(variable, dim_v, cant.vc, numGrupos)))
        print(MCs.svm)
        
      },error = function(e){
        M$MCs.svm <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    output$e_svm_glob  <-  renderEcharts4r({
      input$btn_cv_svm
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        resumen.barras(grafico, labels = c(tr("precG",idioma), "Kernel"))
      }
      else
        return(NULL)
    })    
    
    output$e_svm_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        resumen.barras(err, labels = c(tr("errG",idioma), "Kernel"), error = TRUE)
        
      }
      else
        return(NULL)
    })
    
    
    output$e_svm_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cvsvm.sel
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), "Kernel"))
        
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_svm_ui("cv_svm_1")
    
## To be copied in the server
# mod_cv_svm_server("cv_svm_1")
