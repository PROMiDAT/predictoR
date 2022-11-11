#' cv_rlr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_rlr_ui <- function(id){
  ns <- NS(id)
  
  
  title_comp <- list(conditionalPanel("input['cv_rlr_ui_1-Boxrlr'] == 'tabCVrlrIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvrlr.sel"),label = NULL,
                                                               choices =  "", width = "100%")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxrlr"), title = title_comp, 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVrlrModelo",
               div(col_6(radioSwitch(ns("scale_cvrlr"), "escal", c("si", "no"))),
               col_6(
                 selectizeInput(
                   ns("sel_alpha"), labelInput("selectAlg"), multiple = T,
                   choices = list("Ridge" = 0, "Lasso" = 1)))),
               
               div(col_6(numericInput(ns("cvrlr_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1)),
                        col_6(selectInput(ns("cvrlr_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvrlr")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_rlr"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVrlrIndices",
               div(col_6(echarts4rOutput(ns("e_rlr_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_rlr_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVrlrIndicesCat",
               div(col_12(echarts4rOutput(ns("e_rlr_category"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_rlr Server Functions
#'
#' @noRd 
mod_cv_rlr_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.rlr = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)

    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.rlr <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectizeInput(session, "sel_alpha", selected = "")
        updateSelectInput(session, "cvrlr.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvrlr_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvrlr_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvrlr_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvrlr_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvrlr_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvrlr <- renderPrint({
      input$btn_cv_rlr
      M$MCs.rlr <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        alphas       <- isolate(input$sel_alpha)
        alpha_labels <- alphas
        cant.vc   <- isolate(updateData$numValC)
        MCs.rlr   <- vector(mode = "list")
        datos     <- isolate(updateData$datos)
        numGrupos <- isolate(updateData$numGrupos)
        grupos    <- isolate(updateData$grupos)
        scales    <- isolate(input$scale_cvrlr)
        variable  <- updateData$variable.predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))
        dim_v     <- isolate(length(category))
        nombres   <- vector(mode = "character", length = length(alphas))
        Corte     <- isolate(input$cvrlr_step)
        cat_sel   <- isolate(input$cvrlr_cat)
        
        alpha_labels[which(alpha_labels == 0)] = "Ridge"
        alpha_labels[which(alpha_labels == 1)] = "Lasso"
        
        if(length(alphas)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un alpha")
        }
        for (alpha in 1:length(alphas)){
          MCs.rlr[[paste0("MCs.",alpha_labels[alpha])]] <- vector(mode = "list", length = cant.vc)
          nombres[alpha] <- paste0("MC.",alpha_labels[alpha])
        }
        
        for (i in 1:cant.vc){
          MC.rlr <- vector(mode = "list", length = length(alphas))
          names(MC.rlr) <- nombres
          for (alpha in 1:length(alphas)){
            MC.rlr[[alpha]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            for (j in 1:length(alphas)){
              modelo <- traineR::train.glmnet(as.formula(var_), data = ttraining, standardize = as.logical(scales), alpha = alphas[j], family = 'multinomial' )
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive,]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.rlr[[j]] <- MC.rlr[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.rlr[[j]] <- MC.rlr[[j]] + MC
              }
            }
          }
          
          for (l in 1:length(MCs.rlr)){
            MCs.rlr[[l]][[i]] <- MC.rlr[[l]]
          }
        }
        
        M$MCs.rlr  <- MCs.rlr
        resultados <- indices.cv(category, cant.vc, alpha_labels, MCs.rlr)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        print(MCs.rlr)
        
      },error = function(e){
        M$MCs.rlr <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    output$e_rlr_glob  <-  renderEcharts4r({
      input$btn_cv_rlr
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        resumen.barras(grafico, labels = c(tr("precG",idioma), "alpha"))
      }
      else
        return(NULL)
    })    
    
    output$e_rlr_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        resumen.barras(err, labels = c(tr("errG",idioma), "alpha"), error = TRUE)
        
      }
      else
        return(NULL)
    })
    
    
    output$e_rlr_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cvrlr.sel
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), "alpha"))
        
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_rlr_ui("cv_rlr_1")
    
## To be copied in the server
# mod_cv_rlr_server("cv_rlr_1")
