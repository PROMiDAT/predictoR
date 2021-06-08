#' comparacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comparacion_ui <- function(id){
  ns <- NS(id)

  title_comp <- fluidRow(shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                         tags$div(class="multiple-select-var",
                                  selectInput(inputId = ns("roc.sel"),label = NULL,
                                              choices =  "", width = "100%")))
  tagList(
    tabBoxPrmdt(id = "BoxCom", title = title_comp,
                tabPanel(title = labelInput("tablaComp"),
                         withLoader(DT::dataTableOutput(ns("TablaComp"), height="70vh"), 
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("rocCurva"), 
                         withLoader(echarts4rOutput(ns('plot_roc'), height = "70vh"), 
                                    type = "html", loader = "loader4")))
  )
}
    
#' comparacion Server Function
#'
#' @noRd 
mod_comparacion_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  
  # Update on load testing data
  observeEvent(updateData$datos.prueba, {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    choices      <- as.character(unique(datos[, variable]))
    if(length(choices) == 2){
      updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1])
    }else{
      updateSelectInput(session, "roc.sel", choices = "")
    }
  })
  
  # Update Comparison Table
  output$TablaComp <- DT::renderDataTable({
    res      <- data.frame()
    idioma   <- updateData$idioma
    category <- input$roc.sel
    isolate(test <- updateData$datos.prueba)
    isolate(var  <- updateData$variable.predecir)
    tryCatch({
    for (modelo in modelos$mdls) {
      if(!is.null(modelo)){
          for (alg in modelo) {
            ind <- general.indexes(mc = alg$mc)
            new <- data.frame(
              OAccuracy = ind$overall.accuracy,
              EAccuracy = ind$overall.error
            )
            
            for (cat in names(ind$category.accuracy)) {
              new[[cat]] <- ind$category.accuracy[[cat]]
            }
            if(length(ind$category.accuracy) ==2){
              if(!startsWith(alg$nombre, "rl")){
                new$roc <- areaROC(alg$prob$prediction[,category], test[,var])
              }else{
                new$roc <- areaROC(alg$prob$prediction[,category,], test[,var])
              }
            }else{
              new$roc <- NULL
            }
            row.names(new) <- split_name(alg$nombre, idioma)
            res            <- rbind(res, new)
          } 
      }
      
    }
    colnames(res)[1]           <- tr('precG', idioma)
    colnames(res)[2]           <- tr('errG', idioma)

    if(length(ind$category.accuracy) ==2){
      colnames(res)[dim(res)[2]] <- tr('aROC', idioma)
    }
    
    res[]                      <- lapply(res, as.numeric)
    res                        <- round(res, 5)*100
    DT::datatable(res, selection = "none", editable = FALSE,
                                   options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    }, error = function(e) {
      DT::datatable(data.frame(), selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    })
  },server = FALSE)
  
  # Update Plot ROC
    output$plot_roc <- renderEcharts4r({
      idioma        <- updateData$idioma
      category      <- input$roc.sel
      isolate(test  <- updateData$datos.prueba)
      isolate(var   <- updateData$variable.predecir)
      if(!is.null(test) & length(levels(test[,var])) == 2) {
        clase      <- test[,var]
        y <- c(0, 1)
        x <- c(1, 0)
        nombre <- "roc"
        res    <- data.frame(x, y, nombre = nombre)
        for (modelo in modelos$mdls) {
          if(!is.null(modelo)){
            for (alg in modelo) {
             if(!startsWith(alg$nombre, "rl")){
               roc.data <- pROC::roc(test[,var], alg$prob$prediction[,category], direction= "<" )
             }else{
               roc.data <- pROC::roc(test[,var], alg$prob$prediction[,category,], direction= "<" )
             }
             y   <- roc.data$sensitivities
             x   <- roc.data$specificities
             res <- rbind(res,data.frame(x= x, y = y, nombre = split_name(alg$nombre, idioma)))
            } 
          }
        }
        
        res$nombre <- as.factor(res$nombre)
        colores    <- gg_color_hue(length(unique(res$nombre)))
        plotroc    <- res %>%
          group_by(nombre) %>%
          e_charts(x) %>%
          e_line(y) %>%
          e_legend(type = "scroll", bottom = 1) %>% 
          e_color(c(colores))    %>% 
          e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
        plotroc$x$opts$color[[which(plotroc$x$opts$legend$data == "roc")]] <- "#5F5C5C"
        plotroc$x$opts$legend$data[[which(plotroc$x$opts$legend$data == "roc")]] <- NULL
        plotroc$x$opts$xAxis[[1]]$inverse <- TRUE
        plotroc
      } else {
        showNotification(tr("RocNo", idioma), duration = 15, type = "warning")
        return(NULL)
      }
    })
}
    
## To be copied in the UI
# mod_comparacion_ui("comparacion_ui_1")
    
## To be copied in the server
# callModule(mod_comparacion_server, "comparacion_ui_1")
 
