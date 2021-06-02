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
  
  opciones.comparacion <- list(options.run(ns("runComp")), tags$hr(style = "margin-top: 0px;"),
                               fluidRow(col_12(selectInput(inputId = ns("roc.sel"),label = labelInput("selectCat"),
                                                           choices =  "", width = "100%"))))
  
  
  opcs_comparacion  <- tabsOptions(botones = list(icon("gear")), widths = c(100), heights = c(88),
                                   tabs.content = list(opciones.comparacion))
  
  tagList(
    tabBoxPrmdt(id = "BoxCom", opciones = opcs_comparacion,
                tabPanel(title = labelInput("tablaComp"),
                         withLoader(DT::dataTableOutput(ns("TablaComp"), height="70vh"), 
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("rocCurva"), 
                         withLoader(echarts4rOutput(ns('plot_roc'), height = "70vh"), 
                                    type = "html", loader = "loader4")))
  )
}
    
#' comparacion Server Function

mod_comparacion_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  #' Update on load testing data
  observeEvent(updateData$datos.prueba, {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    updateData$selector.comparativa <- actualizar.selector.comparativa()
    choices      <<- as.character(unique(datos[, variable]))
    
    updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1])
  })

  output$TablaComp <- DT::renderDataTable({
    res <- data.frame()
    m   <<- modelos$mdls
    idioma <- updateData$idiaoma
    isolate(test <- updateData$datos.prueba)
    isolate(var  <- updateData$variable.predecir)
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
              pred     <<- predict(alg$modelo, test, type = "prob")
              if(!startsWith(alg$nombre, "rl")){
                new$roc <- areaROC( pred$prediction[,2],test[,var])
              }else{
                new$roc <- areaROC( pred$prediction[,2,],test[,var])
              }
            }else{
              new$roc <- NA
            }
            row.names(new) <- alg$nombre
            res <- rbind(res, new)
          } 
      }
      #colnames(res[c(1:2,length(res))]) <- c(tr('precG', idioma),tr("errG", idioma), tr('aROC', idioma))
      
    }
    
    DT::datatable(res, selection = "none", editable = FALSE,
                                   options = list(dom = "frtip", pageLength = 10, buttons = NULL))
  },server = FALSE)
  
  #   #Hace el grafico de la curva roc
    output$plot_roc <- renderEcharts4r({
      idioma <- updateData$idioma
      isolate(test <- updateData$datos.prueba)
      isolate(var <- updateData$variable.predecir)
      if(!is.null(test) & length(levels(test[,var])) == 2) {
        clase      <- test[,var]
        y = c(0, 1)
        x = c(1, 0)
        res <<- data.frame(x, y, nombre = "roc")
        for (modelo in modelos$mdls) {
          if(!is.null(modelo)){
            for (alg in modelo) {
             pre      <- predict(alg$modelo, test, type = "prob")
             roc.data <- pROC::roc(test[,var], pre$prediction[,2])
             y  <- roc.data$sensitivities
             x  <- roc.data$specificities
             res <<- rbind(res,data.frame(x= x, y = y, nombre = alg$nombre))
            } 
          }
        }
        
        res$nombre <- as.factor(res$nombre)
        colores  <- gg_color_hue(length(unique(res$nombre)))
        plotroc  <- res %>%
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
        # calcular.areas(input$roc.sel)
        # e_plot_ROC(input$select.models)
      } else {
        showNotification(tr("RocNo", idioma), duration = 15, type = "warning")
        return(NULL)
      }
    })
  
  # #Genera las áreas y gráfico de la curva ROC
  # ejecutar.roc <- function(){
  #   output$TablaComp <- DT::renderDataTable({
  #     if (!is.null(updateData$datos.aprendizaje)) {
  #       calcular.areas(input$roc.sel)
  #       DT::datatable(tabla.comparativa(input$select.models, updateData$selector.comparativa, updateData$idioma),
  #                     selection = "none", editable = FALSE,
  #                     options = list(dom = "frtip", pageLength = 10, buttons = NULL))
  #     }
  #   },server = FALSE)
  #   
  #   #Hace el grafico de la curva roc
  #   output$plot_roc <- renderEcharts4r({
  #     idioma <- updateData$idioma
  #     if(!is.null(datos.prueba) & length(levels(datos[,variable.predecir])) == 2) {
  #       calcular.areas(input$roc.sel)
  #       e_plot_ROC(input$select.models)
  #     } else {
  #       showNotification(tr("RocNo", idioma), duration = 15, type = "warning")
  #       return(NULL)
  #     }
  #   })
  # }

}
    
## To be copied in the UI
# mod_comparacion_ui("comparacion_ui_1")
    
## To be copied in the server
# callModule(mod_comparacion_server, "comparacion_ui_1")
 
