#' poder_pred UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_poder_pred_ui <- function(id){
  ns <- NS(id)
  opc_podpred <- fluidRow(
    conditionalPanel(
    "input.BoxPodPred == 'tabDistpred' || input.BoxPodPred == 'tabPares'",
    tabsOptions(botones = list(icon("terminal")), widths = 100,heights = 55, tabs.content = list(
    list(
      conditionalPanel(
        "input.BoxPodPred == 'tabDistpred'",
        codigo.monokai(ns("fieldCodeDistpred"), height = "10vh")),
      conditionalPanel(
        "input.BoxPodPred == 'tabPares'",
        codigo.monokai(ns("fieldCodePares"), height = "10vh")))
  ))),
  conditionalPanel(
    "input.BoxPodPred == 'tabDistpredcat' || input.BoxPodPred == 'tabDenspred'",
    tabsOptions(heights = c(70, 30), tabs.content = list(
      list(options.run(ns("run_podpred")), tags$hr(style = "margin-top: 0px;"),
           conditionalPanel(
             "input.BoxPodPred == 'tabDistpredcat'",
             selectInput(inputId = ns("sel_pred_cat"), label = NULL, choices = "")
           ),
           conditionalPanel(
             "input.BoxPodPred == 'tabDenspred'",
             selectInput(inputId = ns("sel_dens_pred"), label = NULL, choices = "")
           )),
      list(
        conditionalPanel(
          "input.BoxPodPred == 'tabDistpredcat'",
          codigo.monokai(ns("fieldCodeDistpredcat"), height = "10vh")),
        conditionalPanel(
          "input.BoxPodPred == 'tabDenspred'",
          codigo.monokai(ns("fieldCodeDenspred"), height = "10vh")))
    ))))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxPodPred",opciones = opc_podpred,
      tabPanel(
        title = labelInput("distpred"), value = "tabDistpred",
        withLoader(echarts4rOutput(ns('hc_distpred'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("pares"), value = "tabPares",
        withLoader(plotOutput(ns('plot_pairs_poder'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("distpredcat"), value = "tabDistpredcat",
        withLoader(plotOutput(ns('plot_dist_poder'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("denspred"), value = "tabDenspred",
        withLoader(plotOutput(ns('plot_density_poder'), height = "75vh"), 
                   type = "html", loader = "loader4"))
    )
  )
}
    
#' poder_pred Server Function
#'

mod_poder_pred_server <- function(input, output, session, updateData){
  ns <- session$ns
 
  #' Gráfico de Distribución Variable a Predecir 
  output$hc_distpred = renderEcharts4r({
    var  <- updateData$variable.predecir
    validate(need(var != "", tr("errorcat", isolate(updateData$idioma))))
    
    tryCatch({
      data <- updateData$datos[, var]
      cod <- code.dist.varpred(var)
      updateAceEditor(session, "fieldCodeDistpred", value = cod)
      datos <- data.frame (
        label = levels(data), 
        value = summary(data, maxsum = length(levels(data)))
      )
      datos %>% e_charts(label) %>% e_bar(value, name = var) %>%
        e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  

  #' Update on load testing data
  observeEvent(updateData$datos.prueba, {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    nombres      <- colnames.empty(var.numericas(datos))
    cat.sin.pred <- colnames.empty(var.categoricas(datos))
    cat.sin.pred <- cat.sin.pred[cat.sin.pred != variable]
    updateSelectInput(session, "sel_pred_cat", choices = cat.sin.pred)
    updateSelectInput(session, "sel_dens_pred", choices = nombres)
  })
  
  
  output$plot_pairs_poder <- renderPlot({
    tryCatch({
      variable  <- updateData$variable.predecir
      datos     <- updateData$datos
      cod.pairs <- code.pairs.poder(variable)
      idioma    <- updateData$idioma
      res       <-    NULL
      updateAceEditor(session, "fieldCodePares", value = cod.pairs)
       if (ncol(var.numericas(datos)) >= 2) {
         if(ncol(var.numericas(datos)) <= 25){
           pairs.poder(datos,variable)
         }else{
           showNotification(tr("bigPlot",idioma), duration = 10, type = "message")
           
         }
       }else{
         res <- error.variables(T,idioma)
       }
      return(res)
      
    }, error = function(e) {
      showNotification(paste0("Error en Poder Predictivo: ", e),
                       duration = 10,
                       type = "error")
      return(NULL)
    })
    
  })
  
  
  output$plot_density_poder <- renderPlot({
    input$run_podpred
    variable.num  <- isolate(input$sel_dens_pred)
    idioma        <- updateData$idioma
    variable.pred <- updateData$variable.predecir
    datos         <- updateData$datos
    tryCatch({
      cod.poder.den <- code.plot.numerico.dens(variable.num,variable.pred,label=tr("distpodcat",idioma))
      updateAceEditor(session, "fieldCodeDenspred", value = cod.poder.den)
      if (ncol(var.numericas(datos)) >= 1) {
        plot.numerico.dens(datos,variable.num,variable.pred,label=tr("distpodcat",idioma)) 
      }else{
        res <- error.variables(T,idioma)
        return(res)
      }
    }, error = function(e) {
      showNotification(paste0("Error en Poder Predictivo: ", e),
                       duration = 10,
                       type = "error")
      return(NULL)
    })
  })
  
  # Hace el grafico de poder predictivo categorico
  output$plot_dist_poder <- renderPlot({
    input$run_podpred
    variable.cat  <- isolate(input$sel_pred_cat)
    idioma        <- updateData$idioma
    variable.pred <- updateData$variable.predecir
    datos         <- updateData$datos
    #Incluir alguna de estas librerias en DESCRIPTION para grafico de categoricos
    #library(forcats)
    #library(ggplot2)
    #library(dplyr)
    #library(lubridate)
    #library(scales)
    tryCatch({
      cod.poder.dist.cat <- code.plot.dist.cat(variable.cat,variable.pred,label=tr("denspodlab",idioma))
      updateAceEditor(session, "fieldCodeDistpredcat", value = cod.poder.dist.cat)
      if (ncol(var.categoricas(datos)) > 1) {
        plot.dist.cat(datos,variable.cat,variable.pred,label=tr("denspodlab",idioma)) 
      }else{
        res <- error.variables(F,idioma)
        return(res)
      }
    }, error = function(e) {
      showNotification(paste0("Error en Poder Predictivo: ", e),
                       duration = 10,
                       type = "error")
      return(NULL)
    })
  })

}
    
## To be copied in the UI
# mod_poder_pred_ui("poder_pred_ui_1")
    
## To be copied in the server
# callModule(mod_poder_pred_server, "poder_pred_ui_1")
 
