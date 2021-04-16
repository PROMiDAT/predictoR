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
  selector.modelos <- checkboxGroupButtons(ns("select.models"), labelInput("selectMod"), c(" ---- " = "NoDisponible"),
                                           size = "sm", status = "primary",
                                           checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                            no = icon("remove", lib = "glyphicon")))
  
  opciones.comparacion <- list(options.run(ns("runComp")), tags$hr(style = "margin-top: 0px;"),
                               fluidRow(col_12(selectInput(inputId = ns("roc.sel"),label = labelInput("selectCat"),
                                                           choices =  "", width = "100%"))),
                               fluidRow(col_12(selector.modelos)))
  
  
  opcs_comparacion  <- tabsOptions(botones = list(icon("gear")), widths = c(100), heights = c(88),
                                   tabs.content = list(opciones.comparacion))
  
  tagList(
    tabBoxPrmdt(id = "BoxCom", opciones = opcs_comparacion,
                tabPanel(title = labelInput("tablaComp"),
                         withLoader(DT::dataTableOutput(ns("TablaComp"), height="70vh"), 
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("rocCurva"), 
                         withLoader(plotOutput(ns('plot_roc'), height = "70vh"), 
                                    type = "html", loader = "loader4")))
  )
}
    
#' comparacion Server Function

mod_comparacion_server <- function(input, output, session, updateData){
  ns <- session$ns
  #' Update on load testing data
  observeEvent(updateData$datos.prueba, {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    updateData$selector.comparativa <- actualizar.selector.comparativa()
    choices      <- as.character(unique(datos[, variable]))
    updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1])
    shinyWidgets::updateCheckboxGroupButtons(session, inputId = "select.models",
                                 choices = c(" ---- " = "NoDisponible"),
                                 size = "sm", status = "primary",
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                  no = icon("remove", lib = "glyphicon")))
    
  })

observeEvent(updateData$selector.comparativa, {
  nombres <- updateData$selector.comparativa
  shinyWidgets::updateCheckboxGroupButtons(session,"select.models",choices = sort(nombres),selected = sort(nombres),
                                           status = "primary",checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                               no = icon("remove", lib = "glyphicon")))

})

observeEvent(updateData$roc, {
  ejecutar.roc()
})

observeEvent(input$runComp, {
  ejecutar.roc()
})

ejecutar.roc <- function(){
  output$TablaComp <- DT::renderDataTable({
    if (!is.null(updateData$datos.aprendizaje)) {
      roc.sel <<- input$roc.sel
      calcular.areas(input$roc.sel)
      DT::datatable(tabla.comparativa(input$select.models, updateData$selector.comparativa, updateData$idioma),
                    selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    }
  },server = FALSE)
  #Hace el grafico de la curva roc
  output$plot_roc <- renderPlot({
    idioma <- updateData$idioma
    if(!is.null(datos.prueba) & length(levels(datos[,variable.predecir])) == 2) {
      calcular.areas(input$roc.sel)
      plotROC(input$select.models)
    } else {
      showNotification(tr("RocNo", idioma), duration = 15, type = "warning")
      return(NULL)
    }
  })
}

}
    
## To be copied in the UI
# mod_comparacion_ui("comparacion_ui_1")
    
## To be copied in the server
# callModule(mod_comparacion_server, "comparacion_ui_1")
 
