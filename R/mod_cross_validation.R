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
  tagList(
    tabBoxPrmdt(
      id = ns("BoxCV"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVsvmModelo",
               fluidRow(col_12(
                          selectizeInput(
                            ns("sel_methods"), labelInput("selkernel"), multiple = T,
                            choices = list("knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr")))),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_svm"), labelInput("generar"), width  = "100%" ),br(),br())
    )
 
  )
}
    
#' cross_validation Server Functions
#'
#' @noRd 
mod_cross_validation_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    observeEvent(codedioma$idioma, {
      
      nombres <- list( "knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr")
      names(nombres) <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb", "nn", "rl", "rlr"),codedioma$idioma)
      
      updateSelectInput(session, "sel_methods", choices = nombres, selected = input$sel_methods)
    })
    

}
    
## To be copied in the UI
# mod_cross_validation_ui("cross_validation_1")
    
## To be copied in the server
# mod_cross_validation_server("cross_validation_1")
