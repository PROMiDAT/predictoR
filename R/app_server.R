#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @keywords internal
app_server <- function( input, output, session ) {
  
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'), emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  exe(paste0("library(traineR)"))
  ##################################  Variables  ##############################
  updateData <- rv(datos              = NULL, 
                   originales         = NULL, 
                   datos.tabla        = NULL, 
                   datos.prueba       = NULL, 
                   datos.aprendizaje  = NULL,
                   variable.predecir  = NULL,
                   indices            = NULL, 
                   numGrupos          = NULL, 
                   numValC            = NULL, 
                   grupos             = NULL)
  
  codedioma <- rv(idioma             = NULL,
                  code = list())
  
  updateData2 <- rv(datos              = NULL, 
                    originales         = NULL, 
                    datos.tabla        = NULL, 
                    datos.prueba       = NULL, 
                    datos.aprendizaje  = NULL,
                    variable.predecir  = NULL,
                    indices            = NULL, 
                    numGrupos          = NULL, 
                    numValC            = NULL, 
                    grupos             = NULL)
  
  newCases   <-     rv(originales        = NULL, 
                       datos.prueba      = NULL, 
                       datos.aprendizaje = NULL,
                       m.seleccionado    = NULL,
                       modelo            = NULL,
                       prediccion        = NULL,
                       variable.predecir = NULL)
  
  modelos    <-  rv(svm      = NULL,
                    knn      = NULL,
                    bayes    = NULL,
                    rl       = NULL,
                    rlr      = NULL,
                    xgb      = NULL,
                    boosting = NULL,
                    rf       = NULL,
                    nn       = NULL,
                    dt       = NULL)
  ###################################  Update  ################################

  #' Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    etiquetas <- c(readeR::labels_readeR(), cambiar.labels())
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg <- input$idioma
    
    keys <- c(
      'doccarga', 'doctt', 'doccv', 'docresumen', 'dochist', 'docqq', 
      'docnormal', 'docdisp', 'docdistnum', 'docdistcat', 'doccor',
      'docrename', 'doctrans', 'doceliminar', 'distpred', 'pares', 'denspred',
      'docpredcat', 'knnl', 'svml', 'gclasificacion', 'dtl', 'reglas', 'garbol', 
      'xgb', 'docImpV', 'rfl', 'docpot', 'evolerror', 'Bayes', 'redPlot', 'nN', 'rl', 'plr',
      'posibLanda', 'gcoeff', 'betas')
    
    for (k in keys) {
      codigo <- gsub(k, tr(k, idioma = lg), codigo, fixed = T)
    }
    
    codigo.completo <- paste0(
      "library(XLConnect)\n", "library(caret)\n",
      "library(traineR)\n", "library(xgboost)\n",
      "library(rpart)\n", "library(rpart.plot)\n",
      "library(glmnet)\n", "library(predictoR)\n",
      "library(echarts4r)\n", "library(readeR)\n\n"
    )
    for (cod in codigo) {
      codigo.completo <- paste0(codigo.completo, "\n", cod)
    }
    updateAceEditor(session, "fieldCode", value = codigo.completo)
  })
  
  output$btn_code <- downloadHandler(
    filename = "codigo.R",
    content = function(con) {
      write(input$fieldCode, con)
    }
  )
  
  #' Enable/disable on load data
  observe({
    element <- "#sidebarItemExpanded li"
    menu.values <- c(
      "[class^=treeview]",  " a[data-value=acp]",  " a[data-value=parte1]", " a[data-value=cj]",
      " a[data-value=kmedias]", " a[data-value=reporte]")
    
    lapply(menu.values, function(i){
      if(is.null(updateData$datos) || ncol(updateData$datos) < 1) {
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
      }
    })
    
    menu.values.segment <- c(
      " a[data-value=poderPred]", " a[data-value=parte2]", " a[data-value=knn]")
    
    lapply(menu.values.segment, function(i){
      if(is.null(updateData$datos.prueba) || ncol(updateData$datos.prueba) < 1) {
        shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')
        shinyjs::disable(selector = 'a[href^="#shiny-tab-comparar"]')
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
        shinyjs::enable(selector = 'a[href^="#shiny-tab-parte2"]')
        shinyjs::enable(selector = 'a[href^="#shiny-tab-comparar"]')
        
      }
      
    })
  })
  
  
  ###################################  Modules  ###############################
  #Carga de Datos
  readeR::mod_carga_datos_server("carga_datos_ui_1", updateData, modelos, codedioma, "predictoR")
  readeR::mod_carga_datos_server("carga_datos_ui_2", updateData2, NULL, codedioma, "discoveR")
  #Estadísticas Básicas
  readeR::mod_r_numerico_server("r_numerico_ui_1",         updateData, codedioma)
  readeR::mod_normal_server("normal_ui_1",                 updateData, codedioma)
  readeR::mod_dispersion_server("dispersion_ui_1",         updateData, codedioma)
  readeR::mod_distribuciones_server("distribuciones_ui_1", updateData, codedioma)
  readeR::mod_correlacion_server("correlacion_ui_1",       updateData, codedioma)
  mod_poder_pred_server("poder_pred_ui_1",                 updateData, codedioma)
  
  #Aprendizaje Supervisado
  callModule(mod_knn_server,            "knn_ui_1",            updateData, modelos, codedioma)
  callModule(mod_svm_server,            "svm_ui_1",            updateData, modelos, codedioma)
  callModule(mod_d_tree_server,         "d_tree_ui_1",         updateData, modelos, codedioma)
  callModule(mod_r_forest_server,       "r_forest_ui_1",       updateData, modelos, codedioma)
  callModule(mod_xgboosting_server,     "xgboosting_ui_1",     updateData, modelos, codedioma)
  callModule(mod_boosting_server,       "boosting_ui_1",       updateData, modelos, codedioma)
  callModule(mod_bayes_server,          "bayes_ui_1",          updateData, modelos, codedioma)
  callModule(mod_neural_net_server,     "neural_net_ui_1",     updateData, modelos, codedioma)
  callModule(mod_l_regression_server,   "l_regression_ui_1",   updateData, modelos, codedioma)
  callModule(mod_penalized_l_r_server,  "penalized_l_r_ui_1",  updateData, modelos, codedioma)
  
  #Comparación de Modelos
  callModule(mod_comparacion_server,    "comparacion_ui_1",    updateData, modelos, codedioma)
  
  
  #Predicción de Individuos Nuevos
  callModule(mod_ind_nuevos_server,     "ind_nuevos_ui_1",  newCases, updateData2, codedioma)
  
}
