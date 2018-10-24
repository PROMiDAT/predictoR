
library(shiny)
library(shinyAce)
library(shinydashboard)
library(shinyWidgets)
library(colourpicker)
library(shinyjs)
library(knitr)
library(DT)
library(ggplot2)
library(corrplot)
library(dendextend)
library(scatterplot3d)
library(stringr)
library(modeest)
library(caret)
library(kknn)
library(flexdashboard)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ada)
library(xgboost)
library(nnet)
library(dplyr)
library(forcats)
library(psych)
library(ROCR)
library(xtable)
library(raster)
library(rattle)

# FUNCIONES --------------------------------------------------------------------------------------------------------------

# Crea un campo de codigo con boton de ejecutar y cargar
campo.codigo <- function(runid, refid, fieldid, ...){
  if(!missing(refid))
    undo <- tags$button(id = refid, type = "button", class = "run-button action-button",
                        icon("undo"), tags$a("Recuperar", style = "color:white"))
  else
    undo <- div()
  tags$div(class = "box box-solid bg-black",
           tags$div(style = "text-align:right;padding-right: 10px;",
                    tags$button(id = runid, type = "button", class = "run-button action-button",
                                icon("play"), tags$a("Ejecutar", style = "color:white")),
                    undo),
           tags$div(class = "box-body",
                    aceEditor(fieldid, mode = "r", theme = "monokai", value = "", ...)))
}

infoBoxPROMiDAT <- function(titulo, valor, icono){
  tags$div(class = "info-box bg-promidat",
           tags$span(class = "info-box-icon", icono),
           tags$div(class = "info-box-content",
                    tags$span(class = "info-box-text", titulo),
                    tags$span(class = "info-box-number", valor)))
}

# MENU --------------------------------------------------------------------------------------------------------------------

menu.cargar <- menuItem("Datos", tabName = "cargar", icon = icon("dashboard"))

menu.estadisticas <- menuItem("Estadísticas Básicas", tabName = "parte1", icon = icon("th-list"),
                              menuSubItem("Resumen Numérico", tabName = "resumen", icon = icon("sort-numeric-asc")),
                              menuSubItem("Test de Normalidad", tabName = "normalidad", icon = icon("bar-chart")),
                              menuSubItem("Dispersión", tabName = "dispersion", icon = icon("line-chart")),
                              menuSubItem("Distribuciones", tabName = "distribucion", icon = icon("area-chart")),
                              menuSubItem("Correlación", tabName = "correlacion", icon = icon("table")),
                              menuItem("Poder Predictivo", tabName = "poderPred", icon = icon("rocket")))

menu.aprendizaje.supervisado <- menuItem("Aprendizaje Supervisado", tabName = "parte2", icon = icon("th-list"),
                                         menuSubItem("K Vecinos Más Cercanos",tabName = "knn",icon = icon("dot-circle-o")),
                                         menuSubItem("Árboles de Decisión",tabName = "dt",icon = icon("tree")),
                                         menuSubItem("Bosques Aleatorios",tabName = "rf",icon = icon("sitemap")),
                                         menuSubItem("Potenciación",tabName = "boosting",icon = icon("superscript")),
                                         menuSubItem("Soporte Vectorial",tabName = "svm",icon = icon("line-chart")))

menu.reporte <- menuItem("Generar Reporte", tabName = "reporte", icon = icon("save-file",lib = "glyphicon"))

menu.comparar <- menuItem("Comparación de Modelos", tabName = "comparar", icon = icon("eye"))

menu.prediccion.nuevos <- menuItem("Predicción Individuos Nuevos", tabName = "predNuevos", icon = icon("table"))

menu.info <- menuItem("Acerca De", tabName = "acercaDe", icon = icon("info"))

mi.menu <- sidebarMenu(id = "principal",
              tags$div(style="padding-top:10px;"),
              menu.cargar,
              menu.estadisticas,
              menu.aprendizaje.supervisado,
              menu.comparar,
              menu.prediccion.nuevos,
              menu.reporte,
              menu.info)


# HEAD HTML ---------------------------------------------------------------------------------------------------------

mi.head <- tags$head(
  tags$script(src = "myscript.js"),
  tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
  tags$link(rel="shortcut icon", href="http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
  useShinyjs()
)

mi.titulo <- tags$script(HTML(
  '$(document).ready(function() {
  $("header").find("nav").append(\'<span class="header-title"> <i>Predicto</i>R </span>\');
  })'))

load.page <- conditionalPanel(condition="($('html').hasClass('shiny-busy'))",
                              div(id = "loaderWrapper", div(id="loader")) )

# PAGINA DE CARGA Y TRANSFORMACION DE DATOS -----------------------------------------------------------------------------

panel.cargar.datos <- tabPanel(title = "Cargar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               checkboxInput('header', 'Encabezado (Header)', TRUE),
                               checkboxInput('rowname', 'Incluir nombre de filas', TRUE),
                               radioButtons('sep', 'Separador', c(Coma=',', 'Punto y Coma'=';', Tab='\t'), selected = 'Coma'),
                               radioButtons('dec', 'Separador Decimal', c('Punto'='.', 'Coma'=","), selected = 'Punto'),
                               switchInput(inputId = "deleteNA", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                           label = "Eliminar NA", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               fileInput('file1', label = 'Cargar Archivo', placeholder = "", buttonLabel = "Subir", width = "100%",
                                         accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
                               actionButton("loadButton", "Cargar", width = "100%"),
                               br(),br(),
                               aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "15vh", readOnly = T))

panel.tansformar.datos <- tabPanel(title = "Transformar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                   DT::dataTableOutput('transData'),
                                   br(),br(),
                                   actionButton("transButton", "Aplicar", width = "100%"),
                                   br(),br(),
                                   aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T))

panel.segmentar.datos <- tabPanel(title = "Configuraciones", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                  fluidRow(column(width = 5, numericInput("semilla", "Semilla Aleatoria:", "NULL", width = "100%")), br(),
                                           column(width = 4, switchInput(inputId = "permitir.semilla", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                                         label = "", onLabel = "Habilitada", offLabel = "Deshabilitada", labelWidth = "100%",
                                                                         inline = T,size = "large"))),
                                  selectInput(inputId = "sel.predic.var", label = h4("Seleccionar Variable a Predecir:"), choices =  "", width = "100%"),
                                  sliderInput("segmentacionDatosA", "Proporción Aprendizaje:",width = "100%",
                                              min = 5, max = 95, value = 70, step = 5),
                                  sliderInput("segmentacionDatosT", "Proporción Prueba:", width = "100%",
                                              min = 5, max = 95, value = 30, step = 5),
                                  actionButton("segmentButton", "Generar", width = "100%"),
                                  br(),br(),
                                  aceEditor("fieldCodeSegment", mode = "r", theme = "monokai", value = "", height = "8vh",  readOnly = T))

muestra.datos <- box(title = "Datos", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                     DT::DTOutput('contents'), type = 7, color = "#CBB051")

muestra.datos.aprend <- box(title = "Datos de Aprendizaje", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                            DT::DTOutput('contentsAprend'), type = 7, color = "#CBB051")

muestra.datos.prueba <- box(title = "Datos de Prueba", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                            DT::DTOutput('contentsPrueba'), type = 7, color = "#CBB051")

pagina.cargar.datos <- tabItem(tabName = "cargar",
                               fluidRow(column(width = 5, tabBox(id ="tabs", title = NULL, width = 12, panel.cargar.datos, panel.tansformar.datos, panel.segmentar.datos)),
                               column(width = 7, muestra.datos)),
                               conditionalPanel(
                                 condition = "input.tabs == 'Configuraciones'",
                                 fluidRow(column(width = 6, muestra.datos.aprend ),
                                          column(width = 6, muestra.datos.prueba ))) )


# PAGINA DE RESUMEN NUMERICO ----------------------------------------------------------------------------------------------

cuadro.resumen.completo <- box(title = "Resumen Numérico", status = "primary", width = 7, solidHeader = TRUE, collapsible = TRUE,
                               DT::dataTableOutput("resumen.completo"), hr(),
                               aceEditor("fieldCodeResum", mode = "r", theme = "monokai", value = "", height = "8vh",  readOnly = T))

cuadro.resumen.variable <- box(title = "Resumen Numérico por Variable", status = "primary", width = 5, solidHeader = TRUE, collapsible = TRUE,
                               selectInput(inputId = "sel.resumen", label = h4("Seleccionar Variable:"), choices =  ""),
                               fluidRow(uiOutput("resumen")))

pagina.resumen.numerico <- tabItem(tabName = "resumen",
                                   fluidRow(cuadro.resumen.completo,
                                   cuadro.resumen.variable ))

# PAGINA DEL TEST DE NORMALIDAD -------------------------------------------------------------------------------------------

boton.colores <- column(width = 3, dropdownButton(h4("Opciones"),
                                                  colourpicker::colourInput("col.normal", "Seleccionar Color:",
                                                                            value = "#00FF22AA", allowTransparent = T),
                                                  circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                                  tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T))


opciones.normalidad <-  fluidRow( column(width = 9, selectInput(inputId = "sel.normal", label = NULL, choices =  "")),
                                  boton.colores )

panel.grafico.normalidad.num <- tabPanel(title = "Gráfico Normalidad", value = "tabNormalPlot", plotOutput('plot.normal', height = "65vh"))

panel.grafico.normalidad.cat <- tabPanel(title = "Test de Normalidad", value = "tabNormalCalc", DT::dataTableOutput('calculo.normal'))

codigo.normalidad.uno <- conditionalPanel("input.BoxNormal == 'tabNormalPlot'",
                                          column(width = 12, campo.codigo(runid = "run.normal", fieldid = "fieldCodeNormal", height = "8vh")))

codigo.normalidad.dos <- conditionalPanel("input.BoxNormal == 'tabNormalCalc'",
                                          column(width = 12, campo.codigo(runid = "run.calc.normal", fieldid = "fieldCalcNormal", height = "8vh")))

pagina.test.normalidad <- tabItem(tabName = "normalidad",
                                  tabBox(id = "BoxNormal",
                                         width = 12, title = opciones.normalidad,
                                         panel.grafico.normalidad.num,
                                         panel.grafico.normalidad.cat),
                                  fluidRow(codigo.normalidad.uno,
                                  codigo.normalidad.dos))

# PAGINA DE DISPERSION -----------------------------------------------------------------------------------------------------

codigo.dispersion <- column(width = 12, campo.codigo(runid = "run.disp", fieldid = "fieldCodeDisp", height = "8vh"))

datos.dispersiones <- column(width = 4, DT::dataTableOutput('mostrar.disp.zoom'), hr(), plotOutput('plot.disp.zoom'))

opciones.dispersion <- fluidRow(column(width = 9, tags$div(class="select-var-ind",
                                                            selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                                                           options = list(maxItems = 3, placeholder = "Seleccione la(s) variable(s)")))),
                                 column(width = 3,dropdownButton(h4("Opciones"),
                                                                 colourpicker::colourInput("col.disp", "Seleccionar Color:",
                                                                                           value = "#FF0000AA", allowTransparent = T),
                                                                 circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                                                 tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T)))


grafico.dispersion <- tabPanel(title = "Dispersión", value = "tabDisp",
                               plotOutput('plot.disp', height = "65vh", brush = brushOpts(id = "zoom.disp", resetOnNew = TRUE)))

pagina.dispersion<- tabItem(tabName = "dispersion",
                            fluidRow(tabBox(id = "BoxDisp", width = 8,
                                            title = opciones.dispersion,
                                            grafico.dispersion),
                                     datos.dispersiones),
                            fluidRow(codigo.dispersion))

# PAGINA DE CORRELACIONES -------------------------------------------------------------------------------------------------

opciones.correlaciones <- dropdownButton(h4("Opciones"),
                                         selectInput(inputId = "cor.metodo", label = "Seleccionar Método",
                                                     choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                                         selectInput(inputId = "cor.tipo", label = "Seleccionar Tipo", choices =  c("lower", "upper", "full")),
                                         circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                         tooltip = tooltipOptions(title = "Clic para ver opciones"))

tab.correlacion <- tabPanel(title = 'Correlación', value = "correlacion",
                            plotOutput('plot.cor', height = "67vh"),
                            fluidRow(column(width = 4,aceEditor("fieldModelCor", height = "6vh", mode = "r",
                                                                theme = "monokai", value = "", readOnly = T)),
                                     column(width = 8,campo.codigo(runid = "run.code.cor",fieldid =  "fieldCodeCor", height = "6vh"))))

tab.codigo.correlaciones <- tabPanel(title = 'Resultados Numéricos', value = "cor.salida", verbatimTextOutput("txtcor"))

pagina.correlaciones <- tabItem(tabName = "correlacion",
                                fluidRow(tabBox(id = "tabCor", width = 12,
                                                          title = opciones.correlaciones,
                                                          tab.correlacion,
                                                          tab.codigo.correlaciones)))

# PAGINA DE DISTRIBUCIONES -------------------------------------------------------------------------------------------------

boton.codigo.distribuciones <- dropdownButton(h4("Código"),
                                              h5("Grafico de la Distribución (Numéricas)"),
                                              aceEditor("fieldFuncNum", mode = "r", theme = "monokai", value = "", height = "20vh", autoComplete = "enabled"),
                                              h5("Grafico de la Distribución (Categóricas)"),
                                              aceEditor("fieldFuncCat", mode = "r", theme = "monokai", value = "", height = "20vh", autoComplete = "enabled"),
                                              circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                              tooltip = tooltipOptions(title = "Clic para ver el código"))

boton.opciones.distribuciones <- dropdownButton(h4("Opciones"), colourpicker::colourInput("col.dist", "Seleccionar Color:", value = "#0D00FFAA", allowTransparent = T),
                                                circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                tooltip = tooltipOptions(title = "Clic para ver opciones"))

selector.variables.distribucion <- column(width = 7,tags$div(class = "select-var-ind",
                                                             conditionalPanel( condition = "input.tabDyA == 'numericas'",
                                                                               selectInput(inputId = "sel.distribucion.num", label = NULL, choices =  "")),
                                                             conditionalPanel( condition = "input.tabDyA == 'categoricas'",
                                                                               selectInput(inputId = "sel.distribucion.cat", label = NULL, choices =  "") )))

resultados.distribucion.numericas <- tabPanel(title = 'Numéricas', value = "numericas",
                                              plotOutput('plot.num', height = "65vh"),
                                              fluidRow(column(width = 6, campo.codigo(runid = "run.dya.num",fieldid = "fieldCodeNum", height = "8vh")),
                                                       column(width = 6, DT::dataTableOutput("mostrar.atipicos"))) )

resultados.distribucion.categoricas <- tabPanel(title = 'Categóricas', value = "categoricas", plotOutput('plot.cat', height = "76vh"),
                                                campo.codigo(runid = "run.dya.cat", fieldid = "fieldCodeCat", height = "6vh"))

pagina.distribuciones <- tabItem(tabName = "distribucion",
                                 fluidRow(tabBox(id = "tabDyA", width = 12,
                                               title = fluidRow(
                                                 selector.variables.distribucion,
                                                 column(width = 2, boton.codigo.distribuciones),
                                                 column(width = 2, boton.opciones.distribuciones)),
                                               resultados.distribucion.numericas,
                                               resultados.distribucion.categoricas )))


# PAGINA DE PODER PREDICTIVO ----------------------------------------------------------------------------------------------

plot.pred.poder <- tabPanel(title = 'Distribución Variable a Predecir',
                            plotOutput('plot.pred.poder', height = "55vh"),
                            campo.codigo(runid = "run.code.poder.pred", fieldid = "fieldCodePoderPred", height = "16vh"))

plot.dist.poder <- tabPanel(title = 'Distribución Variables Categóricas Según Variable a Predecir',
                            plotOutput('plot.dist.poder', height = "55vh"),
                            selectInput(inputId = "sel.distribucion.poder", label = NULL, choices =  "", width = "100%"),
                            campo.codigo(runid = "run.code.poder.cat", fieldid = "fieldCodePoderCat", height = "16vh"))

plot.pairs.poder <- tabPanel(title = 'Gráfico de Pares',
                             plotOutput('plot.pairs.poder', height = "55vh"),
                             campo.codigo(runid = "run.code.poder.num", fieldid = "fieldCodePoderNum", height = "16vh"))

plot.dens.poder <- tabPanel(title = 'Densidad Variables Numéricas Según Variable a Predecir',
                             plotOutput('plot.density.poder', height = "55vh"),
                             selectInput(inputId = "sel.density.poder", label = NULL, choices =  "", width = "100%"),
                             campo.codigo(runid = "run.code.poder.dens", fieldid = "fieldCodePoderDens", height = "16vh"))


pagina.poder <- tabItem(tabName = "poderPred",
                        fluidRow(tabBox(width = 12,
                               plot.pred.poder,
                               plot.pairs.poder,
                               plot.dist.poder,
                               plot.dens.poder)))

# PAGINA DE KNN -----------------------------------------------------------------------------------------------------------

panel.generar.knn <- tabPanel(title = "Generación del Modelo",
                             verbatimTextOutput("txtknn"),
                             aceEditor("fieldCodeKnn", mode = "r", theme = "monokai", value = "", height = "4vh", readOnly = F))

panel.prediccion.knn <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("knnPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeKnnPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.knn <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.knn.mc', height = "45vh"),
                                       verbatimTextOutput("txtknnMC"),
                                       aceEditor("fieldCodeKnnMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.knn <- tabPanel(title = "Índices Generales",
                                    fluidRow(column(width = 6, gaugeOutput("knnPrecGlob", width = "100%")),
                                             column(width = 6, gaugeOutput("knnErrorGlob", width = "100%"))),
                                    fluidRow(column(width = 12, shiny::tableOutput("knnIndPrecTable"))),
                                    fluidRow(column(width = 12, shiny::tableOutput("knnIndErrTable"))),
                                    aceEditor("fieldCodeKnnIG", mode = "r", theme = "monokai",
                                              value = "", height = "37vh", readOnly = F, autoComplete = "enabled"))

opciones.knn <- fluidRow(column(width = 6, actionButton("runKnn", label = "Ejecutar", icon = icon("play"))),
                         column(width = 6, dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               switchInput(inputId = "switch.scale.knn", onStatus = "success", offStatus = "danger", value = T,
                                           label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               numericInput("kmax.knn", "K Máximo: ", min = 1,step = 1, value = 7),
                               selectInput(inputId = "kernel.knn", label = "Seleccionar un Kernel",selected = 1,
                                           choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                        "triweight", "cos","inv","gaussian")))))


titulo.knn <- fluidRow(column(width = 12, opciones.knn))

pagina.knn <- tabItem(tabName = "knn",
                      fluidRow(tabBox(width = 12, title = titulo.knn,
                             panel.generar.knn,
                             panel.prediccion.knn,
                             panel.matriz.confucion.knn,
                             panel.indices.generales.knn)))


# PAGINA DE SVM -----------------------------------------------------------------------------------------------------------

panel.generar.svm <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtSvm"),
                              aceEditor("fieldCodeSvm", mode = "r", theme = "monokai",
                                        value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.svm <- tabPanel(title = "Gráfico Clasificación",
                     plotOutput('plot.svm', height = "55vh"),
                     hr(),
                     selectizeInput("select.var.svm.plot",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                    options = list(maxItems = 2, placeholder = "Seleccione la(s) variable(s) predictoras"), width = "100%"),
                     aceEditor("fieldCodeSvmPlot", mode = "r", theme = "monokai",
                               value = "", height = "5vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.svm <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("svmPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeSvmPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.svm <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.svm.mc', height = "45vh"),
                                       verbatimTextOutput("txtSvmMC"),
                                       aceEditor("fieldCodeSvmMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.svm <- tabPanel(title = "Índices Generales",
                                        fluidRow(column(width = 6, gaugeOutput("svmPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("svmErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("svmIndPrecTable"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("svmIndErrTable"))),
                                        aceEditor("fieldCodeSvmIG", mode = "r", theme = "monokai",
                                                  value = "", height = "37vh", readOnly = F, autoComplete = "enabled"))

opciones.svm <- fluidRow(column(width = 6, actionButton("runSvm", label = "Ejecutar", icon = icon("play"))),
                         column(width = 6,
                                dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                                               switchInput(inputId = "switch.scale.svm", onStatus = "success", offStatus = "danger", value = T,
                                                           label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                                               selectInput(inputId = "kernel.svm", label = "Seleccionar un Kernel", selected = "radial",
                                                           choices =  c("linear", "polynomial", "radial", "sigmoid")))))

titulo.svm <- fluidRow(column(width = 12, opciones.svm))

pagina.svm <- tabItem(tabName = "svm",
                      fluidRow(tabBox(width = 12, title = titulo.svm,
                             panel.generar.svm,
                             plot.svm,
                             panel.prediccion.svm,
                             panel.matriz.confucion.svm,
                             panel.indices.generales.svm)))

# PAGINA DE DT ------------------------------------------------------------------------------------------------------------

panel.generar.dt <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtDt"),
                              aceEditor("fieldCodeDt", mode = "r", theme = "monokai",
                                        value = "", height = "5vh", readOnly = F, autoComplete = "enabled"))

plot.dt <- tabPanel(title = "Gráfico Árbol",
                     plotOutput('plot.dt', height = "55vh"),
                     hr(),
                     aceEditor("fieldCodeDtPlot", mode = "r", theme = "monokai",
                               value = "", height = "6vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.dt <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("dtPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeDtPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.dt <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.dt.mc', height = "45vh"),
                                       verbatimTextOutput("txtDtMC"),
                                       aceEditor("fieldCodeDtMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.dt <- tabPanel(title = "Índices Generales",
                                       fluidRow(column(width = 6, gaugeOutput("dtPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("dtErrorGlob", width = "100%"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("dtIndPrecTable"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("dtIndErrTable"))),
                                        aceEditor("fieldCodeDtIG", mode = "r", theme = "monokai",
                                                  value = "", height = "37vh", readOnly = F, autoComplete = "enabled"))

panel.reglas.dt <- tabPanel(title = "Reglas",verbatimTextOutput("rulesDt"))


opciones.dt <- fluidRow(column(width = 6, actionButton("runDt", label = "Ejecutar", icon = icon("play"))),
                        column(width = 6,
                               dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                             tooltip = tooltipOptions(title = "Clic para ver opciones"),
                                             numericInput("minsplit.dt", "Mínimo para dividir un nodo:", 20, width = "100%",min = 1),
                                             numericInput("maxdepth.dt", "Profundidad Máxima:", 15, width = "100%",min = 0, max = 30, step = 1),
                                             selectInput(inputId = "split.dt", label = "Índice de división:",selected = 1,
                                                         choices =  list("gini" = "gini", "Entropía" = "information")))

                               ))


titulo.dt <- fluidRow(column(width = 12, opciones.dt))

pagina.dt <- tabItem(tabName = "dt",
                     fluidRow(tabBox(width = 12, title = titulo.dt,
                            panel.generar.dt,
                            plot.dt,
                            panel.prediccion.dt,
                            panel.matriz.confucion.dt,
                            panel.indices.generales.dt,
                            panel.reglas.dt)))

# PAGINA DE RF ------------------------------------------------------------------------------------------------------------

panel.generar.rf <- tabPanel(title = "Generación del Modelo",
                             verbatimTextOutput("txtRf"),
                             aceEditor("fieldCodeRf", mode = "r", theme = "monokai",
                                       value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plor.error.ft <- tabPanel(title = "Evolución del Error",
                          plotOutput('plot.error.rf', height = "55vh"),
                          hr(),
                          aceEditor("fieldCodeRfPlotError", mode = "r", theme = "monokai",
                                    value = "", height = "5vh", readOnly = F, autoComplete = "enabled"))

plot.rf <- tabPanel(title = "Importancia de Variables",
                     plotOutput('plot.rf', height = "55vh"),
                     hr(),
                     aceEditor("fieldCodeRfPlot", mode = "r", theme = "monokai",
                               value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.rf <- tabPanel(title = "Predicción del Modelo",
                                DT::dataTableOutput("rfPrediTable"),
                                hr(),
                                aceEditor("fieldCodeRfPred", mode = "r", theme = "monokai",
                                          value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.rf <- tabPanel(title = "Matriz de Confusión",
                                      plotOutput('plot.rf.mc', height = "45vh"),
                                      verbatimTextOutput("txtRfMC"),
                                      aceEditor("fieldCodeRfMC", mode = "r", theme = "monokai",
                                                value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.rf <- tabPanel(title = "Índices Generales",
                                       fluidRow(column(width = 6, gaugeOutput("rfPrecGlob", width = "100%")),
                                                column(width = 6, gaugeOutput("rfErrorGlob", width = "100%"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rfIndPrecTable"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rfIndErrTable"))),
                                       aceEditor("fieldCodeRfIG", mode = "r", theme = "monokai",
                                                 value = "", height = "37vh", readOnly = F, autoComplete = "enabled"))


reglas.rf <- tabPanel(title = "Reglas",
                      verbatimTextOutput("rulesRf"),
                      numericInput("rules.rf.n","Reglas del árbol número:",1, width = "100%", min = 1))

opciones.rf <- fluidRow(column(width = 6,actionButton("runRf",label = "Ejecutar", icon = icon("play"))),
                        column(width = 6, dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                                        tooltip = tooltipOptions(title = "Clic para ver opciones"),
                                                        numericInput("ntree.rf", "Número de Árboles:", 20, width = "100%", min = 0),
                                                        numericInput("mtry.rf","Número de variables:",1, width = "100%", min = 1) )))

titulo.rf <- fluidRow(column(width = 12, opciones.rf))

pagina.rf <- tabItem(tabName = "rf",
                     fluidRow(tabBox(width = 12, title = titulo.rf,
                            panel.generar.rf,
                            plor.error.ft,
                            plot.rf,
                            panel.prediccion.rf,
                            panel.matriz.confucion.rf,
                            panel.indices.generales.rf,
                            reglas.rf)))

# PAGINA DE BOOSTING ------------------------------------------------------------------------------------------------------

panel.generar.boosting <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtBoosting"),
                              aceEditor("fieldCodeBoosting", mode = "r", theme = "monokai",
                                        value = "", height = "4vh", readOnly = F, autoComplete = "enabled"))

plot.boosting <- tabPanel(title = "Evolución del Error",
                                 plotOutput('plot.boosting', height = "55vh"),
                                 hr(),
                                 aceEditor("fieldCodeBoostingPlot", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.boosting.import <- tabPanel(title = "Importancia de Variables",
                    plotOutput('plot.boosting.import', height = "55vh"),
                    hr(),
                    aceEditor("fieldCodeBoostingPlotImport", mode = "r", theme = "monokai",
                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.boosting <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("boostingPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeBoostingPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.boosting <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.boosting.mc', height = "45vh"),
                                       verbatimTextOutput("txtBoostingMC"),
                                       aceEditor("fieldCodeBoostingMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.boosting <- tabPanel(title = "Índices Generales",
                                        fluidRow(column(width = 6, gaugeOutput("boostingPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("boostingErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("boostingIndPrecTable"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("boostingIndErrTable"))),
                                        aceEditor("fieldCodeBoostingIG", mode = "r", theme = "monokai",
                                                  value = "", height = "37vh", readOnly = F, autoComplete = "enabled"))

reglas.boosting <- tabPanel(title = "Reglas",
                      verbatimTextOutput("rulesB"),
                      numericInput("rules.b.n","Reglas del árbol número:",1, width = "100%", min = 1))

opciones.boosting <- fluidRow(column(width = 6, actionButton("runBoosting",label = "Ejecutar", icon = icon("play"))),
                              column(width = 6, dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                                                               numericInput("iter.boosting", "Número de Árboles:", 50, width = "100%",min = 1),
                                                               selectInput(inputId = "tipo.boosting", label = "Seleccionar algoritmo",selected = 1,
                                                                           choices =  c("discrete", "real", "gentle")),
                                                               numericInput("maxdepth.boosting", "Profundidad Máxima:", 15, width = "100%",min = 1),
                                                               numericInput("minsplit.boosting", "Mínimo para dividir un nodo:", 20, width = "100%",min = 1))))

titulo.boosting <- fluidRow(column(width = 12,opciones.boosting))

pagina.boosting <- tabItem(tabName = "boosting",
                           fluidRow(tabBox(width = 12, title = titulo.boosting,
                                  panel.generar.boosting,
                                  plot.boosting,
                                  plot.boosting.import,
                                  panel.prediccion.boosting,
                                  panel.matriz.confucion.boosting,
                                  panel.indices.generales.boosting,
                                  reglas.boosting)))

# PAGINA DE COMPARACION DE MODELOS ---------------------------------------------------------------------------------------

panel.comparacion.tabla <- tabPanel(title = "Tabla Comparativa",
                                    DT::dataTableOutput("TablaComp"))

plot.comparacion.roc <- tabPanel(title = "Curva ROC",
                          plotOutput('plot.roc', height = "45vh"),
                          fluidRow(column(width = 12, selectInput(inputId = "roc.sel",
                                                                  label = h4("Seleccionar la Categoría:"),
                                                                  choices =  "", width = "100%"))))


selector.modelos <- fluidRow(box(title = "Mostrar Modelos:", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        checkboxGroupButtons("select.models", "", c("No Disponible" = "NoDisponible"),
                                         size = "sm", status = "primary",
                                         checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                          no = icon("remove", lib = "glyphicon")))))

pagina.comparacion <- tabItem(tabName = "comparar",
                              fluidRow(tabBox(width = 12,
                                       panel.comparacion.tabla,
                                       plot.comparacion.roc )),
                              fluidRow(column(width = 12,selector.modelos)))

# PAGINA DE PREDICCIONES NUEVAS ---------------------------------------------------------------------------------------

muestra.datos.pred <- box(title = "Datos", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                          DT::DTOutput('contentsPred'), type = 7, color = "#CBB051")

muestra.datos.pred2 <- box(title = "Datos", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                           DT::DTOutput('contentsPred2'), type = 7, color = "#CBB051")

muestra.datos.pred3 <- box(title = "Datos", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                           DT::DTOutput('contentsPred3'), type = 7, color = "#CBB051")

panel.cargar.datos.pred <- tabPanel(title = "Cargar Datos", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               fluidRow(column(width = 5,
                               checkboxInput('headerNPred', 'Encabezado (Header)', TRUE),
                               checkboxInput('rownameNPred', 'Incluir nombre de filas', TRUE),
                               radioButtons('sepNPred', 'Separador', c(Coma=',', 'Punto y Coma'=';', Tab='\t'), selected = 'Coma'),
                               radioButtons('decNPred', 'Separador Decimal', c('Punto'='.', 'Coma'=","), selected = 'Punto'),
                               switchInput(inputId = "deleteNAnPred", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                           label = "Eliminar NA", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               fileInput('file2', label = 'Cargar Archivo', placeholder = "", buttonLabel = "Subir", width = "100%",
                                         accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
                               actionButton("loadButtonNPred", "Cargar", width = "100%")),
                               column(width = 7, muestra.datos.pred)))


panel.tansformar.datos <- tabPanel(title = "Transformar Datos", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                   fluidRow(column(width = 5,
                                                   DT::dataTableOutput('transDataPredN'),
                                                   br(),br(),
                                                   actionButton("transButtonPredN", "Aplicar", width = "100%")),
                                   column(width = 7, muestra.datos.pred2)))

panel.cargar.datos.pred2 <- tabPanel(title = "Cargar Individuos Nuevos", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                    fluidRow(column(width = 5,
                                                    checkboxInput('headerNPred2', 'Encabezado (Header)', TRUE),
                                                    checkboxInput('rownameNPred2', 'Incluir nombre de filas', TRUE),
                                                    radioButtons('sep.nPred2', 'Separador', c(Coma=',', 'Punto y Coma'=';', Tab='\t'), selected = 'Coma'),
                                                    radioButtons('dec.nPred2', 'Separador Decimal', c('Punto'='.', 'Coma'=","), selected = 'Punto'),
                                                    switchInput(inputId = "deleteNAnPred2", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                                                label = "Eliminar NA", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                                                    fileInput('file3', label = 'Cargar Archivo', placeholder = "", buttonLabel = "Subir", width = "100%",
                                                              accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
                                                    actionButton("loadButtonNPred2", "Cargar", width = "100%")),
                                             column(width = 7, muestra.datos.pred3)))

opciones.knn.pred <- fluidRow(column(width = 4, br() , switchInput(inputId = "switch.scale.knn.pred", onStatus = "success", offStatus = "danger", value = T,
                                                              label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%", width = "100%")),
                              column(width = 4, numericInput("kmax.knn.pred", "K Máximo: ", min = 1,step = 1, value = 7,width="100%")),
                              column(width = 4, selectInput(inputId = "kernel.knn.pred", label = "Seleccionar un Kernel",selected = 1, width="100%",
                                                                      choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                                                   "triweight", "cos","inv","gaussian"))))

opciones.svm.pred <- fluidRow(column(width = 6, br(), switchInput(inputId = "switch.scale.svm.pred", onStatus = "success", offStatus = "danger", value = T,
                                                           label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%", width = "100%")),
                         column(width = 6, selectInput(inputId = "kernel.svm.pred", label = "Seleccionar un Kernel", selected = "radial", width="100%",
                                                           choices =  c("linear", "polynomial", "radial", "sigmoid"))))

opciones.dt.pred <- fluidRow(column(width = 4, numericInput("minsplit.dt.pred", "Mínimo para dividir un nodo:", 20, width = "100%",min = 1)),
                             column(width = 4, numericInput("maxdepth.dt.pred", "Profundidad Máxima:", 15, width = "100%",min = 0, max = 30, step = 1)),
                             column(width = 4,selectInput(inputId = "split.dt.pred", label = "Índice de división:",selected = 1,width = "100%",choices =  c("gini", "information"))))

opciones.rf.pred <- fluidRow(column(width = 6, numericInput("ntree.rf.pred", "Número de Árboles:", 20, width = "100%", min = 0)),
                             column(width = 6, numericInput("mtry.rf.pred","Número de variables:",1, width = "100%", min = 1)))

opciones.boosting.pred <- list(fluidRow(column(width = 3, numericInput("iter.boosting.pred", "Número de iteraciones:", 50, width = "100%",min = 1)),
                                   column(width = 3, numericInput("maxdepth.boosting.pred", "Profundidad Máxima:", 15, width = "100%",min = 1)),
                                   column(width = 3, numericInput("minsplit.boosting.pred", "Mínimo para dividir un nodo:", 20, width = "100%",min = 1)),
                                   column(width = 3, selectInput(inputId = "tipo.boosting.pred", label = "Seleccionar algoritmo",selected = 1, width = "100%",
                                                                 choices =  c("discrete", "real", "gentle")))))

panel.crear.modelo.pred <- tabPanel(title = "Selección y Parametrización del Modelo",solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                    selectInput(inputId = "sel.predic.var.nuevos", label = h4("Seleccionar Variable a Predecir:"), choices =  "", width = "100%"),
                                    hr(),
                                    radioGroupButtons("selectModelsPred", "", list("K Vecinos Más Cercanos" = "knn",
                                                                                        "Árboles de Decisión" = "dt",
                                                                                        "Bosques Aleatorios" = "rf",
                                                                                        "ADA-Boosting" = "ada",
                                                                                        "Soporte Vectorial" = "svm"),
                                                         size = "sm", status = "primary",individual = FALSE, justified = TRUE, selected = "knn",
                                                         checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                          no = icon("remove", lib = "glyphicon"))),
                                    hr(),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'knn'",
                                                     opciones.knn.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'dt'",
                                                     opciones.dt.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rf'",
                                                     opciones.rf.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'ada'",
                                                     opciones.boosting.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'svm'",
                                                     opciones.svm.pred),
                                    aceEditor("fieldPredNuevos", mode = "r", theme = "monokai", value = "", height = "5vh", readOnly = F),
                                    verbatimTextOutput("txtPredNuevos"),
                                    actionButton("PredNuevosBttnModelo","Generar Modelo", width  = "100%" ))


panel.prediccion.knn <- tabPanel(title = "Predicción de Nuevos Individuos",
                                 DT::dataTableOutput("PrediTablePN"),
                                 hr(),
                                 downloadButton("downloaDatosPred", "Descargar Datos", width = "100%"),
                                 hr(),
                                 aceEditor("fieldCodePredPN", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

pagina.predicciones.nuevas <- tabItem(tabName = "predNuevos",
                                      fluidRow(tabBox(width = 12,
                                               panel.cargar.datos.pred,
                                               panel.tansformar.datos,
                                               panel.crear.modelo.pred,
                                               panel.cargar.datos.pred2,
                                               panel.prediccion.knn)))

# PAGINA DE REPORTE -------------------------------------------------------------------------------------------------------

panel.reporte.encabezado <- column(width = 5, box(title = "Reporte", width = 12,
                                              textInput("textTitulo", value = "Sin Titulo", width = "100%", label = "Digite el Titulo:"),
                                              textInput("textNombre", value = "PROMiDAT", width = "100%", label = "Digite su Nombre:"),
                                              downloadButton("descargar", "Descargar", class = "center-button")))

panel.reporte.codigo <- column(width = 7,box(title = "Código Reporte", width = 12, height = "50vh",status = "primary", solidHeader = TRUE,
                                             collapsible = TRUE, aceEditor("fieldCodeReport", mode="markdown", value='', height = "43vh")))

panel.reporte.salida <- fluidRow(column(width = 12, box(title = "Salida R", width = 12, height = "35vh", verbatimTextOutput("txtreport"))))


pagina.generar.reporte <- tabItem(tabName = "reporte", panel.reporte.encabezado , panel.reporte.codigo, panel.reporte.salida)

# PAGINA DE INFORMACION ---------------------------------------------------------------------------------------------------

pagina.info <- tabItem(tabName = "acercaDe",
                       img(src="Logo.png", style="padding-bottom:20px;margin-left: auto;margin-right: auto;display: block;width: 50%;"),
                       infoBoxPROMiDAT("Todos los derechos reservados a", "PROMiDAT S.A.", icono = icon("copyright")),
                       infoBoxPROMiDAT("Versión del Sistema", "1.0.8", icono = icon("file-code-o")))

# PAGINA COMPLETA ---------------------------------------------------------------------------------------------------------

shinyUI(dashboardPage(title="PROMiDAT - PredictoR",
                      dashboardHeader(title = tags$a(href="http://promidat.com", target = "_blank",
                                                     img(src="Logo2.png", height=55, width="100%", style="padding-top:2px; padding-bottom:6px;"))),
                      dashboardSidebar(mi.menu),
                      dashboardBody(mi.head,
                                    mi.titulo,
                                    load.page,
                                    tabItems( pagina.cargar.datos,
                                              pagina.resumen.numerico,
                                              pagina.test.normalidad,
                                              pagina.dispersion,
                                              pagina.correlaciones,
                                              pagina.distribuciones,
                                              pagina.poder,
                                              pagina.knn,
                                              pagina.svm,
                                              pagina.dt,
                                              pagina.rf,
                                              pagina.boosting,
                                              pagina.comparacion,
                                              pagina.predicciones.nuevas,
                                              pagina.generar.reporte,
                                              pagina.info))) )
