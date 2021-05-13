# -------------------  RF

cod.rf.modelo <<-  NULL
cod.rf.pred   <<-  NULL
cod.rf.mc     <<- NULL
cod.rf.ind    <<- NULL
rf.stop.excu  <<- FALSE

# Pagina de RF --------------------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree   <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo  <- paste0("modelo.rf <<- train.randomForest(",variable.pr,"~., data = datos.aprendizaje,importance = TRUE,",
                    " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

rf.modelo.np <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree  <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0("modelo.nuevos <<- train.randomForest(",variable.pr,"~., data = datos.aprendizaje.completos,importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

#Codigo de la prediccion de rf
rf.prediccion <- function() {
  return(paste0("prediccion.rf <<- predict(modelo.rf, datos.prueba, type = 'class')"))
}

rf.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')"))
}

#Codigo de la matriz de confucion de rf
rf.MC <- function(){
  return(paste0("MC.rf <<- confusion.matrix(datos.prueba, prediccion.rf)\n"))
}

#Codigo del grafico de importancia de variables
rf.plot <- function() {
  return(paste0(
    "aux <- data.frame(modelo.rf$importance)\n",
    "aux <- aux[order(aux$MeanDecreaseAccuracy, decreasing = T), ]\n",
    "aux$label <- row.names(aux)\n\n",
    "aux %>% e_charts(label) %>% e_bar(MeanDecreaseAccuracy, name = var) %>% \n",
    "  e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading() %>% \n",
    "  e_flip_coords() %>%\n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}

# V2 COLOR
# datos.rf<- data.frame(modelo.rf$importance)
# datos.rf<- datos.rf[order(datos.rf$MeanDecreaseAccuracy, decreasing = T), ]
# datos.rf$label <- row.names(datos.rf)
# datos.rf$color <- gg_color_hue(length(row.names(datos.rf)))
# datos.rf %>% e_charts(label) %>% e_bar(MeanDecreaseAccuracy, name = var) %>%
#   e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()%>%
#   e_flip_coords()%>%
#   e_y_axis(inverse = TRUE)%>%
#   e_add("itemStyle", color)


#Codigo del grafico de error del modelo
plot.rf.error <- function(){
  return(paste0("data     <- data.frame(x = c(1:length(modelo.rf$err.rate[,1])),cbind(modelo.rf$err.rate)) \n",
                "e_rf_error(data)\n"))
}

e_rf_error <- function(dataplot) {
  categorias   <- colnames(dataplot[,-c(1,2)])
  aux <- ''
  for (i in 1:length(categorias)) {
    aux <- paste0(aux, "e_line(serie = ",categorias[i],", lineStyle = list(type = 'dashed')) %>% \n ")
  }
  plot <- paste0("dataplot %>% \n",
         "e_charts(x = x) %>% \n",
         "e_line(serie = OOB, showSymbol = FALSE) %>% \n",
         aux,
         "e_legend(orient = 'vertical',
             right = '20', top = '10%') %>% \n",
         "e_axis_labels(
            x = 'Trees',
            y = 'Error') %>%  \n",
         "e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading() \n"
  )
  exe(plot)
}
# 
# datata %>%
#   e_charts(x = x) %>%
#   e_line(serie = OOB,lineStyle = list(type = 'dashed', color = 'black'), showSymbol = FALSE) %>%
#   e_title('Ensemble error vs number or trees',
#           left = 'center',
#           top = 5,
#           textStyle = list(fontSize = 15)) %>%
#   e_legend(orient = 'vertical',
#            right = '20', top = '10%') %>%
#   e_axis_labels(
#     x = 'Iterations',
#     y = 'Error'
#   )%>%  e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
