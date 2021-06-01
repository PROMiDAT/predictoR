# -------------------  RF

cod.rf.modelo <<-  NULL
cod.rf.pred   <<-  NULL
cod.rf.mc     <<-  NULL
cod.rf.ind    <<-  NULL
rf.stop.excu  <<-  FALSE

# Códigos de RF--------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree   <- ifelse(!is.numeric(ntree), 500, ntree)
  Código  <- paste0("modelo.rf <<- train.randomForest(",variable.pr,"~., data = datos.aprendizaje,importance = TRUE,",
                    " ntree =",ntree,",mtry =",mtry,")")
  return(Código)
}

#Código de la predicción de rf
rf.prediccion <- function() {
  return(paste0("prediccion.rf <<- predict(modelo.rf, datos.prueba, type = 'class')"))
}

#Código de la matriz de confución de rf
rf.MC <- function(){
  return(paste0("MC.rf <<- confusion.matrix(datos.prueba, prediccion.rf)\n"))
}

#Código del gráfico de importancia de variables
rf.importance.plot <- function() {
  return(paste0(
    "aux <- data.frame(modelo.rf$importance)\n",
    "aux$MeanDecreaseAccuracy <- abs(aux$MeanDecreaseAccuracy)\n",
    "aux <- aux[order(aux$MeanDecreaseAccuracy, decreasing = T), ]\n",
    "aux$label <- row.names(aux)\n\n",
    "aux %>% e_charts(label) %>% e_bar(MeanDecreaseAccuracy, name = var) %>% \n",
    "  e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading() %>% \n",
    "  e_flip_coords() %>%\n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}

#Código del gráfico de error del modelo
plot.rf.error <- function(){
  return(paste0("data     <- data.frame(x = c(1:length(modelo.rf$err.rate[,1])),cbind(modelo.rf$err.rate)) \n",
                "e_rf_error(data)\n"))
}

#Gráfico de evolución del error
e_rf_error <- function(dataplot) {
  categorias   <- colnames(dataplot[,-c(1,2)])
  aux <- ''
  for (i in 1:length(categorias)) {
    aux <- paste0(aux, "e_line(serie = ",categorias[i],", lineStyle = list(type = 'dashed')) %>% \n ")
  }
  plot <- paste0("dataplot     <- data.frame(x = c(1:length(modelo.rf$err.rate[,1])),cbind(modelo.rf$err.rate))\n",
                 "dataplot %>% \n",       
                 "e_charts(x = x) %>% \n",
                 "e_line(serie = OOB) %>% \n",
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

#Gráfico de evolución del error
e_rf_error <- function(dataplot) {
  categorias   <- colnames(dataplot[,-c(1,2)])
  aux <- ''
  for (i in 1:length(categorias)) {
    if(i == length(categorias)){
      aux <- paste0(aux, "e_line(serie = ",categorias[i],", lineStyle = list(type = 'dashed')) \n ")
    }else{
      aux <- paste0(aux, "e_line(serie = ",categorias[i],", lineStyle = list(type = 'dashed')) %>% \n ")
      
    }
  }
  plot.err <<- dataplot %>%        
                 e_charts(x = x) %>%
                 e_line(serie = OOB) %>% 
                 e_legend(orient = 'vertical',
                    right = '20', top = '10%') %>% 
                 e_axis_labels(
                    x = 'Trees',
                    y = 'Error') %>%  
                 e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading() 
  exe(paste0("plot.err %>%  ", aux))
}
