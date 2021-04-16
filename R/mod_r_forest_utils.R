# -------------------  RF

cod.rf.modelo <<-  NULL
cod.rf.pred <<-  NULL
cod.rf.mc <<- NULL
cod.rf.ind <<- NULL

rf.stop.excu <<- FALSE

# Pagina de RF --------------------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0("modelo.rf <<- train.randomForest(",variable.pr,"~., data = datos.aprendizaje,importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

rf.modelo.np <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
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
    "aux$nombre <- row.names(aux)\n\n",
    "ggplot(aux, aes(x = MeanDecreaseAccuracy, y = fct_reorder(nombre, MeanDecreaseAccuracy), fill = nombre)) +\n",
    "  geom_bar(stat = 'identity') + labs(y = '') +\n",
    "  theme_minimal()+ theme(legend.position = 'none')\n"
  ))
}

# datos.rf<- data.frame(modelo.rf$importance)
# datos.rf<- datos.rf[order(datos.rf$MeanDecreaseAccuracy, decreasing = T), ]
# datos.rf$label <- row.names(datos.rf)
# datos.rf$color <- gg_color_hue(length(row.names(datos.rf)))
# datos.rf %>% e_charts(label) %>% e_bar(MeanDecreaseAccuracy, name = var) %>%
#   e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()%>%
#   e_flip_coords()%>%
#   e_y_axis(inverse = TRUE)

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
  return(paste0("plot(modelo.rf, main='')\n",
                "legend('topright', c('OOB','",
                paste0(unique(datos[,variable.predecir]), collapse = "','"), "'), text.col=1:6, lty=1:5, col=1:6)"))
}
