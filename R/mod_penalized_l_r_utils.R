# -------------------  LR

cod.rlr.modelo <<-  NULL
cod.rlr.pred   <<-  NULL
cod.rlr.mc     <<- NULL
cod.rlr.ind    <<- NULL
cod.select.landa <<- NULL


# Pagina de RLR -------------------------------------------------------------------------------------------------------------


#Crea el modelo RL
rlr.modelo <- function(variable.pr = NULL, type = "ridge", alpha = 0, escalar = TRUE){
  return(paste0("modelo.rlr.",type,"<<- train.glmnet(",variable.pr,"~., data = datos.aprendizaje, standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')"))
}

rlr.modelo.np <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  return(paste0("modelo.nuevos <<- train.glmnet(",variable.pr,"~., data = datos.aprendizaje.completos, standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')"))
}

select.landa <- function(variable.pr = NULL, alpha = 0, escalar = TRUE, type = "ridge"){
  paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.pr,"']\n",
         "cv.glm.",type," <<- glmnet::cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')")
}


plot.coeff.landa <- function(landa = NULL, type = "ridge"){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",type,"$lambda.min"), landa)
  paste0("plot(modelo.rlr.",type,", 'lambda', label = TRUE)\n",
         "abline(v = log(",landa,"), col = 'blue', lwd = 2, lty = 3)")
}
e_coeff_landa <- function(dataplot, modelo, category) {
  predictoras   <- colnames(dataplot[,-"x"])
  aux <- ''
  for (i in 1:length(predictoras)) {
    aux <- paste0(aux, "e_line(serie = ",predictoras[i],", lineStyle = list(type = 'dashed')) %>% \n ")
  }
  plot <- paste0("dataplot %>% \n",
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

#x <- log(modelo.rlr.lasso$lambda)
#y <-as.data.frame(as.matrix(modelo.rlr.lasso$beta$virginica))   
#yt<- data.frame(t(y))
#yt$x <- x
#ytaux <- yt[order(yt$x),]
# ytaux %>% 
#   e_charts(x) %>% 
#   e_line(s.largo)%>% 
#   e_line(s.ancho)%>% 
#   e_line(p.largo)%>% 
#   e_line(p.ancho)

#Codigo de la prediccion de rlr
rlr.prediccion <- function(type = "ridge") {
  return(paste0("prediccion.rlr.",type," <<- predict(modelo.rlr.",type,", datos.prueba, type = 'class')"))
}

rlr.prediccion.np <- function() {
  return(paste0("datos.prueba.aux <<- datos.prueba.completos\n",
                "datos.prueba.aux[['",variable.predecir.np,"']]<- as.factor(levels(datos.aprendizaje.completos[['",variable.predecir.np,"']]))\n",
                "predic.nuevos <<- predict(modelo.nuevos, datos.prueba.aux, type = 'class')"))
}

#Codigo de la matriz de confucion de rlr
rlr.MC <- function(type = "ridge"){
  return(paste0("MC.rlr.",type," <<- confusion.matrix(datos.prueba, prediccion.rlr.",type,")","\n"))
}
