# Pagina de RLR -------------------------------------------------------------------------------------------------------------

#Crea el modelo RLR
rlr.modelo <- function(variable.pr = NULL, type = "ridge", alpha = 0, escalar = TRUE){
  return(paste0("modelo.rlr.",type,"<<- train.glmnet(",variable.pr,"~., data = datos.aprendizaje, standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')"))
}

#Código de la prediccion de rlr
rlr.prediccion <- function(type = "ridge") {
  return(paste0("prediccion.rlr.",type," <<- predict(modelo.rlr.",type,", datos.prueba, type = 'class')"))
}

#Código de la matriz de confución de rlr
rlr.MC <- function(type = "ridge"){
  return(paste0("MC.rlr.",type," <<- confusion.matrix(datos.prueba, prediccion.rlr.",type,")","\n"))
}

select.landa <- function(variable.pr = NULL, alpha = 0, escalar = TRUE, type = "ridge"){
  paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.pr,"']\n",
         "cv.glm.",type," <<- glmnet::cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')\n",
         "plot(cv.glm)")
}

#Crea el gráfico de posibles lambda para rlr
#e_posib_lambda(exe("modelo.rlr.",tipo), exe("cv.glm.",tipo))
e_posib_lambda <- function(cv.glm){
  cv.glm <- cv.glm.lasso
  x  <- log(cv.glm$lambda)
  y  <- cv.glm$cvm
  x1 <- x[cv.glm$index[[1]]]
  x2 <- x[cv.glm$index[[2]]]
  upper <- cv.glm$cvup
  lower <- cv.glm$cvlo
  name  <- cv.glm$name[[1]]
  data.lambda <- data.frame(x, y, upper, lower, name)
  plot <- data.lambda %>%
    e_charts(x) %>%
    e_scatter(y, symbol_size = 7) %>%
    e_error_bar(lower, upper) %>%
    e_mark_line(data = list(xAxis = x1)) %>%
    e_mark_line(data = list(xAxis = x2)) %>%
    e_axis_labels(
      x = 'Log(λ)',
      y = name)%>%
    e_x_axis(
      formatter = e_axis_formatter(digits = 1))  %>% 
    e_legend(FALSE) %>% 
    e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
  plot$x$opts$xAxis[[1]]$type <- "value"
  plot
}

#Gráfico de coeficientes lambda RLR
e_coeff_landa <- function(modelo, category, lambda = NULL) {
  plot.rlr.coeff <<- data.frame(t(as.data.frame(as.matrix(modelo$beta[[category]]))))
  plot.rlr.coeff <<- cbind(x = log(modelo$lambda), plot.rlr.coeff)
  plot.rlr.coeff <<- plot.rlr.coeff[order(plot.rlr.coeff$x),]
  predictoras    <-  colnames(plot.rlr.coeff)
  
  for (i in 2:length(predictoras)) {
    exe(paste0("plot.rlr.coeff$n",i," <<- '",predictoras[i], "'\n"))
  }
  
  nombres <- colnames(plot.rlr.coeff)
  aux <- ''
  n <- length(predictoras) + 1
  for (i in 2:length(predictoras)) {
    aux <- paste0(aux, "e_line(serie = ",predictoras[i],", bind = ",nombres[n],") %>%\n ")
    n <- n +1
  }
  plot <- paste0("plot.rlr.coeff %>% \n",
                 "e_charts(x = x) %>% \n",
                 aux,
                 "e_axis_labels(
                    x = 'Log Lambda',
                    y = 'Coefficients: Response ",category,"') %>%  \n",
                 "e_legend(show = FALSE)%>%  \n",
                 "e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading() "
  )
  plot <- exe(plot)
  plot  %>% 
    e_labels(position = 'left',formatter = htmlwidgets::JS("
                                        function(params){
                                        if(params.dataIndex==0){
                                        return(params.name)
                                        }else
                                        {return('')}}"))
}
# Códigos de RLR Ind.Nuevos--------------------------------------------------------------------------------------------------

#Crea el modelo RLR
rlr.modelo.np <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  return(paste0("modelo.nuevos <<- train.glmnet(",variable.pr,"~., data = datos.aprendizaje.completos, standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')"))
}

#Código de la prediccion de rlr
rlr.prediccion.np <- function() {
  return(paste0("datos.prueba.aux <<- datos.prueba.completos\n",
                "datos.prueba.aux[['",variable.predecir.np,"']]<- as.factor(levels(datos.aprendizaje.completos[['",variable.predecir.np,"']]))\n",
                "predic.nuevos <<- predict(modelo.nuevos, datos.prueba.aux, type = 'class')"))
}

