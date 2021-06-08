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
         "e_posib_lambda(cv.glm.",type,")")
}

#Crea el gráfico de posibles lambda para rlr
#e_posib_lambda(exe("modelo.rlr.",tipo), exe("cv.glm.",tipo))
e_posib_lambda <- function(cv.glm){
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
      x = tr("lambda"),
      y = name)%>%
    e_x_axis(
      formatter = e_axis_formatter(digits = 1))  %>% 
    e_legend(FALSE) %>% 
    e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
  plot$x$opts$xAxis[[1]]$type <- "value"
  plot
}

#Gráfico de coeficientes lambda RLR
e_coeff_landa <- function(modelo, category, best.lambda = NULL, cv.glm) {
  matriz<<- as.matrix(modelo$beta[[category]])
  dataframe<<- as.data.frame(matriz)
  datap <<- t(dataframe)
  plot.rlr.coeff <<- data.frame(datap)
  x <<- log(modelo$lambda)
  plot.rlr.coeff <<- cbind(x = x, plot.rlr.coeff)
  plot.rlr.coeff <<- plot.rlr.coeff[order(plot.rlr.coeff$x),]
  predictoras    <-  colnames(plot.rlr.coeff)
  lambda         <-  ifelse(best.lambda %in% plot.rlr.coeff$x, best.lambda, log(cv.glm$lambda.min))
  
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
                 "e_mark_line(data = list(xAxis = ",lambda,")) %>%",
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
