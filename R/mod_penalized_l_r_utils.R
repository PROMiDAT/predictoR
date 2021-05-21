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
#cv.glm.lasso$cvm valor de y 
#cv.glm.lasso$cvup valor de la rayita superior
#cv.glm.lasso$name y label
#x = log(modelo.rlr.lasso$lambda)
#y = cv.glm.lasso$cvm
# data.lambda %>% 
#   e_charts(x) %>% 
#   e_scatter(y) 

# df <- data.frame(
#   x = factor(c(1, 2)),
#   y = c(1, 5),
#   upper = c(1.1, 5.3),
#   lower = c(0.8, 4.3)
# )
# 
# df %>% 
#   e_charts(x) %>% 
#   e_bar(y) %>% 
#   e_error_bar(lower, upper)


plot.coeff.landa <- function(landa = NULL, type = "ridge"){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",type,"$lambda.min"), landa)
  paste0("plot(modelo.rlr.",type,", 'lambda', label = TRUE)\n",
         "abline(v = log(",landa,"), col = 'blue', lwd = 2, lty = 3)")
}

e_coeff_landa <- function(type, category) {
  exe(paste0("plot.rlr.coeff <<- data.frame(t(as.data.frame(as.matrix(modelo.rlr.",type,"$beta$",category,"))))\n",
             "plot.rlr.coeff <<- cbind(x = log(modelo.rlr.",type,"$lambda), plot.rlr.coeff)\n"))
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

