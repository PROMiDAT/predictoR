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
  plot  <- data.lambda %>%
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
  data   <- data.frame(t(as.data.frame(as.matrix(modelo$beta[[category]]))))
  x      <- log(modelo$lambda)
  data   <- cbind(x = x, data)
  data   <- data[order(data$x),]
  lambda <- ifelse(best.lambda %in% data$x, best.lambda, log(cv.glm$lambda.min))
  new    <- data.frame()
  for (nom in colnames(data)[-1]) {
    x      <- data[["x"]]
    y      <- data[[nom]]
    nombre <- nom
    new.   <- data.frame(x = x, y = y, nombre = nombre)
    new    <- rbind(new, new.)
  }
  new %>%
    group_by(nombre) %>%
    e_charts(x) %>%
    e_line(y, bind = nombre)%>%
    e_axis_labels(
      x = 'Log Lambda',
      y = paste0('Coefficients: Response ', category))%>%
    e_mark_line(data = list(xAxis = lambda)) %>%
    e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()%>% 
    e_labels(position = 'left',formatter = htmlwidgets::JS("
                                        function(params){
                                        if(params.dataIndex==0){
                                        return(params.name)
                                        }else
                                        {return('')}}"))%>%
    e_legend(show = FALSE)
}
