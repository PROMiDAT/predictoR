# -------------------  LR

cod.rlr.modelo <<-  NULL
cod.rlr.pred <<-  NULL
cod.rlr.mc <<- NULL
cod.rlr.ind <<- NULL
cod.select.landa <<- NULL


# Pagina de RLR -------------------------------------------------------------------------------------------------------------


#Crea el modelo RL
rlr.modelo <- function(variable.pr = NULL, type = "ridge", alpha = 0, escalar = TRUE){
  return(paste0("modelo.rlr.",type,"<<- train.glmnet(",variable.predecir,"~., data = datos.aprendizaje, standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')"))
}

rlr.modelo.np <- function(alpha = 0, escalar = TRUE, manual = FALSE, landa = 2){
  landa <- ifelse(manual,"",paste0("cv.glm.nuevos <<- cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')\n"))
  return(paste0("x <- model.matrix(",variable.predecir.pn,"~., datos.aprendizaje.completos)[, -1]\n",
                "y <- datos.aprendizaje.completos[, '",variable.predecir.pn,"']\n",
                landa,
                "modelo.nuevos <<- glmnet(x, y,standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')"))
}

select.landa <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.pr,"']\n",
         "cv.glm.",rlr.type()," <<- cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')")
}

plot.coeff.landa <- function(landa = NULL){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr.type(),"$lambda.min"), landa)
  paste0("plot(modelo.rlr.",rlr.type(),", 'lambda', label = TRUE)\n",
         "abline(v = log(",landa,"), col = 'blue', lwd = 2, lty = 3)")
}

#Codigo de la prediccion de rlr
rlr.prediccion <- function(type = "ridge") {
  return(paste0("prediccion.rlr.",type," <<- predict(modelo.rlr.",type,", datos.prueba, type = 'class')"))
}

rlr.prediccion.np <- function(alpha = 0, escalar = TRUE, manual = FALSE, landa = 2) {
  landa <- ifelse(manual, landa, "cv.glm.nuevos$lambda.min")
  paste0("dp <- datos.prueba.completos\n",
         "dp[, '",variable.predecir.pn,"'] <- 0\n",
         "prueba <- model.matrix(",variable.predecir.pn,"~., dp)[, -1]\n",
         "predic.nuevos <<- predict(modelo.nuevos, prueba,",
         "s = ",landa,", type='class')")
}

#Codigo de la matriz de confucion de rlr
rlr.MC <- function(type = "ridge"){
  return(paste0("MC.rlr.",type," <<- confusion.matrix(datos.prueba, prediccion.rlr.",type,")","\n"))
  }
