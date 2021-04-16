# -------------------      BOOSTING

cod.b.modelo <<-  NULL
cod.b.pred <<-  NULL
cod.b.mc <<- NULL
cod.b.ind <<- NULL

# Pagina de BOOSTING --------------------------------------------------------------------------------------------------------


#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, iter = 50, maxdepth = 1, minsplit = 1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  nu <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo <- paste0("modelo.boosting <<- ada(",variable.pr,"~., data = datos.aprendizaje, iter = ",iter,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

boosting.modelo.np <- function(variable.pr = NULL, iter = 50, maxdepth = 1, type = "discrete", minsplit = 1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  nu <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo <- paste0("modelo.nuevos <<- ada(",variable.pr,"~., data = datos.aprendizaje.completos, iter = ",iter,", type = '",type,"',
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

#Codigo de la prediccion de boosting
boosting.prediccion <- function() {
  return(paste0("prediccion.boosting <<- predict(modelo.boosting, datos.prueba, type = 'class')"))
}

boosting.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la matriz de confucion de boosting
boosting.MC <- function(){
  return(paste0("MC.boosting <<- confusion.matrix(datos.prueba, prediccion.boosting)\n"))
}
