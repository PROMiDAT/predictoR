# Códigos de BAYES ---------------------------------------------------------------------------------------------------------

#Crea el modelo Bayes
bayes.modelo <- function(variable.pr = NULL){
  return(paste0("modelo.bayes <<- train.bayes(",variable.predecir,"~., data = datos.aprendizaje)"))
}

#Codigo de la prediccion de Bayes
bayes.prediccion <- function() {
  return(paste0("prediccion.bayes <<- predict(modelo.bayes, datos.prueba, type = 'class')"))
}

#Codigo de la matriz de confucion de Bayes
bayes.MC <- function(){
  return(paste0("MC.bayes <<- confusion.matrix(datos.prueba, prediccion.bayes)","\n"))
}

# Códigos de BAYES Ind.Nuevos--------------------------------------------------------------------------------------------------

#Código del modelo
bayes.modelo.np <- function(variable.pr = ""){
  return(paste0("modelo.nuevos <<- train.bayes(",variable.pr,"~., data = datos.aprendizaje.completos)"))
}

#Código de la predicción
bayes.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')"))
}

