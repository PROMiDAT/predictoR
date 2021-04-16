# -------------------  LR

cod.rl.modelo <<-  NULL
cod.rl.pred <<-  NULL
cod.rl.mc <<- NULL
cod.rl.ind <<- NULL

# Pagina de RL --------------------------------------------------------------------------------------------------------------


#Crea el modelo RL
rl.modelo <- function(variable.predecir = NULL){
  return(paste0("modelo.rl <<- train.glm(",variable.predecir,"~., data = datos.aprendizaje, family = binomial)"))
}

rl.modelo.np <- function(variable.pr = ""){
  return(paste0("modelo.nuevos <<- traineR::train.glm(",variable.pr,"~., data = datos.aprendizaje.completos)"))
  #return(paste0("modelo.nuevos <<- train.glm(",variable.predecir.pn,"~., data = datos.aprendizaje.completos, family = binomial)"))
}

#Codigo de la prediccion de rl
rl.prediccion <- function() {
  return(paste0("prediccion.rl <<- predict(modelo.rl, datos.prueba, type = 'class')"))
  
}

rl.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')"))
}

#Codigo de la matriz de confucion de rl
rl.MC <- function(){
  return(paste0("MC.rl <<- confusion.matrix(datos.prueba, prediccion.rl)","\n"))
}
