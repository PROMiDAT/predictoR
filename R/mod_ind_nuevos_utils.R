# -------------------  Prediccion Nuevos
datos.originales.completos  <<- NULL
datos.aprendizaje.completos <<- NULL
datos.prueba.completos      <<- NULL
variable.predecir.np        <<- NULL

modelo.seleccionado.pn <<- NULL
contadorPN    <<- 0
code.trans.pn <<- ""

modelo.nuevos <<- NULL
predic.nuevos <<- NULL


borrar.datos <- function (newCases, prueba = FALSE){
  if(!prueba){
  newCases$originales        <- NULL
  newCases$datos.aprendizaje <- NULL
  newCases$variable.predecir <- NULL
  }
  newCases$datos        <- NULL
  newCases$datos.prueba <- NULL
}

asignarDatos <- function(newCases){
  variable.predecir.np        <<- newCases$variable.predecir
  datos.originales.completos  <<- newCases$originales
  datos.aprendizaje.completos <<- newCases$datos.aprendizaje
  datos.prueba.completos      <<- newCases$datos.prueba
}

num.categorias.pred.np <- function(variable.predecir){
  return(length(levels(datos.aprendizaje.completos[,variable.predecir])))
}

borrar.datos.modelos.np <- function(){
  modelo.nuevos <<- NULL
  predic.nuevos <<- NULL
}

crear.datos.np <- function(variable.predecir){
  datos.aux.prueba <- datos.prueba.completos
  datos.aux.prueba[,variable.predecir] <- predic.nuevos
  return(datos.aux.prueba)
}