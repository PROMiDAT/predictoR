
# -------------------  KNN

cod.knn.modelo <<-  NULL
cod.knn.pred <<-  NULL
cod.knn.mc <<- NULL
cod.knn.ind <<- NULL


###############################Generar Codigo##############################################
code.kkn.modelo <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  kmax <- ifelse(!is.numeric(kmax), round(sqrt(nrow(datos.aprendizaje))), kmax)
  library(traineR)
  return(paste0("modelo.knn.",kernel," <<- traineR::train.knn(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion de knn
kkn.prediccion <- function(variable.pr,kernel = "optimal") {
  return(paste0("prediccion.knn.",kernel," <<- predict(modelo.knn.",kernel,", datos.prueba, type = 'class')"))
}

#Codigo de la matriz de confucion de knn
knn.MC <- function(kernel = "optimal"){
  return(paste0("MC.knn.",kernel," <<- confusion.matrix(datos.prueba, prediccion.knn.",kernel,")","\n"))
}


###############################Generar Codigo Ind.Nuevos ##############################################

kkn.modelo.np <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  kmax <- ifelse(!is.numeric(kmax), round(sqrt(nrow(datos.aprendizaje.completos))), kmax)
  return(paste0("modelo.nuevos <<- traineR::train.knn(",variable.pr,"~., data = datos.aprendizaje.completos, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion 
kkn.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')"))
}
