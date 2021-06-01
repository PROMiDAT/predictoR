# Códigos de KNN --------------------------------------------------------------------------------------------------------

#Crea el modelo KNN
code.kkn.modelo <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  library(traineR)
  kmax <- ifelse(!is.numeric(kmax), round(sqrt(nrow(datos.aprendizaje))), kmax)
  return(paste0("modelo.knn.",kernel," <<- traineR::train.knn(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

#Código de la prediccion de knn
kkn.prediccion <- function(variable.pr,kernel = "optimal") {
  return(paste0("prediccion.knn.",kernel," <<- predict(modelo.knn.",kernel,", datos.prueba, type = 'class')"))
}

#Código de la matriz de confucion de knn
knn.MC <- function(kernel = "optimal"){
  return(paste0("MC.knn.",kernel," <<- confusion.matrix(datos.prueba, prediccion.knn.",kernel,")","\n"))
}

