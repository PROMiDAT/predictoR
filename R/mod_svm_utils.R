# -------------------  SVM

cod.svm.modelo <<-  NULL
cod.svm.pred   <<-  NULL
cod.svm.mc     <<-  NULL
cod.svm.ind    <<-  NULL
# Pagina de SVM -------------------------------------------------------------------------------------------------------------

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.svm.",kernel," <<- traineR::train.svm(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kernel = '",kernel,"')"))
}

svm.modelo.np <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.nuevos <<- traineR::train.svm(",variable.pr,"~., data = datos.aprendizaje.completos, scale =",scale,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion de svm
svm.prediccion <- function(kernel = "linear") {
  return(paste0("prediccion.svm.",kernel," <<- predict(modelo.svm.",kernel," , datos.prueba, type = 'class')"))
}

svm.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos , datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.np,"')], type = 'class')"))
}

#Codigo de la matriz de confucion de svm
svm.MC <- function( kernel = "linear"){
  return(paste0("MC.svm.",kernel," <<- confusion.matrix(datos.prueba, prediccion.svm.",kernel,")","\n"))
}

#Codigo del grafico de svm
svm.plot <- function(variables, resto, kernel = "linear"){
  if(is.null(variables)){
    return("NULL")
  }

  l <- c()
  for(i in 1:length(resto)){
    l <- c(l , paste0(resto[i]," = ", i))
  }
  l <- paste0("list(",paste0(l,collapse = ","),")")
  s <- paste0("modelo.svm.temp <<- traineR::train.svm(",variable.predecir,"~",variables[1],"+",variables[2],", data = datos.aprendizaje, kernel = '",kernel,"') \n")
  #exe(s)
  color <- length(unique(datos.aprendizaje[,variable.predecir]))
  color <- as.string.c(gg_color_hue(color))
  return(paste0(s,"plot(modelo.svm.temp, datos, ",variables[1],"~",variables[2],", slice = ",l,", col = ",color,")"))
}

