# -------------------  NN
NN_EXECUTION  <<-  TRUE

# Códigos de NN ---------------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo   <- function(variable.pr = NULL, threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax   <- ifelse(stepmax < 100, 100, stepmax)
  capas     <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  
  return(paste0("modelo.nn <<- train.neuralnet(",variable.pr,"~., data = datos.aprendizaje, hidden = ",capas,",\n\t\t\tlinear.output = FALSE,",
                "threshold = ",threshold,", stepmax = ",stepmax,")\n"))
}

#Código de la prediccion de nn
nn.prediccion <- function() {
  return(paste0("prediccion.nn <<- predict(modelo.nn, datos.prueba, type = 'class')"))
}

#Código de la matriz de confucion de xgb
nn.MC <- function(){
  return(paste0("MC.nn <<- confusion.matrix(datos.prueba, prediccion.nn)","\n"))
}

#Gráfico de la red neuronal
nn.plot <- function(){
  paste0("plot(modelo.nn,,arrow.length = 0.1, rep = 'best', intercept = T,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=F,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)")
}

# Códigos de NN Ind.Nuevos--------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo.np <- function(variable.pr = "",threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  capas     <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  stepmax   <- ifelse(1000>stepmax, 1000, stepmax)
  threshold <- ifelse(0.01>threshold, 0.01, threshold)
  
  return(paste0("modelo.nuevos <<- train.neuralnet(",variable.pr,"~., data = datos.aprendizaje.completos, hidden = ",capas,",\n\t\t\tlinear.output = FALSE,",
                "threshold = ",threshold,", stepmax = ",stepmax,")\n"))
}

#Código de la prediccion de nn
nn.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')\n"))
}
