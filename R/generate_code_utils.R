codigo.modelo <- function(model.name = "knn", variable.pr = NULL){
  return(paste0("modelo.",model.name," <<- train.",model.name,"(",variable.pr,"~., data = datos.aprendizaje)\n"))
}

codigo.prediccion <- function(model.name = "knn", alg = NULL){
  if (is.null(alg)) {
    return(paste0("prediccion.",model.name," <<- predict(modelo.",model.name,", datos.prueba, type = 'class')\n"))
  }else{
    return(paste0("prediccion.",model.name,".",alg," <<- predict(modelo.",model.name,".",alg,", datos.prueba, type='class')\n"))
  }
}

#Codigo de la matriz de confucion de Bayes
codigo.MC <- function(model.name = "knn", alg = NULL){
  if (is.null(alg)) {
    return(paste0("MC.",model.name," <<- confusion.matrix(datos.prueba, prediccion.",model.name,")","\n"))
  }else{
    return(paste0("MC.",model.name,".",alg," <<- confusion.matrix(datos.prueba, prediccion.",model.name,".",alg,")","\n"))
  }
}

# Códigos de BOOSTING --------------------------------------------------------------------------------------------------------

#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, iter = 50, maxdepth = 1, minsplit = 1){
  iter     <- ifelse(!is.numeric(iter), 50, iter)
  maxdepth <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo   <- paste0("modelo.boosting <<- train.adabag(",variable.pr,"~., data = datos.aprendizaje, mfinal = ",iter,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))\n")
  return(codigo)
}

#Código del grafico de boosting
boosting.plot <- function(){
  return(paste0("error(modelo.boosting, datos.aprendizaje) -> evol.train\n",
                "e_evol_error(evol.train)\n"))
}

#Reglas de boosting
rules.boosting <- function(i){
  return(paste0("rules(modelo.boosting$trees[[",i,"]])\n"))
}

#Código del grafico de importancia de variables
boosting.plot.import <- function() {
  return(paste0(
    "aux <- data.frame(importancia = modelo.boosting$importance)\n",
    "aux$nombre <- row.names(aux)\n",
    "aux$importancia <- abs(aux$importancia)\n",
    "aux <- aux[order(aux$importancia, decreasing = T), ]\n\n",
    "aux |>  e_charts(nombre) |>  e_bar(importancia, name = var) |>  \n",
    "  e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() |>  \n",
    "  e_flip_coords() |> \n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}


# Códigos de DT --------------------------------------------------------------------------------------------------------

#Crea el modelo DT
dt.modelo  <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15, split = "gini"){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo   <- paste0("modelo.dt.",split," <<- train.rpart(",variable.pr,"~., data = datos.aprendizaje,
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"),parms = list(split = '",split,"'))\n")
  return(codigo)
}

#Código del gráfico de dt
dt.plot <- function(tipo, num = 1){
  #∫num <- length(levels(datos[,var.pred]))
  return(paste0("prp(modelo.dt.",tipo,", type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
                fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
                box.col = gg_color_hue(",num,")[modelo.dt.",tipo,"$frame$yval])\n"))
}

# Códigos de KNN --------------------------------------------------------------------------------------------------------
#' @import traineR 

#Crea el modelo KNN
code.kkn.modelo <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  return(paste0("modelo.knn.",kernel," <<- traineR::train.knn(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')\n"))
}

# Códigos de  RL --------------------------------------------------------------------------------------------------------------

#Crea el modelo RL
rl.modelo <- function(variable.predecir = NULL){
  return(paste0("modelo.glm <<- train.glm(",variable.predecir,"~., data = datos.aprendizaje, family = binomial)\n"))
}

# Códigos de NN ---------------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo   <- function(variable.pr = NULL, threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax   <- ifelse(stepmax < 100, 100, stepmax)
  capas     <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  
  return(paste0("modelo.neuralnet <<- train.neuralnet(",variable.pr,"~., data = datos.aprendizaje, hidden = ",capas,",\n\t\t\tlinear.output = FALSE,",
                "threshold = ",threshold,", stepmax = ",stepmax,")\n"))
}

#Gráfico de la red neuronal
nn.plot <- function(){
  paste0("plot(modelo.neuralnet,,arrow.length = 0.1, rep = 'best', intercept = T,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=F,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)\n")
}

# Códigos de RLR -------------------------------------------------------------------------------------------------------------

#Crea el modelo RLR
rlr.modelo <- function(variable.pr = NULL, type = "ridge", alpha = 0, escalar = TRUE){
  return(paste0("modelo.glmnet.",type,"<<- train.glmnet(",variable.pr,"~., data = datos.aprendizaje, standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')\n"))
}

# Códigos de RF--------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree   <- ifelse(!is.numeric(ntree), 500, ntree)
  Codigo  <- paste0("modelo.rf <<- train.randomForest(",variable.pr,"~., data = datos.aprendizaje,importance = TRUE,",
                    " ntree =",ntree,",mtry =",mtry,")\n")
  return(Codigo)
}

#Código del gráfico de importancia de variables
rf.importance.plot <- function() {
  return(paste0(
    "aux <- data.frame(modelo.rf$importance)\n",
    "aux$MeanDecreaseAccuracy <- abs(aux$MeanDecreaseAccuracy)\n",
    "aux <- aux[order(aux$MeanDecreaseAccuracy, decreasing = T), ]\n",
    "aux$label <- row.names(aux)\n\n",
    "aux |>  e_charts(label) |>  e_bar(MeanDecreaseAccuracy, name = var) |>  \n",
    "  e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() |>  \n",
    "  e_flip_coords() |> \n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}

#Código del gráfico de error del modelo
plot.rf.error <- function(){
  return(paste0("e_rf_error(modelo.rf)\n"))
}

# Códigos de SVM -------------------------------------------------------------------------------------------------------------

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.svm.",kernel," <- traineR::train.svm(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kernel = '",kernel,"')\n"))
}

#Código del gráfico de svm
svm.plot <- function(var.pred,train,  variables, resto, kernel = "linear"){
  if(is.null(variables)){
    return("NULL")
  }
  
  l <- c()
  for(i in 1:length(resto)){
    l <- c(l , paste0(resto[i]," = ", i))
  }
  l <- paste0("list(",paste0(l,collapse = ","),")")
  s <- paste0("modelo.svm.temp <<- traineR::train.svm(",var.pred,"~",variables[1],"+",variables[2],", data = datos.aprendizaje, kernel = '",kernel,"') \n")
  color <- length(unique(train[,var.pred]))
  color <- as.string.c(gg_color_hue(color))
  return(paste0(s,"plot(modelo.svm.temp, datos, ",variables[1],"~",variables[2],", slice = ",l,", col = ",color,")\n"))
}
# Códigos de GX BOOSTING ---------------------------------------------------------------------------------------------------

#Crea el modelo
xgb.modelo <- function(variable.pr = NULL, booster = "gbtree",max.depth = 6, n.rounds = 60){
  return(paste0("modelo.xgb.",booster," <<- traineR::train.xgboost(",variable.pr,"~., data = datos.aprendizaje, booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")\n"))
}


#Código del grafico de importancia de variables
e_xgb_varImp <- function(booster = "gbtree"){
  paste0("nombres                   <- modelo.xgb.",booster,"$feature_names\n",   
         "variables.importantes     <- xgboost::xgb.importance(feature_names = nombres, model = modelo.xgb.",booster,")\n",
         "variables.importantes     <- variables.importantes[1:length(nombres),]","\n",
         "variables.importantes[,2] <- abs(variables.importantes[,2])","\n",
         "variables.importantes     <- na.omit(variables.importantes)\n",
         "datos.xgb <- data.frame(label = variables.importantes$Feature, values = variables.importantes[,2])\n",
         "datos.xgb |>  e_charts(label) |>  e_bar(values, name = var) |> \n",
         "e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|> \n",
         "e_flip_coords()|> \n",
         "e_y_axis(inverse = TRUE)\n"
         
  )
}

