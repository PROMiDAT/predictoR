# -------------------  GX BOOSTING

cod.xgb.modelo <<-  NULL
cod.xgb.pred <<-  NULL
cod.xgb.mc <<- NULL
cod.xgb.ind <<- NULL


# Pagina de GX BOOSTING ---------------------------------------------------------------------------------------------------

xgb.modelo <- function(variable.pr = NULL, booster = "gbtree",max.depth = 6, n.rounds = 60){
  return(paste0("modelo.xgb.",booster," <<- traineR::train.xgboost(",variable.pr,"~., data = datos.aprendizaje, booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")"))
}

xgb.modelo.np <- function(variable.pr = "", booster = "gbtree", max.depth = 6, n.rounds = 60){
  return(paste0("modelo.nuevos <<- traineR::train.xgboost(",variable.pr,"~., data = datos.aprendizaje.completos, booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")"))
}

#Codigo de la prediccion de xgb
xgb.prediccion <- function(booster = "gbtree") {
  return(paste0("prediccion.xgb.",booster," <<- predict(modelo.xgb.",booster,", datos.prueba, type = 'class')"))
}

xgb.prediccion.np2 <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')"))
}

xgb.prediccion.np <- function() {
  clases <- levels(datos.aprendizaje.completos[,variable.predecir.np])
  num.class <- length(clases)
  if(num.class > 2){
    pred <- paste0("predic.nuevos <<- max.col(matrix(predic.nuevos, ncol=",num.class,", byrow=TRUE))\n")
  }else{
    pred <- paste0("predic.nuevos <<- ifelse(predic.nuevos > 0.5, 2, 1)\n")
  }
  x <- paste0("'",1:num.class,"'='%s'",collapse = ",")
  recod <- do.call(sprintf, c(list(x), clases))
  
  categoricas <- colnames(var.categoricas(datos.prueba.completos))
  categoricas <- categoricas[variable.predecir.np != categoricas]
  if(length(categoricas) > 0){
    categoricas <- paste0(categoricas,collapse = "','")
    categoricas <- paste0("d.prueba[,c('",categoricas,"')] <- d.prueba[,c('",categoricas,"')] - 1\n")
  }else{
    ategoricas <- ""
  }
  pre <- paste0("d.prueba <- mutate_all(datos.prueba.completos, funs(as.numeric))\n",
                categoricas,
                "selector <- -which(colnames(d.prueba) == '",variable.predecir.np,"')\n",
                "mxgb.prueba <- xgboost::xgb.DMatrix(data = data.matrix(d.prueba[,selector]),",
                "label = data.matrix(d.prueba$'",variable.predecir.np,"'))\n",
                "predic.nuevos <<- predict(modelo.nuevos, mxgb.prueba)\n",
                pred,
                "predic.nuevos <<- recode(predic.nuevos, ",recod,")")
  return(pre)
}
#Codigo de la matriz de confucion de xgb
xgb.MC <- function(booster = "gbtree"){
  return(paste0("MC.xgb.",booster," <<- confusion.matrix(datos.prueba, prediccion.xgb.",booster,")","\n"))
}

#Codigo del grafico de importancia de variables
xgb.varImp <- function(booster = "gbtree"){
  paste0("nombres <<- modelo.xgb.",booster,"$feature_names\n",   
         "variables.importantes <<- xgboost::xgb.importance(feature_names = nombres, model = modelo.xgb.",booster,")\n",
         "variables.importantes <- variables.importantes[1:length(nombres),]","\n",
         "variables.importantes[,2] <- abs(variables.importantes[,2])","\n",
         "xgboost::xgb.plot.importance(importance_matrix = variables.importantes, col = ",as.string.c(gg_color_hue(ncol(datos.aprendizaje) - 1)),")"
  )
}

# V1 
# nombres <- modelo.xgb.gbtree$feature_names
# vars <- variables.importantes[1:length(nombres),]
# vars[,2] <- abs(vars[,2])
# color <- as.string.c(gg_color_hue(length(nombres)))
# datos.xgb<- data.frame(label = nombres, values = vars[,2])
# datos.xgb %>% e_charts(label) %>% e_bar(Gain, name = var) %>%
#        e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()%>%
#        e_flip_coords()%>%
#   e_y_axis(inverse = TRUE)

# V2 COLOR
# nombres <- modelo.xgb.gbtree$feature_names
# vars <- variables.importantes[1:length(nombres),]
# vars[,2] <- abs(vars[,2])
# color <- gg_color_hue(length(nombres))
# datos.xgb<- data.frame(label = nombres, values = vars[,2], color = gg_color_hue(length(nombres)))
# datos.xgb %>% e_charts(label) %>% e_bar(Gain, name = var) %>%
#   e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()%>%
#   e_flip_coords()%>%
#   e_y_axis(inverse = TRUE)%>%
#   e_add("itemStyle", color)
