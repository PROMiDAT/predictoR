# -------------------  GX BOOSTING

cod.xgb.modelo <<-  NULL
cod.xgb.pred   <<-  NULL
cod.xgb.mc     <<- NULL
cod.xgb.ind    <<- NULL


# Pagina de GX BOOSTING ---------------------------------------------------------------------------------------------------

xgb.modelo <- function(variable.pr = NULL, booster = "gbtree",max.depth = 6, n.rounds = 60){
  return(paste0("modelo.xgb.",booster," <<- traineR::train.xgboost(",variable.pr,"~., data = datos.aprendizaje, booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")"))
}

#Codigo de la prediccion de xgb
xgb.prediccion <- function(booster = "gbtree") {
  return(paste0("prediccion.xgb.",booster," <<- predict(modelo.xgb.",booster,", datos.prueba, type = 'class')"))
}

xgb.modelo.np <- function(variable.pr = "", booster = "gbtree", max.depth = 6, n.rounds = 60){
  return(paste0("modelo.nuevos <<- traineR::train.xgboost(",variable.pr,"~., data = datos.aprendizaje.completos, booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")"))
}

xgb.prediccion.np <- function() {
  return(paste0("datos.prueba.aux <<- datos.prueba.completos\n",
                "datos.prueba.aux[['",variable.predecir.np,"']]<- as.factor(levels(datos.aprendizaje.completos[['",variable.predecir.np,"']]))\n",
                "predic.nuevos <<- predict(modelo.nuevos, datos.prueba.aux, type = 'class')"))
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
# variables.importantes <<- xgboost::xgb.importance(feature_names = nombres, model = modelo.xgb.gbtree)
# vars <- variables.importantes[1:length(nombres),]
# vars[,2] <- abs(vars[,2])
# datos.xgb<- data.frame(label = nombres, values = vars[,2])
# datos.xgb %>% e_charts(label) %>% e_bar(Gain, name = var) %>%
#        e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()%>%
#        e_flip_coords()%>%
#   e_y_axis(inverse = TRUE)

