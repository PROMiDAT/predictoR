#Divide un string con "-"
split_name <-function(name, idioma){
  nom.aux <- unlist(strsplit(name, "-"))
  ifelse(length(nom.aux) == 1,
         tr(nom.aux, idioma),
         paste0(tr(nom.aux[1], idioma),"-",nom.aux[2]))
}

#Obtiene la cantidad de categorías de la variable a predecir
num.categorias.pred <- function(){
  return(length(levels(datos.aprendizaje[,variable.predecir])))
}

#Cálcula el área de la curva ROC
areaROC <- function(prediccion,real) {
  area <- pROC::roc(real, prediccion, direction= "<" )
  return(as.numeric(area$auc))
}
