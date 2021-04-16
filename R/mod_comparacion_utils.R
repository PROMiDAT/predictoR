split_name <-function(name){
  nom.aux <- unlist(strsplit(name, "-"))
  ifelse(length(nom.aux) == 1,
         tr(nom.aux),
         paste0(tr(nom.aux[1]),"-",nom.aux[2]))
}

get_names_models <- function(){
  if(length(IndicesM) == 0) {
    return("---X---")
  }
  nombres <- c()
  for (nom in names(IndicesM)){
    nombres <- c(nombres,split_name(nom))
  }
  return(nombres)
}

#Actualiza los selectores de la tabla comparativa
actualizar.selector.comparativa <- function(){
  return(get_names_models())
}

num.categorias.pred <- function(){
  return(length(levels(datos.aprendizaje[,variable.predecir])))
}

# Crea la tabla comparativa
tabla.comparativa <- function(sel, nombres, idioma) {
  tryCatch({
    if(nombres[1] == "---X---") {
      return(data.frame())
    }
    resp <- lapply(IndicesM, function(x)t(as.data.frame(unlist(x))))
    resp <- as.data.frame(do.call(rbind, resp))
    selector <- (ncol(resp)-num.categorias.pred() + 1):ncol(resp)
    resp <- resp[,-selector,drop = FALSE]
    resp <- cbind(resp, replace(unlist(areas[nombres]),is.null(unlist(areas[nombres])),NA))
    rownames(resp) <- nombres
    colnames(resp) <- c(tr('precG', idioma),tr("errG", idioma), levels(datos.aprendizaje[,variable.predecir]), tr('aROC', idioma))
    resp[] <- lapply(resp, as.numeric)
    resp <- round(resp, 4)
    resp <- resp[nombres %in% sel,]
    return(resp)
    
  }, error = function(e){
    return(data.frame())
  })
}

# Calcula las areas de la curva roc de todos los modelos
calcular.areas <- function(sel) {
  clase <- datos.prueba[, variable.predecir]
  if(length(unique(clase)) == 2){
    for(nombre in names(scores)){
      nom.aux <- split_name(nombre)
      if (is.factor(scores[[nombre]])) {
        areas[[nom.aux]] <<- areaROC(attributes(scores[[nombre]])$probabilities[, sel], clase)
      }else{
        if(is.vector((scores[[nombre]]))){
          areas[[nom.aux]] <<- areaROC(scores[[nombre]], clase)

        }else{
          areas[[nom.aux]] <<- areaROC(as.data.frame(scores[[nombre]])[, which(levels(clase) == sel)], clase)
        }
      }
    }
  }
}

#Calcula el area de la curva ROC
areaROC <- function(prediccion,real) {
  pred <- ROCR::prediction(prediccion,real)
  auc <- ROCR::performance(pred,"auc")
  return(attributes(auc)$y.values[[1]])
}

#Hace el grafico de una curba de roc
plotROCInd <- function(prediccion,real,adicionar=FALSE,color="red") {
  pred <- ROCR::prediction(prediccion,real)
  perf <- ROCR::performance(pred,"tpr","fpr")
  plot(perf, col=color, add=adicionar, main="Curva ROC")
  segments(0,0,1,1, col='black')
  grid()
}

#Hace el grafico de la curba de roc de los modelos
plotROC <- function(sel) {

  clase <- datos.prueba[,variable.predecir]
  col <- gg_color_hue(length(scores))
  nombres <- c()
  colores <- c()
  adicionar <- FALSE
  index <- 1
  
  nombres.tr <- unlist(lapply(names(scores), split_name))
  SCORES <<- scores[nombres.tr %in% sel]
  nombres.tr <- nombres.tr[nombres.tr %in% sel]
  
  if(length(SCORES) == 0) {
    return(NULL)
  }
  
  for (nombre in names(SCORES)) {
    if(is.numeric(SCORES[[nombre]])){
      plotROCInd(SCORES[[nombre]],clase, adicionar, col[index])
    }else{
      if(is.factor(SCORES[[nombre]])){
        plotROCInd(SCORES[[nombre]],clase, adicionar, col[index])
        }
    }
    adicionar <- TRUE
    colores <- c(colores, col[index])
    nombres <- c(nombres, nombres.tr[index])
    index <- index + 1
  }
  legend(x=0.85, y=0.8, legend = nombres, bty = "n", pch=19 ,
         col = colores , text.col = "black", cex=0.7, pt.cex=0.7)
}

