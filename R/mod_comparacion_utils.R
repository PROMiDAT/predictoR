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
    resp      <- lapply(IndicesM, function(x)t(as.data.frame(unlist(x))))
    resp      <- as.data.frame(do.call(rbind, resp))
    selector  <- (ncol(resp)-num.categorias.pred() + 1):ncol(resp)
    resp      <- resp[,-selector,drop = FALSE]
    resp      <- cbind(resp, replace(unlist(areas[nombres]),is.null(unlist(areas[nombres])),NA))
    rownames(resp) <- nombres
    colnames(resp) <- c(tr('precG', idioma),tr("errG", idioma), levels(datos.aprendizaje[,variable.predecir]), tr('aROC', idioma))
    resp[]    <- lapply(resp, as.numeric)
    resp      <- round(resp, 4)
    resp      <- resp[nombres %in% sel,]
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
  pred  <- ROCR::prediction(prediccion,real)
  auc   <- ROCR::performance(pred,"auc")
  return(attributes(auc)$y.values[[1]])
}

#Hace el grafico de una curba de roc
plotROCInd <- function(prediccion,real,adicionar=FALSE,color="red") {
  pred <- ROCR::prediction(prediccion,real)
  perf <<- ROCR::performance(pred,"tpr","fpr")
  plot(perf, col=color, add=adicionar, main="Curva ROC")
  segments(0,0,1,1, col='black')
  grid()
}

performanceROC <- function(prediccion,real) {
  pred <- ROCR::prediction(prediccion,real)
  perf <<- ROCR::performance(pred,"tpr","fpr")
  return(perf)
}

#Hace el grafico de la curba de roc de los modelos
plotROC <- function(sel) {
  
  clase   <- datos.prueba[,variable.predecir]
  col     <- gg_color_hue(length(scores))
  nombres <- c()
  colores <- c()
  adicionar <- FALSE
  index   <- 1
  
  nombres.tr <<- unlist(lapply(names(scores), split_name))
  SCORES     <<- scores[nombres.tr %in% sel]
  nombres.tr <<- nombres.tr[nombres.tr %in% sel]
  
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
    colores   <- c(colores, col[index])
    nombres   <- c(nombres, nombres.tr[index])
    index     <- index + 1
  }
  legend(x=0.85, y=0.8, legend = nombres, bty = "n", pch=19 ,
         col = colores , text.col = "black", cex=0.7, pt.cex=0.7)
}

e_plot_ROC <- function(sel) {
  index   <- 2
  
  clase   <- datos.prueba[,variable.predecir]
  nombres.tr <<- unlist(lapply(names(scores), split_name))
  SCORES     <<- scores[nombres.tr %in% sel]
  nombres.tr <<- nombres.tr[nombres.tr %in% sel]
   
  if(length(SCORES) == 0) {
    return(NULL)
  }
  pred <-  ROCR::prediction(SCORES[[1]],clase)
  perf <-  ROCR::performance(pred,"tpr","fpr")

  col_names <- c('x',nombres.tr)
  colList <- list(attributes(perf)$y.values[[1]])
  df <- do.call(data.frame,colList)
  df <- cbind(x =  attributes(perf)$x.values[[1]], df)
  #df <- binding(df,as.data.frame(attributes(perf3)$y.values[[1]]) )
  for (nombre in names(SCORES)[2:length(SCORES)]) {
    if(is.numeric(SCORES[[nombre]])){
      pred <-  ROCR::prediction(SCORES[[nombre]],clase)
      perf <- ROCR::performance(pred,"tpr","fpr")
      df <- binding(df,as.data.frame(attributes(perf)$y.values[[1]]) )
      #attributes(p)$y.values[[1]]
      #attributes(p)$x.values[[1]]
    }else{
      if(is.factor(SCORES[[nombre]])){
      p <- performanceROC(SCORES[[nombre]],clase)
      }
    }
    index     <- index + 1
    
  }
  colnames(df) <- col_names
}


#df %>% e_charts(x) %>% e_line(`Bosques Aleatorios`) %>% e_tooltip()
# pred <-  ROCR::prediction(SCORES[[3]],clase)
# perf <- ROCR::performance(pred,"tpr","fpr")
# x2 <- attributes(perf)$x.values[[1]]
# df <- binding(df, as.data.frame(x2) )
# df %>%  e_charts(x2) %>% e_line(`Bosques Aleatorios`) %>% e_tooltip()
# df %>% e_charts(x1) %>% e_line(`Potenciación`) %>% e_line(`Bosques Aleatorios`) %>% e_line(`Árboles De Decisión-gini`) %>% e_tooltip()%>% e_datazoom(show = F)
# 

binding <- function (...) 
{
  x <- list(...)
  test <- sapply(x, function(z) is.matrix(z) | is.data.frame(z))
  if (any(!test)) 
    stop("only matrices and data.frames can be used")
  tmp <- sapply(x, nrow)
  maxi <- which.max(tmp)
  test <- tmp < tmp[maxi]
  for (i in 1:length(tmp)) {
    if (test[i]) {
      add <- matrix(nrow = tmp[maxi] - tmp[i], ncol = ncol(x[[i]]))
      if (is.data.frame(x[[i]])) {
        add <- as.data.frame(add)
      }
      colnames(add) <- colnames(x[[i]])
      x[[i]] <- rbind(x[[i]], add)
    }
  }
  ret <- x[[1]]
  for (i in 2:length(tmp)) {
    ret <- cbind(ret, x[[i]])
  }
  ret
}
