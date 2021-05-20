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
  area <- pROC::roc(real, prediccion)
  return(as.numeric(area$auc))
}

e_plot_ROC <- function(sel) {

  clase      <- datos.prueba[,variable.predecir]
  nombres.tr <- unlist(lapply(names(scores), split_name))
  SCORES     <- scores[nombres.tr %in% sel]
  nombres.tr <- nombres.tr[nombres.tr %in% sel]
   
  if(length(SCORES) == 0) {
    return(NULL)
  }
  y = c(0, 1)
  x = c(1, 0)
  df <- data.frame(x, y, nombre = "roc")
  n  <- 1
  for (nombre in names(SCORES)[1:length(SCORES)]) {
    if(is.numeric(SCORES[[nombre]])){
      roc.data <-  pROC::roc(clase, SCORES[[nombre]])
      df <- add.df.roc(roc.data, df, nombre = nombres.tr[[n]])
      n  <- n + 1
    }else{
      if(is.factor(SCORES[[nombre]])){
        roc.data <-  pROC::roc(clase, SCORES[[nombre]])
        df <- add.df.roc(roc.data, df, nombre = nombres.tr[[n]])
        n  <- n + 1
      }
    }
  }

  df$nombre <- as.factor(df$nombre)
  colores <- gg_color_hue(length(unique(df$nombre)))
  plotroc  <- df %>%
    group_by(nombre) %>%
    e_charts(x) %>%
    e_line(y) %>%
    e_legend(type = "scroll", bottom = 1) %>% 
    e_color(c(colores))    %>% 
    e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
  plotroc$x$opts$legend$data[[which(plotroc$x$opts$legend$data == "roc")]] <- NULL
  #plotroc$x$opts$color[[which(plotroc$x$opts$legend$data == "roc")]] <- "black"
  plotroc$x$opts$xAxis[[1]]$inverse <- TRUE
  plotroc
}

#Añade datos al df para gráfica de la curva ROC
add.df.roc <- function(roc.data, df, nombre){
  y  <- roc.data$sensitivities
  x  <- roc.data$specificities
  df <- rbind(df,data.frame(x= x, y = y, nombre = nombre))
  return(df)
}


# 
# var <- data.frame(x,y,modelo = "arboles")
# var <- rbind(var,data.frame(x= x2, y = y2,modelo = "Bosques"))
# var %>% 
#   group_by(modelo) %>% 
#   e_charts(x) %>% 
#   e_line(y)
# ar <- roc(clase, scores[[1]])
# y <- ar$sensitivities
# x <- ar$specificities
# area  <- ar$auc
# area <- as.numeric(d)
