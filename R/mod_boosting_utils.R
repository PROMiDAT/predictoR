# -------------------      BOOSTING

cod.b.modelo <<-  NULL
cod.b.pred <<-  NULL
cod.b.mc <<- NULL
cod.b.ind <<- NULL

# Pagina de BOOSTING --------------------------------------------------------------------------------------------------------


#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, iter = 50, maxdepth = 1, minsplit = 1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  nu <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo <- paste0("modelo.boosting <<- train.adabag(",variable.pr,"~., data = datos.aprendizaje, mfinal = ",iter,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

boosting.modelo.np <- function(variable.pr = NULL, iter = 50, maxdepth = 1,  minsplit = 1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  nu <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo <- paste0("modelo.nuevos <<- train.adabag(",variable.pr,"~., data = datos.aprendizaje.completos, mfinal = ",iter,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

#Codigo de la prediccion de boosting
boosting.prediccion <- function() {
  return(paste0("prediccion.boosting <<- predict(modelo.boosting, datos.prueba, type = 'class')"))
}

boosting.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')"))
}

#Codigo de la matriz de confucion de boosting
boosting.MC <- function(){
  return(paste0("MC.boosting <<- confusion.matrix(datos.prueba, prediccion.boosting)\n"))
}

#Codigo del grafico de boosting
boosting.plot <- function(){
  return(paste0("error(modelo.boosting, datos.aprendizaje) -> evol.train\n",
                 "e_evol_error(evol.train)"))
}

#Codigo del grafico de importancia de variables
boosting.plot.import <- function() {
  return(paste0(
    "aux <- data.frame(importancia = modelo.boosting$importance)\n",
    "aux$nombre <- row.names(aux)\n\n",
    "aux <- aux[order(aux$importancia, decreasing = T), ]\n",
    "aux %>% e_charts(nombre) %>% e_bar(importancia, name = var) %>% \n",
    "  e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading() %>% \n",
    "  e_flip_coords() %>%\n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}

#Codigo del grafico de evolucion del error
e_evol_error <- function(x) {
  if (!((class(x) == "errorevol"))) 
    stop("x class should be errorevol")
  
  evolplot <- data.frame(x = c(1:length(x$error)), train = x$error)
  evolplot %>%
    e_charts(x) %>%
    e_line(train) %>%
    e_title("Ensemble error vs number or trees",
            left = 'center',
            top = 5,
            textStyle = list(fontSize = 15))%>%
    e_legend(orient = 'vertical',
             right = '20', top = '10%') %>%
    e_axis_labels(
      x = "Iterations",
      y = "Error"
    )%>%  e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
}

rules.boosting <- function(i){
  return(paste0("rules(modelo.boosting$trees[[",i,"]])"))
}

rules <- function (model, compact = FALSE, ...){
  if (!inherits(model, "rpart"))
    stop(rattle:::Rtxt("Not a legitimate rpart tree"))
  rtree <- length(attr(model, "ylevels")) == 0
  target <- as.character(attr(model$terms, "variables")[2])
  frm <- model$frame
  names <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1, ]$n
  if (rtree){
    ordered <- rev(sort(frm$n, index = TRUE)$ix)
  } else {
    ordered <- rev(sort(frm$yval2[, 5], index = TRUE)$ix)
  }
  for (i in ordered) {
    if (frm[i, 1] == "<leaf>") {
      if (rtree)
        yval <- frm[i, ]$yval
      else {
        yval <- ylevels[frm[i, ]$yval]
        yval <- ifelse(yval == -1, 1, 2)
        yval <- levels(datos.aprendizaje[,variable.predecir])[yval]
      }
      cover <- frm[i, ]$n
      pcover <- round(100 * cover/ds.size)
      if (!rtree)
        prob <- frm[i, ]$yval2[, 5]
      cat("\n")
      pth <- rpart::path.rpart(model, nodes = as.numeric(names[i]),
                               print.it = FALSE)
      pth <- unlist(pth)[-1]
      if (!length(pth))
        pth <- "True"
      if (compact) {
        cat(sprintf("R%03s ", names[i]))
        if (rtree)
          cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
        else cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
        cat(sprintf(" %s", pth), sep = "")
      }
      else {
        cat(sprintf(rattle:::Rtxt("Rule number: %s "), names[i]))
        if (rtree){
          cat(sprintf("[%s=%s cover=%d (%.0f%%)]\n", target, yval, cover, pcover))
        }else{
          cat(sprintf("[%s=%s cover=%d (%.0f%%) prob=%0.2f]\n", target, yval, cover, pcover, prob))
        }
        cat(sprintf("  %s\n", pth), sep = "")
      }
    }
  }
  cat("\n")
  invisible(ordered)
}
