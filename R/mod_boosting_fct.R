#Funciones tomadas del paquete ADABAG 

error <- function (object, newdata) 
{
  if (!("boosting" %in% class(object))) {
    stop("object must of class 'boosting' ")
  }
  vardep <- newdata[, as.character(object$formula[[2]])]
  mfinal <- length(object$trees)
  n <- length(newdata[, 1])
  nclases <- nlevels(vardep)
  
  ponderacion <- object$weights
  
  erroracum <- rep(0, mfinal)
  pred <- as.data.frame(sapply(object$trees, predict, newdata = newdata, 
                               type = "class"))
  mvotos <- list()
  classfinal <- array(0, c(n, nlevels(vardep)))
  for (i in 1:nlevels(vardep)) {
    mvotos[[i]] <- matrix(as.numeric(pred == levels(vardep)[i]), 
                          nrow = n) %*% diag(ponderacion)
  }
  for (j in 1:mfinal) {
    if (j == 1) {
      for (i in 1:nlevels(vardep)) {
        classfinal[, i] <- mvotos[[i]][, 1]
      }
    }
    else {
      for (i in 1:nlevels(vardep)) {
        classfinal[, i] <- apply(cbind(classfinal[, i], 
                                       mvotos[[i]][, j]), 1, sum)
      }
    }
    predclass <- rep("O", n)
    predclass[] <- apply(classfinal, 1, FUN = selectada, vardep.summary = summary(vardep))
    error <- 1 - sum(predclass == vardep)/n
    erroracum[j] <- error
  }
  output <- list(error = erroracum)
  class(output) <- "errorevol"
  output
}

selectada <- function (fila, vardep.summary, ...) 
{
  if (length(which(fila == max(fila))) > 1) {
    predclass <- names(vardep.summary[which(fila == max(fila))])[order(vardep.summary[which(fila == 
                                                                                              max(fila))], decreasing = TRUE)[1]]
  }
  else {
    predclass <- as.character(names(vardep.summary)[(order(fila, 
                                                           decreasing = TRUE)[1])])
  }
  predclass
}

plot.errorevol.ada <- function (x, y = NULL, ...) 
{
  if (!((class(x) == "errorevol"))) 
    stop("x class should be errorevol")
  if (!((class(y) == "errorevol") | is.null(y))) 
    stop("y class should be errorevol or NULL")
  plot(x$error, type = "l", ylim = c(0, max(x$error) + 0.05), 
       main = "Ensemble error vs number of trees", xlab = "Iterations", 
       ylab = "Error", col = "red", ...)
  if (!is.null(y)) {
    lines(y$error, cex = 0.5, col = "blue3", lty = 2)
    legend("topright", c("test", "train"), col = c("red", 
                                                   "blue"), lty = 1:2)
  }  
  if (!is.null(x)) {
    legend("topright", c("train"), col = c("red"), lty = 1)
  }
}


