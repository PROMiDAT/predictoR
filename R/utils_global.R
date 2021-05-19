# VARIABLES GLOBALES --------------------------------------------------------------------------------------------------------

# -------------------  Datos
datos             <<- NULL
datos.originales  <<- NULL
datos.prueba      <<- NULL
datos.aprendizaje <<- NULL
variable.predecir <<- NULL

# -------------------  Modelos

nombres.modelos   <<- c()
IndicesM <<- list()
areas    <<- list()
scores   <<- list()


updatePlot <- reactiveValues(roc = FALSE, svm.graf = NULL)

# Validacion comun para todos los modelos
validar.datos <- function(print = TRUE,variable.predecir,datos.aprendizaje) {
  # Validaciones
  if (is.null(variable.predecir) & print) {
    showNotification(tr("tieneVP"), duration = 10, type = "error")
  }
  if (is.null(datos.aprendizaje) & print) {
    showNotification(tr("tieneDAP"), duration = 10, type = "error")
  }
  return(!is.null(variable.predecir) & !is.null(datos.aprendizaje))
}

# Crea la tabla de comparacion entre prediccion y datos reales (datos de prueba)
obj.predic <- function(predic.var = NULL, idioma){
  real <- datos.prueba[, variable.predecir]
  if(is.numeric(predic.var$prediction)) {
    predic.var <- factor(predic.var, labels = levels(real))
  }
  real   <- as.character(real)
  predi  <- as.character(predic.var$prediction)
  acerto <- paste0("<span style='color:green'><b>",tr("acerto",idioma),"</b></span>")
  fallo  <- paste0("<span style='color:red'><b>",tr("fallo",idioma),"</b></span>")
  df     <- cbind(real, predi, ifelse(real == predi,
                                  rep(acerto, length(real)),
                                  rep(fallo, length(real)) ))
  colns  <- c(tr("reald", idioma), tr("pred", idioma), " ")
  colnames(df) <- colns
  sketch <- htmltools::withTags(table(tableHeader(colns)))
  return(DT::datatable(df,
                       selection = "none",
                       editable = FALSE,
                       escape = FALSE,
                       container = sketch,
                       options = list(dom = "frtip", pageLength = 10)))
}

# Cierra un menu segun su tabName
close.menu <- function(tabname = NA, valor = T) {
  select <- paste0("a[href^='#shiny-tab-", tabname, "']")
  if(valor){
    shinyjs::hide(selector = "ul.menu-open")
    shinyjs::disable(selector = select)
  } else {
    shinyjs::enable(selector = select)
  }
}

#tr("denspodlab",idioma)
# Hace el grafico de la matriz de confusion
plot.MC.code <- function(cm,idioma) {
  return(paste0("
plot.MC <<- function(cm,idioma) {
  par(mar = c(2, 2, 2, 2))
  plot(c(1, 600), c(-100, 500), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  title('",tr("mc",idioma),"', cex.main = 2)

  start <- 80
  len <- 500 - start

  n.class <- ncol(cm)
  names.class <- colnames(cm)
  prec.cat <- diag(cm) / rowSums(cm)
  error.cat <- 1 - prec.cat

  ancho <- len / n.class
  alto <- len / (n.class)
  x2 <- (x1 <- start) + ancho
  y2 <- (y1 <- len) - alto

  text(310, 485, '",tr("pred",idioma),"', cex = 1.3, font = 2)
  text(start - 55, 250, 'Real', cex = 1.3, srt = 90, font = 2)

  for (i in 0:(n.class - 1)) {
    for (j in 0:(n.class - 1)) {
      x1.aux <- x1 + j * (ancho + 3)
      y1.aux <- y1 - i * (alto + 5)
      x2.aux <- x2 + j * (ancho + 3)
      y2.aux <- y2 - i * (alto + 5)
      if (j < (n.class)) {
        rect(x1.aux, y1.aux, x2.aux, y2.aux, col = ifelse(i == j, '#3f72af', '#11999e'))
        text(mean(c(x1.aux, x2.aux)),
          mean(c(y1.aux, y2.aux)),
          paste0(cm[(i + 1), (j + 1)], ' (', round(cm[(i + 1), (j + 1)] / sum(cm[(i + 1), ]), 2) * 100, '%)'),
          cex = 1.1, font = 2, col = 'white')
      }
    }
    text(mean(c((x2 + i * (ancho + 3)), (x1 + i * (ancho + 3)))), y1 + 20, names.class[i + 1], cex = 1)
    text(x1 - 20, mean(c((y1 - i * (alto + 5)), (y2 - i * (alto + 5)))), names.class[i + 1], cex = 1)
  }
  text(mean(c((x2 + (i + 1) * (ancho + 3)), (x1 + (i + 1) * (ancho + 3)))), y1 + 20, names.class[i + 2], cex = 1.2)
  text(mean(c((x2 + (i + 2) * (ancho + 3)), (x1 + (i + 2) * (ancho + 3)))), y1 + 20, names.class[i + 3], cex = 1.2)
  text(mean(c((x2 + (i + 3) * (ancho + 3)), (x1 + (i + 3) * (ancho + 3)))), y1 + 20, names.class[i + 4], cex = 1.2)
}"))
}

#Codigo del calculo de los indices
indices.generales <- function(MC) {
  if(1 == dim(MC)[2]) {
    MC <- cbind(MC, 0)
  }
  precision.global <- (sum(diag(MC)) / sum(MC)) * 100
  error.global <- (1 - (sum(diag(MC)) / sum(MC))) * 100
  precision.clase <- diag(MC)/rowSums(MC) * 100
  error.clase <- 100 - precision.clase
  return(list(precision.global = precision.global,
              error.global = error.global,
              precision.clase = precision.clase,
              error.clase = error.clase))
}

#Crea la tabla de errores que se grafica en los indices de todos los modelos
indices.error.table <- function(indices, nombre = ""){
  err            <- rbind(indices[[4]])
  colnames(err)  <- paste0(c("Error."), colnames(err))
  row.names(err) <- nombre
  return(err)
}

#Crea la tabla de precisiones que se grafica en los indices de todos los modelos
indices.prec.table <- function(indices, nombre = "", idioma){
  prec            <- rbind(indices[[3]])
  colnames(prec)  <- paste0(tr("prec", idioma),".",colnames(prec))
  row.names(prec) <- nombre
  return(prec)
}

#Genera un gauge
new.gauge <- function(val, lab){
  return(paste0("flexdashboard::renderGauge({
                flexdashboard::gauge(round(",val,",2),
                min = 0, max = 100, symbol = '%',
                label = '",lab,"',
                flexdashboard::gaugeSectors(success = c(0, 100)))})"))
}

# Genera los gauges
fill.gauges <- function(indices, titulos) {
    exe(new.gauge(indices, titulos))
}

# Concatena y ejecuta un string como codigo
exe <- function(...){
  eval(parse(text = paste0(...)))
}

extract.code <- function(funcion) {
  code <- paste(head(exe(funcion), 100), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

as.string.c <- function(vect, .numeric = FALSE){
  if(.numeric){
    return(paste0("c(",paste0(vect, collapse = ","),")"))
  }
  else{
    return(paste0("c('",paste0(vect, collapse = "','"),"')"))
  }
}

#Funciones tomadas del paquete dummies

dummy.data.frame<-function (data, names = NULL, omit.constants = TRUE, dummy.classes = getOption("dummy.classes"), 
                            all = TRUE, ...) 
{
  df <- data.frame(row.names = row.names(data))
  new.attr <- list()
  for (nm in names(data)) {
    
    old.attr <- attr(df, "dummies")
    if (isTRUE(nm %in% names || (is.null(names) && (dummy.classes == 
                                                    "ALL" || class(data[, nm]) %in% dummy.classes)))) {
      dummies <- dummy(nm, data, ...)
      if (ncol(dummies) == 1 & omit.constants) {
        dummies <- matrix(nrow = nrow(data), ncol = 0)
      }
      if (ncol(dummies) > 0) 
        new.attr[[nm]] <- (ncol(df) + 1):(ncol(df) + 
                                            ncol(dummies))
    }
    else {
      if (!all) 
        (next)()
      dummies <- data[, nm, drop = FALSE]
    }
    df <- cbind(df, dummies)
    print(df)
  }
  attr(df, "dummies") <- new.attr
  return(df)
}

dummy <- function (x, data = NULL, sep = "", drop = TRUE, fun = as.integer, 
                   verbose = FALSE) 
{
  if (is.null(data)) {
    name <- as.character(sys.call(1))[2]
    name <- sub("^(.*\\$)", "", name)
    name <- sub("\\[.*\\]$", "", name)
  }
  else {
    if (length(x) > 1) 
      stop("More than one variable provided to produce dummy variable.")
    name <- x
    x <- data[, name]
  }
  if (drop == FALSE && class(x) == "factor") {
    x <- factor(x, levels = levels(x), exclude = NULL)
  }
  else {
    x <- factor(x, exclude = NULL)
  }
  if (length(levels(x)) < 2) {
    if (verbose) 
      warning(name, " has only 1 level. Producing dummy variable anyway.")
    return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x), 
                                                               c(paste(name, sep, x[[1]], sep = "")))))
  }
  mm <- model.matrix(~x - 1, model.frame(~x - 1), contrasts = FALSE)
  colnames.mm <- colnames(mm)
  if (verbose) 
    cat(" ", name, ":", ncol(mm), "dummy varibles created\n")
  mm <- matrix(fun(mm), nrow = nrow(mm), ncol = ncol(mm), dimnames = list(NULL, 
                                                                          colnames.mm))
  colnames(mm) <- sub("^x", paste(name, sep, sep = ""), colnames(mm))
  if (!is.null(row.names(data))) 
    rownames(mm) <- rownames(data)
  return(mm)
}

#Funciones tomadas del paquete rpart
rpart.control <- function (minsplit = 20L, minbucket = round(minsplit/3), cp = 0.01, 
          maxcompete = 4L, maxsurrogate = 5L, usesurrogate = 2L, xval = 10L, 
          surrogatestyle = 0L, maxdepth = 30L, ...) 
{
  if (maxcompete < 0L) {
    warning("The value of 'maxcompete' supplied is < 0; the value 0 was used instead")
    maxcompete <- 0L
  }
  if (any(xval < 0L)) {
    warning("The value of 'xval' supplied is < 0; the value 0 was used instead")
    xval <- 0L
  }
  if (maxdepth > 30L) 
    stop("Maximum depth is 30")
  if (maxdepth < 1L) 
    stop("Maximum depth must be at least 1")
  if (missing(minsplit) && !missing(minbucket)) 
    minsplit <- minbucket * 3L
  if ((usesurrogate < 0L) || (usesurrogate > 2L)) {
    warning("The value of 'usesurrogate' supplied was out of range, the default value of 2 is used instead.")
    usesurrogate <- 2L
  }
  if ((surrogatestyle < 0L) || (surrogatestyle > 1L)) {
    warning("The value of 'surrogatestyle' supplied was out of range, the default value of 0 is used instead.")
    surrogatestyle <- 0L
  }
  list(minsplit = minsplit, minbucket = minbucket, cp = cp, 
       maxcompete = maxcompete, maxsurrogate = maxsurrogate, 
       usesurrogate = usesurrogate, surrogatestyle = surrogatestyle, 
       maxdepth = maxdepth, xval = xval)
}


#Funciones tomadas del paquete PSYCH

pairs.panels <- function (x, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE,
                          digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE,
                          jiggle = FALSE, factor = 2, hist.col = "cyan", show.points = TRUE,
                          rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, smoother = FALSE,
                          stars = FALSE, ci = FALSE, alpha = 0.05, ...){
  "panel.hist.density" <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1], usr[2], 0, 1.5))
    tax <- table(x)
    if (length(tax) < 11) {
      breaks <- as.numeric(names(tax))
      y <- tax/max(tax)
      interbreak <- min(diff(breaks)) * (length(tax) -
                                           1)/41
      rect(breaks - interbreak, 0, breaks + interbreak,
           y, col = hist.col)
    }
    else {
      h      <- hist(x, breaks = breaks, plot = FALSE)
      breaks <- h$breaks
      nB <- length(breaks)
      y  <- h$counts
      y  <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    }
    if (density) {
      tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd",
                               adjust = 1.2), silent = TRUE)
      if (class(tryd) != "try-error") {
        d$y <- d$y/max(d$y)
        lines(d)
      }
    }
    if (rug)
      rug(x)
  }
  "panel.cor" <- function(x, y, prefix = "", ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    if (is.null(wt)) {
      r <- cor(x, y, use = "pairwise", method = method)
    }
    else {
      r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1,
                                                        2]
    }
    txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (stars) {
      pval <- r.test(sum(!is.na(x * y)), r)$p
      symp <- symnum(pval, corr = FALSE, cutpoints = c(0,
                                                       0.001, 0.01, 0.05, 1), symbols = c("***", "**",
                                                                                          "*", " "), legend = FALSE)
      txt <- paste0(txt, symp)
    }
    cex <- cex.cor * 0.8/(max(strwidth("0.12***"), strwidth(txt)))
    if (scale) {
      cex1 <- cex * abs(r)
      if (cex1 < 0.25)
        cex1 <- 0.25
      text(0.5, 0.5, txt, cex = cex1)
    }
    else {
      text(0.5, 0.5, txt, cex = cex)
    }
  }
  "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = "red",
                               span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points)
        points(x, y, pch = pch, ...)
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      if (smooth & ci) {
        lml   <- loess(y ~ x, degree = 1, family = "symmetric")
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred  <- predict(lml, newdata = tempx, se = TRUE)
        if (ci) {
          upperci <- pred$fit + confid * pred$se.fit
          lowerci <- pred$fit - confid * pred$se.fit
          polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                              rev(upperci)), col = adjustcolor("light grey",
                                                                               alpha.f = 0.8), border = NA)
        }
        lines(tempx$x, pred$fit, col = col.smooth, ...)
      }
      else {
        if (smooth)
          lines(stats::lowess(x[ok], y[ok], f = span,
                              iter = iter), col = col.smooth)
      }
    }
    if (ellipses)
      draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.smooth,
                   ...)
  }
  "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red",
                         ...) {
    ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin, xmin), max(ymax, xmax))
    xlim <- ylim
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ylim = ylim, xlim = xlim,
               ...)
      }
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lml <- lm(y ~ x)
      if (ci) {
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred    <- predict.lm(lml, newdata = tempx, se.fit = TRUE)
        upperci <- pred$fit + confid * pred$se.fit
        lowerci <- pred$fit - confid * pred$se.fit
        polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                            rev(upperci)), col = adjustcolor("light grey",
                                                                             alpha.f = 0.8), border = NA)
      }
      if (ellipses) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.lm,
                     ...)
      }
      abline(lml, col = col.lm, ...)
    }
  }
  "draw.ellipse" <- function(x = 0, y = 0, xs = 1, ys = 1,
                             r = 0, col.smooth, add = TRUE, segments = 51, ...) {
    angles      <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + x
      ellipse[, 2] <- ellipse[, 2] * ys + y
      if (show.points)
        points(x, y, pch = 19, col = col.smooth, cex = 1.5)
      lines(ellipse, ...)
    }
  }
  "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red",
                              ...) {
    segments = 51
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1] - abs(0.05 * usr[1]), usr[2] + abs(0.05 *
                                                            usr[2]), 0, 1.5))
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ...)
      }
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + xm
      ellipse[, 2] <- ellipse[, 2] * ys + ym
      points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
      if (ellipses)
        lines(ellipse, ...)
    }
  }
  
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (missing(cex.cor))
    cex.cor <- 1
  for (i in 1:ncol(x)) {
    if (is.character(x[[i]])) {
      x[[i]] <- as.numeric(as.factor(x[[i]]))
      colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
    }
  }
  n.obs <- nrow(x)
  confid <- qt(1 - alpha/2, n.obs - 2)
  if (!lm) {
    if (cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.smoother, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.smoother,
            lower.panel = panel.smoother, pch = pch, ...)
    }
  }
  else {
    if (!cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm,
            lower.panel = panel.lm, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.lm, pch = pch, ...)
    }
  }
}


#Colores de ggplot2
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}