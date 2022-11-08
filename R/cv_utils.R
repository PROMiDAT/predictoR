cv.values <- function(datos){
  modelos <- unique(datos$name)
  puntos <- vector(mode = "list", length = length(modelos))
  j <- 1
  for (modelo in modelos) {
    res <- datos[datos$name == modelo,]
    punto <- vector(mode = "list", length = nrow(res))
    for (i in 1:nrow(res)) {
      punto[i] <- append(list(c(res$rep[i], res$value[i])), punto)
    }
    puntos[[j]] <- list(data = punto,
                        type  = "line",  
                        color = res$color, 
                        name = modelo)
    j <- j + 1
  }
  return(puntos)
}

resumen.lineas <- function(datos.grafico, labels = c("Global", "repeticion")) {
  puntos <- cv.values(datos.grafico)
  opts <- list(
    xAxis = list(show = TRUE),
    yAxis = list(show = TRUE),
    series = puntos)
  
  comp_plot <- e_charts() |>  
    e_list(opts) |>  
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left = "center",
            top = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(1) + '%' )}")) |>
    e_datazoom(show = F,startValue=1) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  comp_plot
}

resumen.barras <- function(datos.grafico, labels = c("Global", "iteracion"), error = FALSE) {
    
  if(!error){  
    datos.grafico <- datos.grafico |>
      dplyr::group_by( name, color ) |>
      dplyr::summarise(value = mean(value), .groups = 'drop') |>
      dplyr::arrange(desc(value))
  }else{
    datos.grafico <- datos.grafico |>
      dplyr::group_by( name, color ) |>
      dplyr::summarise(value = mean(value), .groups = 'drop') |>
      dplyr::arrange(value)
  }
  
  resumen <- datos.grafico |>
    e_charts( name) |>
    e_bar(value, name = var) |> 
    e_add_nested("itemStyle", color) |>
    e_labels(show     = TRUE,
             position = 'top' ,
             formatter =  e_JS("function(params){
                                           return(parseFloat(params.value[1] *100).toFixed(2) + '%' )}")) |>
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left  = "center",
            top   = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(1) + '%' )}")) |>
    e_datazoom(show = F) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  resumen$x$opts$legend$data <- datos.grafico$name
  resumen
}

resumen.barras2 <- function(datos.grafico, labels = c("Global", "iteracion")) {
  
  datos.grafico <- datos.grafico |>
    dplyr::group_by( name, color ) |>
    dplyr::summarise(value = mean(value), .groups = 'drop') |>
    dplyr::arrange(desc(value))
  
  resumen <- datos.grafico |>
    e_charts( name) |>
    e_bar(value, name = var) |> 
    e_add_nested("itemStyle", color) |>
    e_labels(show     = TRUE,
             position = 'top' ,
             formatter =  e_JS("function(params){
                                           return(parseFloat(params.value[1] *100).toFixed(2) + '%' )}")) |>
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left  = "center",
            top   = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(1) + '%' )}")) |>
    e_datazoom(show = F) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  resumen$x$opts$legend$data <- datos.grafico$name
  resumen
}

precision <- function(clase){
  function(mc){
    indices = general.indexes(mc = mc)
    indices$category.accuracy[clase]
  }
}

precision.global <- function(x) sum(diag(x))/sum(x)

#####################   Indices CrossVal   ########################

indices.cv <- function(category, cant.vc, kernels, MCs.knn){
  ind.categ  <- vector(mode = "list",   length = length(category))
  names(ind.categ) <- category
  
  for (cat in category) {
    ind.categ[[cat]] <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  }
  col_      <- gg_color_hue(length(kernels))
  value     <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  name      <- vector(mode = "character", length = cant.vc * length(kernels))
  color     <- vector(mode = "character", length = cant.vc * length(kernels))
  rep       <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  indice    <- 1
  
  for (kernel in 1:length(kernels)){
    n <- kernel * cant.vc
    rep[indice:(n)]       <- 1:cant.vc
    name[indice:(n)]      <- kernels[kernel]
    color[indice:(n)]     <- col_[kernel]
    value[indice:(n)]     <- sapply(MCs.knn[[paste0("MCs.", kernels[kernel])]], 
                                    precision.global)
    
    for (cat in category) {
      ind.categ[[cat]][indice:(n)] <- sapply(MCs.knn[[paste0("MCs.", kernels[kernel])]], 
                                             precision(cat))
    }
    indice <- indice + cant.vc
  }
  
  grafico    <- data.frame(rep, name, value, color)
  
  resultados <- list(grafico = grafico, global = value, categories = ind.categ)
  return(list(grafico = grafico, global = value, categories = ind.categ))
}

# indices.cvknn2 <- function(category, cant.vc, kernels, MCs.knn){
#   ind.categ  <- vector(mode = "list",   length = length(category))
#   names(ind.categ) <- category
#   
#   for (cat in category) {
#     ind.categ[[cat]]  <-  data.frame(sapply(MCs.knn, function(mc) sapply(mc, precision(cat))))
#   }
#   
#   data2 <- data.frame(sapply(ind.categ, function(mc) sapply(mc, function(x) x)))
#   
#   for (cat in category) {
#     ind.categ[[cat]]  <-  data2[[cat]]
#   }
#   grafico <- data.frame(sapply(MCs.knn, function(mc) sapply(mc, precision.global)))
#   grafico <- grafico |>
#     tidyr::pivot_longer(cols = starts_with("MCs"), names_to = "name", values_to = "value", names_prefix = "MCs.")
#   grafico <- grafico |> arrange(name) |> mutate(rep = rep(1:cant.vc, length(kernels))) |> data.frame()
#   
#   return(list(grafico = grafico, global = grafico$value, categories = ind.categ))
# }

#GRAFICO MARKLINE
# # 
# mtcars |>
#   e_charts(mpg) |>
#   e_line(wt) |>
#   e_mark_line(data = list(
#     list(xAxis = 5, yAxis = 0, symbolSize = 1, lineStyle = list(width = 4, type = "solid")),
#     list(xAxis = 5, yAxis = 4.5, symbol = "circle", 
#          symbolSize = 20, name = 11,
#          label = list(show = TRUE, distance = 15, formatter = "{b} %"), 
#          tooltip = list(formatter = e_JS(paste0("function(params){",
#                                                 "return('<b>Log(lambda.1se): </b>' + ",
#                                                 "params.name)}"))))
#   ))|>
#   e_tooltip()

# resumen |> e_mark_line(data = list(list(xAxis = resumen$x$opts$series[[2]]$data[[1]]$value[1], yAxis = 0, symbolSize = 1),
#                                    list(symbol = "none", xAxis = resumen$x$opts$series[[2]]$data[[1]]$value[1], yAxis = resumen$x$opts$series[[2]]$data[[1]]$value[2])))

##OBTENR PARAMETROS DE FUNCION
#as.list(args(append))[-length(as.list(args(append)))]
#res <- names(as.list(args(append))[-length(as.list(args(append)))])


