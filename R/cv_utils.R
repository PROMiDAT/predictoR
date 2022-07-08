# library(caret)
# library(tidyverse)
# library(scales)
# 
# resumen.lineas <- function(resultados, labels = c("Global", "repetición")) {
#   datos.grafico <- pivot_longer(
#     resultados,
#     cols      = -rep,
#     names_to  = 'name',
#     values_to = 'value'
#   )
#   
#   color <- gg_color_hue(length(unique(datos.grafico[["name"]])))
#   
#   datos.grafico |> 
#     group_by(name)|> 
#     e_charts(rep, color = color) |> 
#     e_line(value,
#               symbolSize = 5)|> 
#     e_y_axis(formatter = e_axis_formatter("percent", 
#                                           digits = 0)) |>
#     e_axis_labels(x = labels[2],
#                   y = paste('%', labels[1])) |> 
#     e_title(labels[1],
#             left = "center",
#             top = 5,
#             textStyle = list(fontSize = 20)) |>   
#     e_tooltip(formatter = e_JS("function(params){
#                                            return('<strong>' + params.value[0] + ' </strong>' +",
#                                " parseFloat(params.value[1] * 100).toFixed(1) + '%' )}")) |>  
#     e_datazoom(show = F) |> 
#     e_legend(show = T, type = "scroll", bottom = 1) |>  
#     e_show_loading()
# }
# 
resumen.puntos <- function(datos.grafico, labels = c("Global", "iteracion")) {
  # datos.grafico <- pivot_longer(
  #   resultados,
  #   cols      = -rep,
  #   names_to  = 'name',
  #   values_to = 'value'
  # )
  
  color <- gg_color_hue(length(unique(datos.grafico[["name"]])))
  datos.grafico <- datos.grafico |>
    dplyr::group_by( name ) |>
    dplyr::summarise(value = mean(value)) |>
    dplyr::arrange(desc(value))
  
  resumen <- datos.grafico |>
    group_by( name )|>
    e_charts( name , color = color) |>
    e_scatter(value,
              symbolSize = 15)|>
    e_labels(show     = TRUE,
             position = 'top' ,
             formatter =  e_JS("function(params){
                                           return(parseFloat(params.value[1] *100).toFixed(2) + '%' )}")) |>
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
    e_datazoom(show = F) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()
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
# resultado.global <- readRDS("global.rds")
# resultado.no <- readRDS("datos_no.rds")
# resultado.si <- readRDS("datos_si.rds")
# resumen.puntos(resultado.no, c("Global", "repetición"))
# resumen.lineas(resultado.no, c("Global", "repetición"))
# plot_resumen(resultado.no, c("Global", "repetición"))

#####################   KNN   ########################

indices.cv <- function(category, cant.vc, kernels, MCs.knn){
  ind.categ  <- vector(mode = "list",   length = length(category))
  names(ind.categ) <- category
  
  for (cat in category) {
    ind.categ[[cat]] <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  }
  
  value     <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  name      <- vector(mode = "character", length = cant.vc * length(kernels))
  rep       <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  indice    <- 1
  
  for (kernel in 1:length(kernels)){
    n <- kernel * cant.vc
    rep[indice:(n)]       <- 1:cant.vc
    name[indice:(n)]      <- kernels[kernel]
    value[indice:(n)]     <- sapply(MCs.knn[[paste0("MCs.", kernels[kernel])]], 
                                    precision.global)
    
    for (cat in category) {
      ind.categ[[cat]][indice:(n)] <- sapply(MCs.knn[[paste0("MCs.", kernels[kernel])]], 
                                             precision(cat))
    }
    indice <- indice + cant.vc
  }
  
  grafico   <- data.frame(rep, name, value)
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


