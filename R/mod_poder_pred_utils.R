#Calcula proporciones
dist.x.predecir <- function(data, variable, variable.predecir) {
  if(variable != variable.predecir){
    data <- data %>%
      dplyr::group_by_at(c(variable, variable.predecir)) %>%
      dplyr::summarise(count = n())
  } else {
    data <- data %>% dplyr::group_by_at(variable) %>%
      dplyr::summarise(count = n())
  }
  data <- data %>% dplyr::mutate(prop = round(count/sum(count),4))
  return(data)
}


#Grafica el pairs
pairs.poder  <- function(datos,variable.predecir){
      vars.p <- datos[,variable.predecir]
      col    <- rainbow((length(unique(vars.p)) + 1)*2)[seq(2,(length(unique(vars.p)) + 1)*2,2)]
      col    <- col[2:length(col)]
      r      <- pairs.panels(var.numericas(datos),bg = col[datos[,variable.predecir]],
      pch= 22, main='', hist.col = gg_color_hue(1), ellipses = FALSE, oma=c(3,3,3,15))
      legend('topright', fill = unique(col[datos[,variable.predecir]]), 
                legend = c(levels(datos[,variable.predecir])))
  return(r)
}

#Grafica la densidad de las variables numericas
e_numerico_dens <- function(datos.dens, variable, variable.predecir, label = "${X} ${Y}"){
  label = stringr::str_interp(label,list(X=variable,Y=variable.predecir))
  datos.plot <- data.frame(
    "variable" = datos.dens[, variable],
    "variable.predecir" = datos.dens[, variable.predecir]
  )
  datos.plot %>%
  group_by(variable.predecir) %>%
                 e_charts() %>%
                 e_density(variable) %>%
                 e_title(label,
                         left = 'left',
                         top = 5,
                         textStyle = list(fontSize = 18)) %>%
                 e_legend(orient = 'vertical',
                          right = '5', top = '15%') %>%
                 e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()
}

#Hace la grafica de proporciones segun la variable predictiva
plot.dist.cat <- function(datos,variable, var.predecir, colores = NA, label.size = 9.5, label = "${X} ${Y}"){
  label = stringr::str_interp(label,list(X=variable,Y=var.predecir))
  colores <- gg_color_hue(length(unique(datos[,var.predecir])))
  label.size <- label.size - length(unique(datos[,variable]))
  label.size <- ifelse(label.size < 3, 3, label.size)
  data <- dist.x.predecir(datos, variable, var.predecir)
  ggplot(data, aes(fct_reorder(data[[variable]], count, .desc = T), prop, fill = data[[var.predecir]])) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = paste0(count, ' (', scales::percent(prop), ')'), y = prop), color = 'black',
              position = position_stack(vjust = .5), size = label.size) +
    theme_minimal() +
    theme(text = element_text(size=15)) +
    scale_fill_manual(values = colores) +
    scale_y_continuous(labels = scales::percent)+
    coord_flip() +
    labs(title = label, x = '', y = '') +
    guides(fill = guide_legend(reverse=T)) +
    theme(legend.position = 'top', legend.title = element_blank())
}

e_categorico_dist <- function(datos,variable, var.predecir, colores = NA, label.size = 9.5, label = "${X} ${Y}"){
  label = stringr::str_interp(label,list(X=variable,Y=var.predecir))
  
  dataplot <-  dist.x.predecir(datos, variable, var.predecir)
  colnames(dataplot) <- c(
    "variable" ,
    "variable.predecir" ,
    "count" ,
    "prop"
  )
  #dataplot <<- dataplot[order(dataplot$variable,dataplot$count,  decreasing = T), ]
  dataplot %>%
    group_by(variable.predecir) %>%
    e_charts(variable, stack = "grp") %>%
    e_bar(prop, count, label = list(show = T)) %>%
    e_title(label,
            left = "left",
            top = 5,
            textStyle = list(fontSize = 18)) %>%
    e_legend(orient = 'vertical',
             right = '5', top = '15%') %>%
    e_flip_coords() %>%
    e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.value[1] +
                                        '</strong><br />Percent: ' + parseFloat(params.value[0] * 100).toFixed(2)+
                                        '%<br /> ' + 'Count: ' + params.name)}"))%>%
    e_x_axis(
      formatter = e_axis_formatter("percent", digits = 0)) %>%
    e_labels(position = 'inside' ,formatter = htmlwidgets::JS("
                                        function(params){
                                        return(params.name + ' (' + parseFloat(params.value[0] *100).toFixed(2) + '%)' )}"))%>% 
    e_datazoom(show = F) %>% 
    e_show_loading()
}


###############################Generar Codigo##############################################

#Grafica de distribucion de la Variable a predecir
code.dist.varpred <- function(var) {
  paste0(
    "datos.plot <- data.frame (\n",
    "  label = levels(datos[['", var, "']]), value = summary(datos[['", var, "']],\n",
    "  maxsum = length(levels(datos[['", var, "']])))\n",
    ")\n\n",
    "datos.plot %>% e_charts(label) %>% e_bar(value, name = var) %>%\n",
    "  e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()\n"
  )
}

#Grafica el pairs
code.pairs.poder <- function(variable.predecir){
  return(paste0("vars.p <- datos[,'",variable.predecir,"']
                col <- rainbow((length(unique(vars.p)) + 1)*2)[seq(2,(length(unique(vars.p)) + 1)*2,2)]
                col <- col[2:length(col)]
                pairs.panels(var.numericas(datos),bg = col[datos[,'",variable.predecir,"']],
                pch= 22, main='', hist.col = gg_color_hue(1), ellipses = FALSE, oma=c(3,3,3,15))
                legend('topright', fill = unique(col[datos[,'",variable.predecir,"']]), legend = c(levels(datos[,'",variable.predecir,"'])))"))
}
df <- data.frame(
  x = 1:20, 
  testing123 = runif(20, 10, 100),
  v = runif(20, 15, 100),
  w = runif(20, 1, 100),
  z = runif(20, 25, 75)
)

########################MULTIPLE PLOT##################################
# df %>% 
#   e_charts(x) %>% 
#   e_y_axis(gridIndex = 0, min=0,max=100) %>% 
#   e_y_axis(gridIndex = 1, min=0,max=100) %>% 
#   e_y_axis(gridIndex = 2, min=0,max=100) %>%
#   e_y_axis(gridIndex = 3, min=0,max=100) %>% 
#   e_x_axis(gridIndex = 0, min=0,max=20) %>%
#   e_x_axis(gridIndex = 1, min=0,max=20) %>%
#   e_x_axis(gridIndex = 2, min=0,max=20) %>%
#   e_x_axis(gridIndex = 3, min=0,max=20) %>% 
#   e_grid(x= '7%',y='7%',width='38%',height='38%') %>%
#   e_grid(x2= '7%',y='7%',width='38%',height='38%') %>%
#   e_grid(x= '7%',y2='7%',width='38%',height='38%') %>%
#   e_grid(x2= '7%',y2='7%',width='38%',height='38%') %>%
#   e_line(serie = w, x_index = 0, y_index = 0) %>%
#   e_line(serie = z, x_index = 1, y_index = 1) %>%
#   e_line(serie = v, x_index = 2, y_index = 2) %>%
#   e_line(serie = testing123, x_index = 3, y_index = 3) %>%
#   e_tooltip(trigger = "axis") 
# 
# 
# df <- data.frame(
#   x = 1:20, 
#   testing123 = runif(20, 10, 100),
#   v = runif(20, 15, 100),
#   w = runif(20, 1, 100),
#   z = runif(20, 25, 75)
# )
# 
# # remember to specify the ids
# p1 <- e_charts(df, x, elementId = "chart1") %>% 
#   e_line(serie = w, name = "common") %>% 
#   e_tooltip(trigger = "axis")
# 
# p2 <- e_charts(df, x, elementId = "chart2") %>% 
#   e_line(serie = v, name = "common") %>% 
#   e_tooltip(trigger = "axis")
# 
# p3 <- e_charts(df, x, elementId = "chart3") %>% 
#   e_line(serie = z, name = "common") %>% 
#   e_tooltip(trigger = "axis")
# 
# p4 <- e_charts(df, x) %>% 
#   e_line(serie = testing123, name = "common") %>% 
#   e_tooltip(trigger = "axis") %>% 
#   e_connect(c("chart1", "chart2", "chart3")) # connect the last chart to all others
# 
# # this is a convenience function to display the charts in a grid
# e_arrange(p1, p2, p3, p4, rows = 2, cols = 2)
