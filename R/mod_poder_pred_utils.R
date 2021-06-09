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

#Gráfica el pairs
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

#Gráfica la densidad de las variables númericas
e_numerico_dens <- function(datos.dens, variable, variable.predecir, label = "${X} ${Y}"){
  label = str_interp(label,list(X=variable,Y=variable.predecir))
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

#Hace la gráfica de distribuciones según la variable predictiva
e_categorico_dist <- function(datos, variable, var.predecir, label = "${X} ${Y}"){
  label = str_interp(label,list(X=variable,Y=var.predecir))
  
  dataplot <-  dist.x.predecir(datos, variable, var.predecir)
  colnames(dataplot) <- c(
    "variable" ,
    "var.predecir" ,
    "count" ,
    "prop"
  )
  dataplot %>%
    group_by(var.predecir) %>%
    e_charts(variable, stack = "grp") %>%
    e_bar(prop, count, label = list(show = T)) %>%
    e_title(label,
            left = "left",
            top = 5,
            textStyle = list(fontSize = 18)) %>%
    e_legend(orient = 'vertical',
             right = '5', top = '15%') %>%
    e_flip_coords() %>%
    e_tooltip(formatter = JS("
                                        function(params){
                                        return('<strong>' + params.value[1] +
                                        '</strong><br />Percent: ' + parseFloat(params.value[0] * 100).toFixed(2)+
                                        '%<br /> ' + 'Count: ' + params.name)}"))%>%
    e_x_axis(
      formatter = e_axis_formatter("percent", digits = 0)) %>%
    e_labels(position = 'inside' ,formatter =  JS("
                                        function(params){
                                        return(params.name + ' (' + parseFloat(params.value[0] *100).toFixed(2) + '%)' )}"))%>% 
    e_datazoom(show = F) %>% 
    e_show_loading()
}


###############################Generar Codigo##############################################

#Gráfica de distribución de la Variable a predecir
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

#Gráfica el pairs
code.pairs.poder <- function(variable.predecir){
  return(paste0("vars.p <- datos[,'",variable.predecir,"']
                col <- rainbow((length(unique(vars.p)) + 1)*2)[seq(2,(length(unique(vars.p)) + 1)*2,2)]
                col <- col[2:length(col)]
                pairs.panels(var.numericas(datos),bg = col[datos[,'",variable.predecir,"']],
                pch= 22, main='', hist.col = gg_color_hue(1), ellipses = FALSE, oma=c(3,3,3,15))
                legend('topright', fill = unique(col[datos[,'",variable.predecir,"']]), legend = c(levels(datos[,'",variable.predecir,"'])))"))
}
