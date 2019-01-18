

#' Start predictoR
#' @title This function will start predictoR
#' @return Nothing
#' @description An interactive Shiny application for exploring data.
#' @details This starts the predictoR application on the user's local computer.
#' @keywords predictoR
#' @examples
#' \dontrun{
#'  if(interactive()){
#'    init_predictor()
#'  }
#'}
init_predictor <- function(){
  rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
  info.sys <- Sys.info()
  if(is.null(info.sys)){
    info.sys <- .Platform$OS.type
  }
  Sys.setenv("LANGUAGE" = "ES")
  if(toupper(info.sys) != "WINDOWS"){
    options(encoding = "utf8")
  }else{
    options(encoding = "UTF-8")
  }
  shiny::runApp(appDir = system.file("application", package = "predictoR"), launch.browser = TRUE)
}
