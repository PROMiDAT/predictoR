

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
  Sys.setenv("LANGUAGE" = "ES")
  options(encoding = "utf8")
  shiny::runApp(appDir = system.file("application", package = "PredictoR"), launch.browser = TRUE)
}
