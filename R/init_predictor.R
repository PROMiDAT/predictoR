
#' STARTUP
#' @author Andres
#' @return Sniny app
#' @export
#'
init_predictor <- function(){
  rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
  Sys.setenv("LANGUAGE" = "ES")
  options(encoding = "utf8")
  shiny::runApp(appDir = system.file("application", package = "PredictoR"),launch.browser = TRUE)
}
