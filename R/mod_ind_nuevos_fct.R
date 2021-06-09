# Funciones tomadas de los paquetes shinyWidgets y shiny

radioGroupButtons <-   function (inputId, label = NULL, choices = NULL, selected = NULL, 
                                 status = "default", size = "normal", direction = "horizontal", 
                                 justified = FALSE, individual = FALSE, checkIcon = list(), 
                                 width = NULL, choiceNames = NULL, choiceValues = NULL, disabled = FALSE) 
{
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- shiny::restoreInput(id = inputId, default = selected)
  if (!is.null(selected) && length(selected) > 1) 
    stop("selected must be length 1")
  if (is.null(selected)) 
    selected <- args$choiceValues[[1]]
  size <- match.arg(arg = size, choices = c("xs", "sm", "normal", 
                                            "lg"))
  direction <- match.arg(arg = direction, choices = c("horizontal", 
                                                      "vertical"))
  divClass <- if (individual) 
    ""
  else "btn-group"
  if (!individual & direction == "vertical") {
    divClass <- paste0(divClass, "-vertical")
  }
  if (justified) {
    if (direction != "vertical") {
      divClass <- paste(divClass, "btn-group-justified")
    }
    else {
      divClass <- paste(divClass, "btn-block")
    }
  }
  if (size != "normal") {
    divClass <- paste0(divClass, " btn-group-", size)
  }
  radioGroupButtonsTag <- tags$div(class = "form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline", 
                                   style = if (!is.null(width)) 
                                     paste("width:", shiny::validateCssUnit(width)), if (!is.null(label)) 
                                       tags$label(class = "control-label", `for` = inputId, 
                                                  label), if (!is.null(label)) 
                                                    tags$br(), tags$div(id = inputId, class = "radioGroupButtons", 
                                                                        style = if (justified) 
                                                                          "width: 100%;", tags$div(class = divClass, role = "group", 
                                                                                                   `aria-label` = "...", `data-toggle` = "buttons", 
                                                                                                   class = "btn-group-container-sw", generateRGB(inputId, 
                                                                                                                                                 args, selected, status, size, checkIcon, disabled = disabled))))
  attachShinyWidgetsDep(radioGroupButtonsTag)
}



normalizeChoicesArgs <- function (choices, choiceNames, choiceValues, mustExist = TRUE) 
{
  if (is.null(choices)) {
    if (is.null(choiceNames) || is.null(choiceValues)) {
      if (mustExist) {
        stop("Please specify a non-empty vector for `choices` (or, ", 
             "alternatively, for both `choiceNames` AND `choiceValues`).")
      }
      else {
        if (is.null(choiceNames) && is.null(choiceValues)) {
          return(list(choiceNames = NULL, choiceValues = NULL))
        }
        else {
          stop("One of `choiceNames` or `choiceValues` was set to ", 
               "NULL, but either both or none should be NULL.")
        }
      }
    }
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
  }
  else {
    if (!is.null(choiceNames) || !is.null(choiceValues)) {
      warning("Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
    }
    choices <- choicesWithNames(choices)
    choiceNames <- names(choices)
    choiceValues <- unname(choices)
  }
  return(list(choiceNames = as.list(choiceNames), choiceValues = as.list(as.character(choiceValues))))
}

generateRGB <- function (inputId, choices, selected, status, size, checkIcon, 
                         disabled = FALSE) 
{
  btn_wrapper <- function(...) {
    htmltools::tags$div(class = "btn-group btn-group-toggle", 
                        class = if (size != "normal") 
                          paste0("btn-group-", size), role = "group", ...)
  }
  if (!is.null(checkIcon) && !is.null(checkIcon$yes)) {
    displayIcon <- TRUE
  }
  else {
    displayIcon <- FALSE
  }
  mapply(FUN = function(name, value) {
    btn_wrapper(tags$button(class = paste0("btn radiobtn btn-", 
                                           status), class = if (value %in% selected) 
                                             "active", if (displayIcon) 
                                               tags$span(class = "radio-btn-icon-yes", checkIcon$yes), 
                            if (displayIcon) 
                              tags$span(class = "radio-btn-icon-no", checkIcon$no), 
                            disabled = if (isTRUE(disabled)) 
                              "disabled", class = if (isTRUE(disabled)) 
                                "disabled", tags$input(type = "radio", autocomplete = "off", 
                                                       name = inputId, value = value, checked = if (value %in% 
                                                                                                    selected) 
                                                         "checked"), if (is.list(name)) 
                                                           name
                            else HTML(name)))
  }, name = choices$choiceNames, value = choices$choiceValues, 
  SIMPLIFY = FALSE, USE.NAMES = FALSE)
}


attachShinyWidgetsDep <- function (tag, widget = NULL) 
{
  dep <- html_dependency_shinyWidgets()
  if (!is.null(widget)) {
    
    if (widget == "multi") {
      dep <- list(dep, html_dependency_multi())
    }
    else if (widget == "dropdown") {
      dep <- list(dep, htmltools::htmlDependency(name = "dropdown-patch", 
                                                 version = packageVersion("shinyWidgets"), src = c(href = "shinyWidgets/dropdown"), 
                                                 script = "dropdown-click.js"))
    }
    else if (widget == "sw-dropdown") {
      dep <- list(dep, htmltools::htmlDependency(name = "sw-dropdown", 
                                                 version = packageVersion("shinyWidgets"), src = c(href = "shinyWidgets/sw-dropdown"), 
                                                 script = "sw-dropdown.js", stylesheet = "sw-dropdown.css"))
    }
    else if (widget == "bttn") {
      dep <- list(dep, html_dependency_bttn())
    }
  }
  htmltools::attachDependencies(tag, dep, append = TRUE)
}
choicesWithNames <- function (choices) 
{
  if (hasGroups(choices)) {
    processGroupedChoices(choices)
  }
  else {
    processFlatChoices(choices)
  }
}
processFlatChoices <- function (choices) 
{
  choices <- setDefaultNames(asCharacter(choices))
  as.list(choices)
}
processGroupedChoices <- function (choices) 
{
  stopifnot(is.list(choices))
  choices <- asNamed(choices)
  choices <- mapply(function(name, choice) {
    choiceIsGroup <- isGroup(choice)
    if (choiceIsGroup && name == "") {
      stop("All sub-lists in \"choices\" must be named.")
    }
    else if (choiceIsGroup) {
      processFlatChoices(choice)
    }
    else {
      as.character(choice)
    }
  }, names(choices), choices, SIMPLIFY = FALSE)
  setDefaultNames(choices)
}
hasGroups <- function (choices) 
{
  is.list(choices) && any(vapply(choices, isGroup, logical(1)))
}
isGroup <- function (choice) 
{
  is.list(choice) || !is.null(names(choice)) || length(choice) > 
    1 || length(choice) == 0
}
html_dependency_shinyWidgets <- function() 
{
  htmltools::htmlDependency(name = "shinyWidgets", version = packageVersion("shinyWidgets"), 
                            src = c(href = "shinyWidgets", file = "assets"), package = "shinyWidgets", 
                            script = "shinyWidgets-bindings.min.js", stylesheet = "shinyWidgets.min.css", 
                            all_files = FALSE)
}
html_dependency_multi <- function() 
{
  htmlDependency(name = "multi", version = "1.4.0", package = "shinyWidgets", 
                 src = c(href = "shinyWidgets/multi", file = "assets/multi"), 
                 script = "multi.min.js", stylesheet = c("multi.min.css"))
}

html_dependency_bttn <- function() 
{
  htmlDependency(name = "bttn", version = "0.2.4", src = c(href = "shinyWidgets/bttn", 
                                                           file = "assets/bttn"), package = "shinyWidgets", stylesheet = "bttn.min.css")
}
asNamed <- function (x) 
{
  if (is.null(names(x))) {
    names(x) <- character(length(x))
  }
  x
}

setDefaultNames <- function (x) 
{
  x <- asNamed(x)
  emptyNames <- names(x) == ""
  names(x)[emptyNames] <- as.character(x)[emptyNames]
  x
}
#Funciones tomadas del paquete SHINY
asCharacter <- function (x) 
{
  setNames(as.character(x), names(x))
}
setNames <- function (object = nm, nm) 
{
  names(object) <- nm
  object
}
#Funciones tomadas del paquete UTILS

packageVersion <- function (pkg, lib.loc = NULL) 
{
  res <- suppressWarnings(packageDescription(pkg, lib.loc = lib.loc, 
                                             fields = "Version"))
  if (!is.na(res)) 
    package_version(res)
  else stop(packageNotFoundError(pkg, lib.loc, sys.call()))
}
packageDescription <- function (pkg, lib.loc = NULL, fields = NULL, drop = TRUE, encoding = "") 
{
  retval <- list()
  if (!is.null(fields)) {
    fields <- as.character(fields)
    retval[fields] <- NA
  }
  pkgpath <- if (is.null(lib.loc)) {
    if (pkg == "base") 
      file.path(.Library, "base")
    else if (isNamespaceLoaded(pkg)) 
      getNamespaceInfo(pkg, "path")
    else if ((envname <- paste0("package:", pkg)) %in% search()) {
      attr(as.environment(envname), "path")
    }
  }
  if (is.null(pkgpath)) 
    pkgpath <- ""
  if (pkgpath == "") {
    libs <- if (is.null(lib.loc)) 
      .libPaths()
    else lib.loc
    for (lib in libs) if (file.access(file.path(lib, pkg), 
                                      5) == 0L) {
      pkgpath <- file.path(lib, pkg)
      break
    }
  }
  if (pkgpath == "") {
    warning(gettextf("no package '%s' was found", pkg), domain = NA)
    return(NA)
  }
  if (file.exists(file <- file.path(pkgpath, "Meta", "package.rds"))) {
    desc <- readRDS(file)$DESCRIPTION
    if (length(desc) < 1) 
      stop(gettextf("metadata of package '%s' is corrupt", 
                    pkg), domain = NA)
    desc <- as.list(desc)
  }
  else if (file.exists(file <- file.path(pkgpath, "DESCRIPTION"))) {
    dcf <- read.dcf(file = file)
    if (NROW(dcf) < 1L) 
      stop(gettextf("DESCRIPTION file of package '%s' is corrupt", 
                    pkg), domain = NA)
    desc <- as.list(dcf[1, ])
  }
  else file <- ""
  if (nzchar(file)) {
    enc <- desc[["Encoding"]]
    if (!is.null(enc) && !is.na(encoding)) {
      if (missing(encoding) && Sys.getlocale("LC_CTYPE") == 
          "C") 
        encoding <- "ASCII//TRANSLIT"
      if (encoding != enc) {
        newdesc <- try(lapply(desc, iconv, from = enc, 
                              to = encoding))
        dOk <- function(nd) !inherits(nd, "error") && 
          !anyNA(nd)
        ok <- dOk(newdesc)
        if (!ok) 
          ok <- dOk(newdesc <- try(lapply(desc, iconv, 
                                          from = enc, to = paste0(encoding, "//TRANSLIT"))))
        if (!ok) 
          ok <- dOk(newdesc <- try(lapply(desc, iconv, 
                                          from = enc, to = "ASCII//TRANSLIT", sub = "?")))
        if (ok) 
          desc <- newdesc
        else warning("'DESCRIPTION' file has an 'Encoding' field and re-encoding is not possible", 
                     call. = FALSE)
      }
    }
    if (!is.null(fields)) {
      ok <- names(desc) %in% fields
      retval[names(desc)[ok]] <- desc[ok]
    }
    else retval[names(desc)] <- desc
  }
  if ((file == "") || (length(retval) == 0)) {
    warning(gettextf("DESCRIPTION file of package '%s' is missing or broken", 
                     pkg), domain = NA)
    return(NA)
  }
  if (drop & length(fields) == 1L) 
    return(retval[[1L]])
  class(retval) <- "packageDescription"
  if (!is.null(fields)) 
    attr(retval, "fields") <- fields
  attr(retval, "file") <- file
  retval
}
