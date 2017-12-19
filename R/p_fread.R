# December 2017
# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)

#' shinypipe UI for getting user input to
#' the function fread
#' @param id namespace id (string)
#' @param fileIn list of params for shiny::fileInput to select a file
#' @param header Whether file has headers (default: T)
#' If user doesn't need this control, set it to NULL, and control the parameter through params in s.fread
#' @param sep Selected value for the separator (default: ",")
#' If user doesn't need this control, set it to NULL, and control the parameter through params in s.fread
#' @export
ui.fread <- function(id,
                     fileIn = list(label="Input file"),
                     sep = ",",
                     header = T) {
  ns <- NS(id)

  sepList <- c(
    "Comma" = ",",
    "Semicolon" = ";",
    "Tab" = "\t",
    "Space" = " ",
    "Pipe" = "|",
    "Caret" = "^")

  l <- list(do.call(fileInput, c(ns("file"), fileIn)))
  if (!is.null(sep))    { l <- list(l, selectizeInput(ns("sep"), "Separator", sepList, sep)) }
  if (!is.null(header)) { l <- list(l, checkboxInput(ns("header"), "Has header?", value = header)) }

  tagList(l)
}

#' shinypipe server function for the function fread
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param params list of arguments for fread that are not already supplied through UI
#' for fread
#' @export
s.fread <- function(input, output, session, params = list()) {
  return(reactive({
    validate(need(input$file, message = FALSE))

    l <- list(input=input$file$datapath)
    if (!is.null(input$sep))    { l$sep    <- input$sep}
    if (!is.null(input$header)) { l$header <- input$header}

    do.call(data.table::fread, c(l, params))
  }))
}
