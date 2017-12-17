#' shinypipe UI for getting user input to
#' the function fread
#' @param id namespace id (string)
#' @param label Label for file input
#' @param header Defaut value for the header
#' @param sep Default value for the sep
#' fread.ui()
fread.ui <- function(id,
                     label = "Input file",
                     header = T,
                     sep = ",") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sepList <- c(
    "Comma" = ",",
    "Semicolon" = ";",
    "Tab" = "\t",
    "Spaces" = " "
  )

  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("header"), "Has header?", value = header),
    selectInput(ns("sep"), "Separator", sepList, selected = sep)
  )
}

#' shinypipe server function for the function fread
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param params list of arguments (ones that are not already included in UI)
#' for fread
#' fread.s()
fread.s <- function(input, output, session, params = list()) {
  return(reactive({
    validate(need(input$file, message = FALSE))

    do.call(fread, c(list(input = input$file$datapath,
                          header = input$header,
                          sep = input$sep),
                     params))
  }))
}
