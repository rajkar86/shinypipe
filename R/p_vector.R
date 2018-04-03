# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for for getting user input for vectors
#' @param id namespace id (string)
#' @param label label for the expression to be evaluated as a vector
#' @param value initial value for the expression to be evaluated as a vector
#' @export
ui.vector <- function(id, label = "Expression:", value = "") {

  ns <- NS(id)

  ##TODO support other input methods
  # column(4,selectizeInput(ns("type"), NULL, choices = c("Expr" = "Expression") )),
  l <- list(
    textInput(ns("expr"), label, width = "100%", value),
    actionButton(ns("submit"), "Submit", width = "100")
  )

  tagList(l)
}

#' shinypipe server function for getting user input for vectors
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return A vector evaluated from the provided expression
#' @export
s.vector <- function(input, output, session) {

  reactive({
    input$submit
    as.vector(eval(parse(text=isolate(input$expr))))
  })
}
