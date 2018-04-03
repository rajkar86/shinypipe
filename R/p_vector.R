# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for for getting user input for vectors
#' @param id namespace id (string)
#' @param type One of "numeric", "character" or "logical"
#' @param label label for the expression to be evaluated as a vector
#' @param value initial value for the expression to be evaluated as a vector
#' @export
ui.vector <- function(id, type = "numeric", label = "Expression", value = "") {

  ns <- NS(id)

  w <- textInput(ns("expr"), paste0(label, " (", type,")") , width = "100%", value)
  if(type=="logical")
    w <- checkboxGroupInput(ns("expr"), paste0(label, " (", type,")"), choices= c(T,F), inline = T)

  l <- fixedRow(
    column(3,w),
    column(9,tableOutput(ns("textOut")))
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
    res <- toString(input$expr)
    if (res == "TRUE, FALSE")
      res <- c(T,F)
    else
      res <- as.vector(eval(parse(text=(input$expr))))

    output$textOut <- renderTable(data.table(Values=toString(res)), colnames = F)
    res
  })
}
