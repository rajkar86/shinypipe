# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for for getting user input for vectors
#' @param id namespace id (string)
#' @param type One of "numeric", "character" or "logical". For now,
#' no validation is provided between character and numeric types and the user is just expected
#' to enter the correct types
#' @param label label for the expression to be evaluated as a vector
#' @param value initial value for the expression to be evaluated as a vector
#' @export
ui.vector <- function(id, type = "numeric", label = "Expression", value = "",
                      showValues = T) {

  ns <- NS(id)

  l <- textInput(ns("expr"), paste0(label, " (", type,")") , width = "100%", value)
  if(type=="logical")
    l <- checkboxGroupInput(ns("expr"), paste0(label, " (", type,")"), choices= c(T,F), inline = T)

  if (showValues)
    l <- fixedRow(column(4,l), column(8,tableOutput(ns("textOut"))))

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
    expr <- input$expr
    req(expr)
    res <- toString(expr)
    if (res == "TRUE, FALSE")
      res <- c(T,F)
    else
      res <- tryCatch(as.vector(eval(parse(text=expr))),
                      error=function(err) {return(c())},
                      warn=function(warn) {return(c())})

    output$textOut <- renderTable(data.table(Values=toString(res)), colnames = F)
    return (list(expr=input$expr, value=res))
  })
}
