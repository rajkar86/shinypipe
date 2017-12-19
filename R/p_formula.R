# December 2017
# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)

#' shinypipe UI for getting creating an R formula
#' @param id namespace id (string)
#' @param y For y selection, list of params for either shiny::selectizeInput or shiny::radioButtons
#' as determined by theme parameter. inputId should not be specified, and for selectizeInput, 'multiple' should not be specified
#' (default for label for the widget is "Y")
#' @param x For x selection, list of params for either shiny::selectizeInput or shiny::radioButtons
#' as determined by theme parameter. inputId should not be specified, and for selectizeInput, 'multiple' should not be specified
#' (default for label for the widget is "X")
#' @param intercept default value for whether to use intercept (if user doesn't need this control, set it to NULL, and control the parameter through s.formula)
#' @param theme 'small' or 'large' (default is 'large')
#' @export

ui.formula <- function(id, y, x, intercept = NULL, theme = 'large') {
  # Create a namespace function using the provided id
  ns <- NS(id)

  if(!exists("label", where=y)) { y$label <- "Y"}
  if(!exists("label", where=x)) { x$label <- "X"}

  l <- switch(theme,
              "large" = list(do.call(selectizeInput, c(inputId = ns("y"), multiple = F, y)),
                             do.call(selectizeInput, c(inputId = ns("x"), multiple = T, x))),
              "small" = list(do.call(radioButtons, c(inputId = ns("y"), y)),
                             do.call(checkboxGroupInput, c(inputId = ns("x"), x)))
  )

  if (!is.null(intercept))
    l <- list(l, checkboxInput(ns("intercept"), label = "Include Intercept", value = intercept))

  tagList(l)
}

#' shinypipe server function for the response (dependent) variable in the formula
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @export
s.formula.y <- function(input, output, session) {
  return(reactive({
    validate(need(input$y, message = F))
    input$y
  }))
}

#' shinypipe server function for the input (independent) variable in the formula
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @export
s.formula.x <- function(input, output, session) {
  return(reactive({
    validate(need(input$x, message = F))
    input$x
  }))
}

#' shinypipe server function that returns the formula
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param intercept whether to use intercept (will be used only if intercept is NULL in ui.formula)
#' @export
s.formula <- function(input, output, session, intercept = T) {
  return(reactive({
    validate(
      need(input$x, message = F),
      need(input$y, message = F)
    )
    fml <- paste(input$y, "~", paste(input$x, collapse = "+"))

    intercept <- ifelse(is.null(input$intercept), intercept, input$intercept)
    if (!intercept) { fml <- paste(fml, "+ 0") }

    stats::as.formula(fml)
  }))
}

