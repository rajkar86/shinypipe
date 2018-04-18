# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param id namespace id (string)
#' @param models reactive list of models
#' @export
ui.caretModelList <- function(id, models, selected = NULL) {
  ns <- NS(id)

  require(caret)

  l <- list(
    selectizeInput(ns("method"), "Method", models(), selected),
    uiOutput(ns("typeOut")),
    uiOutput(ns("paramsOut"))
  )

  tagList(l)
}

#' shinypipe server function for returning a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param return shiny session
#' @export
s.caretModelList <- function(input, output, session) {

  model.info <- reactive({
    req(input$method)
    getModelInfo(input$method, regex = F)[[input$method]]
  })

  model.params <- reactive(model.info()$parameter)
  model.type   <- reactive(model.info()$type)

  output$typeOut <- renderUI(shiny::radioButtons(session$ns("type"), NULL, model.type(), inline = T))
  output$paramsOut <- renderUI(ui.listOfVectors(session$ns("params"), model.params))

  return(reactive({
    r.params <- callModule(s.listOfVectors, "params", reactive(model.params()$parameter))
    list(
      method = input$method,
      type = input$type,
      expr = r.params()$expr,
      params = r.params()$value
  )}))
}
