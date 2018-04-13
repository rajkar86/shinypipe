# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param id namespace id (string)
#' @param models reactive list of models
#' @param selected model to be selected by default
#' @export
ui.caretModel <- function(id, models, selected = NULL) {
  ns <- NS(id)

  require(caret)

  l <- list(
    selectizeInput(ns("model"), "Model", models(), selected),
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
#' @export
s.caretModel <- function(input, output, session) {

  model.params <- reactive({
    req(input$model)
    getModelInfo(input$model, regex = F)[[input$model]]$parameter
  })

  model.type <- reactive({
    req(input$model)
    getModelInfo(input$model, regex = F)[[input$model]]$type
  })

  output$typeOut <- renderUI(shiny::radioButtons(session$ns("type"), NULL, model.type()))
  output$paramsOut <- renderUI(ui.listOfVectors(session$ns("params"), model.params))

  return(reactive(list(
    model = input$model,
    type = input$type,
    params = callModule(s.listOfVectors, "params", reactive(model.params()$parameter))()
  )))
}
