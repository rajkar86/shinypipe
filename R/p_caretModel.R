# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param id namespace id (string)
#' @param models reactive list of models
#' @param selected model to be selected by default []
#' @param show.type boolean to indicate whether type can be selected by the user
#' @export
ui.caretModel <- function(id, models, selected = NULL, show.type = T) {
  ns <- NS(id)

  require(caret)

  showType <- ifelse(show.type, "true", "false")

  l <- list(
    selectizeInput(ns("method"), "Method", models(), selected),
    conditionalPanel(showType,uiOutput(ns("typeOut"))),
    uiOutput(ns("paramsOut"))
  )

  tagList(l)
}

#' shinypipe server function for returning a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param return reactive list
#' @export
s.caretModel <- function(input, output, session) {

  require(shiny)
  rv <- reactiveValues(method=NULL,
                       type=NULL,
                       expr=NULL,
                       params=NULL)

  model.info <- reactive({
    req(input$method)
    getModelInfo(input$method, regex = F)[[input$method]]
  })

  model.params <- reactive(model.info()$parameter)
  model.type   <- reactive(model.info()$type)

  output$typeOut <- renderUI(shiny::radioButtons(session$ns("type"), NULL, model.type(), inline = T))
  output$paramsOut <- renderUI(ui.listOfVectors(session$ns("params"), model.params))

  r.params <- callModule(s.listOfVectors, "params", reactive(model.params()$parameter))

  observeEvent(input$method, rv$method <- input$method)
  observeEvent(input$type, rv$type <- input$type)

  observeEvent(r.params(),{
    rv$expr   <- r.params()$expr
    rv$params <- r.params()$value
  })

  return (reactive(reactiveValuesToList(rv)))
}
