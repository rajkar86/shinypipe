# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a flexible output UI
#' @param id namespace id (string)
#' @param editable whether the user can change the output type and output expression [Default: F]
#' @param plotHeight plot height when plot type is chosen [Default: 400]
#' @export
ui.flexOutput <- function(id, editable = T, plotHeight = 400) {
  ns <- NS(id)

  require(shiny)

  checkType <- function (type) { paste0("input['", ns("type"), "'] == '", type, "'") }

  plotOutput <- function(...) {
    params <- list(...)
    div(style = paste0("height: ", toString(plotHeight), "px;"),
        do.call(shiny::plotOutput, params))
  }

  is.editable <- ifelse(editable, "true", "false")
  not.editable <- ifelse(!editable, "true", "false")

  tagList(
    conditionalPanel(is.editable, uiOutput(ns("outTypeEditable"))),
    conditionalPanel(not.editable, uiOutput(ns("outTypeFixed"))),
    conditionalPanel(is.editable, uiOutput(ns("outExpr"))),
    conditionalPanel(checkType("Print"), fluidPage(verbatimTextOutput(((ns("outPrint")))))),
    conditionalPanel(checkType("Table"), dataTableOutput((ns("outTable")))),
    conditionalPanel(checkType("Plot"),  plotOutput((ns("outPlot"))))
  )
}

#' shinypipe server function for returning a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param expr The expression to be evaluated as a string, or its starting value if editable
#' @param type The type of UI, or its starting value if editable (one of "Print", "Table", or "Plot")
#' @return a reactive list of type and expr used
#' @export
s.flexOutput <- function(input, output, session, expr = "", type = "Print", envir = parent.frame()) {
  ns <- session$ns

  TYPES <- c("Print", "Table", "Plot")
  if (!(type %in% TYPES)) stop(paste("Invalid 'type' value: ", type))

  textAreaInput <- function(...) {
    params <- list(...)
    tags$div(tags$style(HTML(".shiny-input-container:not(.shiny-input-container-inline) { width: 100%;}")),
             do.call(shiny::textAreaInput, params))
  }

  output$outTypeEditable <- renderUI(radioButtons(ns("type"), NULL, TYPES, selected = type, inline = T))

  output$outTypeFixed <- renderUI({
    fixedRow(
      column(1, radioButtons(ns("type"), NULL, type, selected = type, inline = T)),
      column(11, textOutput(ns("label"), inline = T))
    )
  })

  output$label <- renderText(expr)

  output$outExpr <- renderUI(textAreaInput(ns("expr"), NULL, rows = 1, value = expr))

  observe(tryCatch({
    value <- ifelse(is.null(input$expr), expr, input$expr)
    req(value)
    exp <- parse(text = value)

    type <- ifelse(is.null(input$type), type, input$type)
    req(type)
    switch (type,
            "Print" = {output$outPrint <- renderPrint(eval(exp), envir=envir)},
            "Table" = {output$outTable <- renderDataTable(eval(exp), envir=envir)},
            "Plot" =  {output$outPlot  <- renderPlot(eval(exp), envir=envir)}
    )
  },
  error = function(err) { return(reactive(err)) },
  warning = function(warn) { return(reactive(warn)) }
  ))

  return (reactive(list(type = input$type, expr = ifelse(is.null(input$expr), expr, input$expr))))
}
