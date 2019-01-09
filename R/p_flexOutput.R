# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a flexible output UI
#' @param id namespace id (string)
#' @param plotHeight plot height when plot type is chosen [Default: 400]
#' @export
ui.flexOutput <- function(id, editable = T, plotHeight = 400) {
  ns <- NS(id)

  require(shiny)

  checkType <- function (type) { paste0("input['", ns("type"), "'] === '", type, "'")}

  plotOutput <- function(...) {
    params <- list(...)
    div(style = paste0("height: ", toString(plotHeight), "px;"),
        do.call(shiny::plotOutput, params))
  }

  # is.editable <- ifelse(editable, "true", "false")
  # not.editable <- ifelse(!editable, "true", "false")
  
  tagList(
    fluidRow(column(12,uiOutput(ns("outType")))),
    # conditionalPanel(is.editable, uiOutput(ns("outTypeEditable"))),
    # conditionalPanel("false", uiOutput(ns("outTypeFixed"))),
    # conditionalPanel(is.editable, uiOutput(ns("outExpr"))),
    fluidRow(column(12,uiOutput(ns("outExpr")))),
    conditionalPanel(checkType("Print"), fluidPage(verbatimTextOutput(((ns("outPrint")))))),
    conditionalPanel(checkType("Table"), dataTableOutput((ns("outTable")))),
    conditionalPanel(checkType("Plot"),  plotOutput((ns("outPlot"))))
  )
}

#' shinypipe server function for ui.flexOutput
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param envir The environment in which the expr needs to be evaluated
#' @param expr The expression to be evaluated as a string, or its starting value if editable
#' @param type The type of UI, or its starting value if editable (one of "Print", "Table", or "Plot")
#' @param editable whether the user can change the output type and output expression [Default: T]
#' @param rows number of rows of text area input; only valid if editable is T [Default: 1]
#' @return a reactive list of type and expr used
#' @export
s.flexOutput <- function(input, output, session, envir,
                         expr = "", type = "Print", 
                         editable=T, rows=1) {
  ns <- session$ns

  TYPES <- c("Print", "Table", "Plot")
  if (!(type %in% TYPES)) stop(paste("Invalid 'type' value: ", type))

  textAreaInput <- function(...) {
    params <- list(...)
    tags$div(tags$style(HTML(".shiny-input-container:not(.shiny-input-container-inline) { width: 100%;}")),
             do.call(shiny::textAreaInput, params))
  }
  
  choices <- TYPES
  if (!editable) choices <- type
  
  output$outType <- renderUI(radioButtons(ns("type"), NULL, choices, selected = type, inline = T))

  # output$outTypeFixed <- renderUI({
  #   fixedRow(
  #     column(1, radioButtons(ns("typeFixed"), NULL, type, selected = type, inline = T)),
  #     column(11, textOutput(ns("label"), inline = T))
  #   )
  # })

  if (editable)
    output$outExpr <- renderUI(textAreaInput(ns("expr"), NULL, value = expr, rows = rows))
  else
   output$outExpr <- renderText(expr)

  observe(tryCatch({

    value <- ifelse(is.null(input$expr), expr, input$expr)
    req(value)
    exp <- parse(text = value)

    type <- ifelse(is.null(input$type), type, input$type)
    req(type)
    
    switch (type,
            "Print" = {output$outPrint <- renderPrint(eval(exp, envir=envir))},
            "Table" = {output$outTable <- renderDataTable(eval(exp, envir=envir))},
            "Plot" =  {output$outPlot  <- renderPlot(eval(exp, envir=envir))}
    )
  },
  error = function(err) { return(reactive(err)) },
  warning = function(warn) { return(reactive(warn)) }
  ))

  return (reactive(list(type = input$type, expr = ifelse(is.null(input$expr), expr, input$expr))))
}
