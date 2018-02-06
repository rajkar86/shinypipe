# January 2018
# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a plot with zoom and pan functions
#' @param id namespace id (string)
#' @export
ui.plot <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  l <- list(
    fixedRow(
      column(2,tags$div(title = paste("Zoom - zoom on selection (Double-click outside the selected region to reset)",
                                      sep = "\n"),
                        checkboxGroupInput(ns("toggles"), NULL, inline = T,
                                  choiceNames = list("Zoom/Pan"),
                                  choiceValues = list("zoom"),
                                  selected = NULL))), # TODO expose this?
      column(10,span(textOutput(ns("message"), inline = T), style="color:green"))
    ),
    plotOutput(ns("plot"),
               brush    = brushOpts(ns("brush"), clip = F),
               click    = ns("click"),
               hover    = ns("hover"),
               dblclick = ns("dblclick")))

  tagList(l)
}

#' shinypipe server function for creating a plot with zoom and pan functions
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param plot 'reactive' plot object that will be appended to a ggplot object
#' @param data 'reactive' data.table to be passed to ggplot2::ggplot (cannot be NULL)
#' @param mapping 'reactive' mapping passed to ggplot2::ggplot [Default: reactive(aes())]
#' @param selected.colname col name for the additional column that indicates whether the
#' row is selected (within the brushed region) or not [Default: "selected"]
#' @return The original data with an additional column with the name given by selected.colname
#' which is true for points under a brush
#' @export
#' @import data.table
s.plot <- function(input, output, session, plot, data, mapping = reactive(aes()), selected.colname = "selected") {

  rangeExtend <- function(r, f) { r + c(-f,f) * diff(r) }

  val <- reactiveValues(zoomBrush=NULL)

  output$message <- renderText("")

  observeEvent(input$toggles, {
    if ("zoom" %in% input$toggles)
      output$message <- renderText("You can select a region to zoom, and drag it to pan.")
    else
      output$message <- renderText("Zoom/Pan mode is disabled.")
  }, ignoreNULL = F)

  observeEvent(input$dblclick, {
    val$zoomBrush <- NULL
    session$resetBrush(input$brush$brushId)
    output$message <- renderText("")
  })

  observeEvent(input$click, {
  })

  observeEvent(input$brush, {
    if (!is.null(input$brush) && ("zoom" %in% input$toggles)) {
      val$zoomBrush <- input$brush
    }
    if (!("zoom" %in% input$toggles))
      output$message <- renderText("Click outside the region to deselect.")
  })

  output$plot <- renderPlot({
    p <- ggplot2::ggplot(data = data(), mapping()) + plot()

    brush <- val$zoomBrush
    if (!is.null(brush)) {
      p <- p + coord_cartesian(xlim = rangeExtend(c(brush$xmin, brush$xmax), 0.1),
                               ylim = rangeExtend(c(brush$ymin, brush$ymax),0.1), expand = FALSE)
      output$message <- renderText("Drag region to pan. Double click to reset.")
    }
    p
  })

  brushedData <- reactive({
    dt <- data.table::copy(data())
    dt <- dt[, selected_ := F]
    if(!is.null(input$brush))
      dt <- brushedPoints(data(), input$brush, allRows = T)
    setnames(dt, "selected_", selected.colname)
    print(dt)
    as.data.table(dt)
  })

  return(brushedData)
}
