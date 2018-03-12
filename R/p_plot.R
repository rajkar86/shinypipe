# January 2018
# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a plot with zoom and pan functions
#' @param id namespace id (string)
#' @param height height of the widget
#' @param brush list of arguments to be sent to brushOpts (except for id and resetOnNet)
#' @param zoom Default behavior for the brush (set it to NULL to disallow the user to toggle the zoom feature on)
#' @export
ui.plot <- function(id, height = 400, brush = list(clip = F), zoom = F) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  if (is.null(zoom))
    header <- fixedRow(column(12,span(textOutput(ns("message"), inline = T), style="color:green")))
  else
    header <- fixedRow(
      column(2,tags$div(title = paste("Zoom - zoom on selection (Double-click outside the selected region to reset)",
                                      sep = "\n"),
                        checkboxInput(ns("zoom"), "Zoom", value = zoom))),
      column(10,span(textOutput(ns("message"), inline = T), style="color:green"))
    )



  pObj <- plotOutput(ns("plot"),
                     brush    = do.call("brushOpts",
                                        c(list(id=ns("plot_brush"),
                                               resetOnNew = T),
                                          brush)),
                     click    = ns("click"),
                     hover    = ns("hover"),
                     dblclick = ns("dblclick"))


  l <- list(fillCol(flex = c(NA, 1), header, pObj))
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
#' row is selected (within the brushed region) or not.
#' If NULL is provided, only selected columns will be returned [Default: NULL]
#' @return The original data with an additional column with the name given by selected.colname
#' which is true for points under a brush
#' @export
#' @import data.table
s.plot <- function(input, output, session, plot, data,
                   mapping = reactive(aes()),
                   selected.colname = NULL) {

  val <- reactiveValues(zoomBrush=NULL, click.x=NULL, click.y = NULL)

  output$message <- renderText("Click to see coordinates")

  observeEvent(input$zoom, {
    if (!is.null(input$zoom) && input$zoom)
      output$message <- renderText("Select a region to zoom.")
    else
      output$message <- renderText("Zoom mode is disabled.")
  }, ignoreNULL = T)

  observeEvent(input$dblclick, {
    val$zoomBrush <- NULL
    # session$resetBrush(input$plot_brush$brushId)
    output$message <- renderText("Click to see coordinates")
  })

  observeEvent(input$click, {
    if (!is.null(input$click)) {
      val$click.x <- convertLimitForType(input$click$x, class(data()[,get(toString(input$click$mapping$x))]))
      val$click.y <- convertLimitForType(input$click$y, class(data()[,get(toString(input$click$mapping$y))]))
    }
    if (is.null(input$plot_brush))
      output$message <- renderText(paste0("Last click: ( ",
                                          val$click.x, ", ",
                                          val$click.y, " )"))
  })

  observeEvent(input$plot_brush, {
    if (!is.null(input$plot_brush) && !is.null(input$zoom) && input$zoom) {
      val$zoomBrush <- input$plot_brush
      # session$resetBrush(input$plot_brush$brushId)
    }
    if (is.null(input$zoom) || !input$zoom)
      output$message <- renderText("Click outside the region to deselect.")
  })

  observeEvent(plot(),{
    val$zoomBrush <- NULL # resetOnNew for the cached zoomBrush
  })

  observeEvent(data(),{
    val$zoomBrush <- NULL # resetOnNew for the cached zoomBrush
  })

  convertLimitForType <- function(lim, datatype) {
    # origin used here may be exposed, if necessary
    switch (datatype[[1]],
      "Date" = as.Date(lim, origin="1970-01-01"),
      "POSIXct" = as.POSIXct(lim, origin = "1970-01-01"),
      lim
    )
  }

  output$plot <- renderPlot({
    p <- ggplot2::ggplot(data = data(), mapping()) + theme_light(base_size = 16) + plot()

    brush <- val$zoomBrush
    if (!is.null(brush)) {
      xlim <- c(brush$xmin, brush$xmax)
      ylim <- c(brush$ymin, brush$ymax)
      xlim <- convertLimitForType(xlim, class(data()[,get(toString(brush$mapping$x))]))
      ylim <- convertLimitForType(ylim, class(data()[,get(toString(brush$mapping$y))]))
      p <- p + coord_cartesian(xlim, ylim, expand = F)
      output$message <- renderText("Double click to reset.")
    }
    p
  })

  brushedData <- reactive({
    allRows <- !is.null(selected.colname)
    if(is.null(input$plot_brush) & allRows) {
      dt <- copy(data())
      dt[, eval(selected.colname) := F]
    } else {
      dt <- brushedPoints(data(), input$plot_brush, allRows = allRows)
      if (allRows)
        setnames(dt, "selected_", selected.colname)
    }
    as.data.table(dt)
  })

  return(brushedData)
}
