# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param id namespace id (string)
#' @param table.params reactive data frame of parameters with the following columns:
#' parameter - name of the parameter
#' class - One of "numeric", "character" or "logical" [Default: "numeric"]
#' label - [Default: same as parameter]
#' value - Expression value to be populated [Default: ""]
#' @param showValues show the evaluated vector next to each row
#' @export
ui.listOfVectors <- function(id, table.params, showValues = F) {
  ns <- NS(id)

  l <- lapply(1:nrow(table.params()), function (i) {
    rw <- table.params()[i,]

    parameter <- rw$parameter
    class <- rw$class
    label <- rw$label
    value <- rw$value

    if (is.null(class)) class <- "numeric"
    if (is.null(label)) label <- parameter
    if (is.null(value)) value <- ""

    ui.vector(ns(parameter), class, label, value, showValues = showValues)
  })

  tagList(l)
}

#' shinypipe server function for returning a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param parameters reactive vector of parameters (usually the parameter column sent to table.params in ui.listOfVectors)
#' @return a data frame that can potentially be used as a tuning grid for caret::train by calling expand.grid
#' @export
s.listOfVectors <- function(input, output, session, parameters) {
  reactive({
    r <- lapply(parameters(), function(p) callModule(s.vector, p)())
    names(r) <- parameters()
    r
  })
}
