# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)


#' shinypipe UI for creating a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param id namespace id (string)
#' @param table.params reactive data frame of parameters with three columns:
#' parameter, class and label similar to that provided by the parameter item in the
#' object returned by caret::getModelInfo
#' @export
ui.caretModel <- function(id, table.params) {

  ns <- NS(id)

  l <- lapply(1:nrow(table.params()), function (i) {
    rw <- table.params()[i,]
    ui.vector(ns(rw$parameter), rw$class, rw$label)
  })

  tagList(l)
}

#' shinypipe server function for returning a data frame that can potentially be used
#' as a tuning grid for caret::train
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param parameters reactive vector of parameters (usually the parameter column sent to table.params in ui.caretModel)
#' @return a data frame that can potentially be used as a tuning grid for caret::train
#' @export
s.caretModel <- function(input, output, session, parameters) {
  reactive({
    r <- lapply(parameters(), function(p) callModule(s.vector, p)())
    names(r) <- parameters()
    r
  })
}
