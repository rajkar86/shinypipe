# December 2017
# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)

#' shinypipe UI for getting creating an interface for obtaining a
#' list of parameters from the user
#'
#' @param id namespace id (string)
#' @param ... One argument for each parameter for which an interface is required
#' Each argument must be named.
#' The widget value will be returned under this name by s.params.
#' The default label for the widget will also use this name (unless overridden)
#' Each argument must itself be a list. This inner list must be in one of the following formats:
#'
#' Option 1 - Numeric
#'
#' position 1: (Required) Default value(s)
#' A scalar, or a vector of two values (for a range input)
#'
#' position 2: (Required only if additional named arguments are necessary in positions 3+)
#' vector of (min, max, step), if applicable.
#' Set elements of the vector to NA to skip specifying one or more of min, max and step.
#' If one or more element are NA, the vector is of size less than three,
#' the remaining parameters will be assumed to be unspecified.
#' Use NA instead of a vector to skip specifying all of min, max and step
#'
#' positions 3+: (Optional) list of additional named arguments to be sent to the widget
#' widget will be shiny::sliderInput if both min and max (other than inputId, min, max, step)
#' are specified in position 2, or shiny::numericInput otherwise.
#'
#'
#' Option 2 - String
#'
#' position 1: (Required) Default value(s)
#' For numeric types - either a scalar or a vector of two values (for a range input)
#' For string types - the default string
#' For custom widgets - the function corresponding to the input widget
#'
#' position 2: (Required only if additional named arguments are necessary in positions 3+)
#' list of allowed string values. Use NA if there's no restriction.
#'
#' positions 3+: (Optional) list of additional named arguments to be sent to the widget
#' widget will be shiny::selectizeInput if a list of allowed string is specified,
#' or shiny::textInput otherwise.
#'
#'
#' Option 3 - Custom widgets
#'
#' position 1: (Required) Default value(s)
#' For numeric types - either a scalar or a vector of two values (for a range input)
#' For string types - the default string
#' For custom widgets - the function corresponding to the input widget
#'
#'
#' positions 2+: (Optional) list of additional named arguments to be sent to the widget
#' default for label is the name of the list

#' @export

ui.params <- function(id, ...) {
  ns <- NS(id)

  # Handle numeric inputs
  num <- function(id, p) {

    opt <- list(value = p[[1]])

    if (length(opt$value) > 2)
      stop("Error in ui.params. Numeric value can either be a scalar or a vector of length 2. See ??ui.params for syntax")

    rng <- c(NA,NA,NA)
    # if(length(p) > 1 && !is.na(p[[2]])) { rng <- p[[2]]; length(rng) <- 3 }
    if (!is.na(rng[1])) opt$min  <- rng[1]
    if (!is.na(rng[2])) opt$max  <- rng[2]
    if (!is.na(rng[3])) opt$step <- rng[3]

    if (length(p) > 2) {
      opt <- c(opt, p[-c(1:2)])
    }

    rangeKnown <- !is.null(opt$min) && !is.null(opt$max)

    if (!rangeKnown && length(opt$value) > 1)
      stop("Error in ui.params. min and max need to be specified
           in second position for range inputs. If you need unbounded arguments,
           use separate parameters. See ??ui.params for syntax")

    if (is.null(opt$label)) opt$label <- id

    w <- ifelse(rangeKnown, shiny::sliderInput, shiny::numericInput)

    return (do.call(w, c(ns(id), opt)))
  }

  # Handle string inputs
  str <- function(id, p) {

    if (length(p[[1]]) > 1)
      stop("Error in ui.params. String value can only be a scalar. See ??ui.params for syntax")

    choicesKnown <- (length(p) > 1 && (length(p[[2]]) > 1 || !is.na(p[[2]])))

    if (choicesKnown) {
      opt <- list(selected = p[[1]], choices = p[[2]])
    } else {
      opt <- list(value = p[[1]])
    }

    if (length(p) > 2) {
      opt <- c(opt, p[-c(1:2)])
    }

    if (is.null(opt$label)) opt$label <- id

    w <- ifelse(choicesKnown, shiny::selectizeInput, shiny::textInput)

    return (do.call(w, c(ns(id), opt)))
  }

  custom <- function(id, p) {

    if (length(p[[1]]) > 1)
      stop("Error in ui.params. Specify a unique widget function. See ??ui.params for syntax")

    w <- p[[1]]
    opt <- c(p[-c(1)])
    if (is.null(opt$label)) opt$label <- id
    return (do.call(w, c(ns(id), opt)))
  }

  params <- list(...)
  l <- lapply(1:length(params),
              function(i){
                id <- names(params)[i]
                p <- params[[i]]
                if (length(p) == 0) stop("Error in ui.params. Each parameter needs at least a value. See ??ui.params for syntax")

                type <- class(p[[1]])
                if (type == "NULL" || type == "logical")
                  type <- class(p[[2]])

                return(switch(type,
                              "numeric" = num(id, p),
                              "character" = str(id, p),
                              "function" = custom(id, p)
                              ))})
  tagList(l)
}

#' shinypipe server function that returns the params
#' as defined in ui.params
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @export
s.params <- function(input, output, session) {
  return(reactive(reactiveValuesToList(input)))
}
