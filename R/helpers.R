# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)

#' Helper function to create DT::styleInterval for a given data, in which
#' Positive numbers are mapped to shades of green, and
#' Negative numbers are mapped to shades of red
#' @param data a vector of possible values
#' @param num.breaks number of color breaks
#' @export
signedColorStyleInterval <- function(data, num.breaks = 20) {

  require(DT)
  rng <- range(c(0,abs(data)))

  brks.p <- seq(rng[1], rng[2], length.out = num.breaks)
  brks.n <- rev(-brks.p)

  clrs.p <- round(seq(255, 40, length.out = num.breaks + 1), 0) %>% {paste0("rgb(", .,",255,", ., ")")}
  clrs.n <- round(seq(255, 40, length.out = num.breaks), 0)  %>% {paste0("rgb(255,", ., ",", ., ")")}
  clrs <- c(rev(clrs.n), clrs.p)

  brks <- c(brks.n, brks.p)

  return(DT::styleInterval(brks, clrs))
}

#' Helper function to create DT::styleInterval for a given data, in which
#' Numbers are mapped to shades of green
#' @param data a vector of possible values
#' @param num.breaks number of color breaks
#' @export
singleShadeColorInterval <- function(data, num.breaks = 20) {
  
  require(DT)
  rng <- range(data)
  
  brks <- seq(rng[1], rng[2], length.out = num.breaks)
  
  clrs <- round(seq(255, 40, length.out = num.breaks + 1), 0) %>% 
    {paste0("rgb(", .,",255,", ., ")")}
  
  return(DT::styleInterval(brks, clrs))
}


#' Take a data.table and adds a column indicating whether that row is an outlier
#' based on cook's distance with respect to a linear regression using the provided formula
#' @param dt a data.table obj
#' @param fml formula obj
#' @param thresh Cooks distance threshold for outlier; if the cooks distance is greater than thresh * mean(cooks distance)
#' then the data point will be treated as an outlier
#' @param outlier.colname Column name of the new boolean column that will indicate if the row is an outlier
#' @export
cooksdOutlier <- function (dt, fml, thresh = 4, outlier.colname = "outlier") {
  cooksd <- cooks.distance(lm(fml, dt))
  dt[, eval(outlier.colname) := (cooksd > mean(cooksd) * thresh)]
}


#' Take a data.table and return the columns with only a unique element as a separate table with
#' one row and the rest of the data.table separately
#' @param dt a data.table obj
#' @return a list of two data.tables with
#' @export
extractUniqueColsToList <- function(dt) {
  require(data.table)
  dt <- as.data.table(dt)
  uniq.cols <- sapply(names(dt), function(x) length(unique(dt[,get(x)])) == 1)
  l <- as.data.table(lapply(names(which(uniq.cols)),  function(x) (unique(dt[, x, with=F]))))
  dt <- unique(dt[,names(which(uniq.cols == F)), with=F])
  list (unique = l, rest = dt)
}

