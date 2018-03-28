# December 2018
# Author: Karthik Rajendran (karthikeyan.rajendran@gmail.com)

#' Helper function to create DT::styleInterval for a given data, in which
#' Positive numbers are mapped to shades of green, and 
#' Negative numbers are mapped to shades of red
#' @param data a vector of possible values
#' @param num.breaks number of color breaks
#' @export
createStyleInterval <- function(data, num.breaks = 20) {
  
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