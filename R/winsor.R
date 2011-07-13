##' Another way to trim data (without triming)
##'
##' This function was taken from here:
##' Taken from: http://www.portfolioprobe.com/2011/06/30/winsorization
##' 
##' @seealso \code{links{trim.data}}
winsor <- function(x, multiple=3, na.rm=TRUE) {
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x, na.rm=na.rm)
  y <- x - med
  sc <- mad(y, center=0) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  y + med
}
