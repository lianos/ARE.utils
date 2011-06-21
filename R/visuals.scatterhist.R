##' Draw a scatter plot with density histograms on the top and
##' far-right axes. Taken from:
##' http://sas-and-r.blogspot.com/2011/06/example-841-scatterplot-with-marginal.html
scatterhist <- function(x, y, xlab="", ylab="") {
  zones <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist <- hist(x, plot=FALSE)
  yhist <- hist(y, plot=FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x, y)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
  invisible(NULL)
}
