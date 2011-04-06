## Taken and adapted from:
## http://rforcancer.drupalgardens.com/content/ggheat-ggplot2-style-heatmap-function

##' A ggplot-ized implementation of a heatmap.
##'
##' TODO: Enable \code{heatscale} to use any type of color ramp, ie. define a 
##'       3-color map
##' 
##' @param m The data matrix/data.frame
##' @param rescaling A way to resacle the data. Either pass a function to work
##' over \code{m}, or \code{'none', 'row', 'column'}
##' @param clustering A way to cluster the data. Either a function, or
##' \code{'none', 'row', 'column', 'both'}
##' @param labCol Label the columns?
##' @param labRow Label the rows?
##' @param border \code{logical(1)} use a border?
##' @param heatscale Define the colorramp to use
##' 
##' @usage
##' m <- matrix(data=sample(rnorm(100,mean=0,sd=2)), ncol=10)
##' ggheat(m)
##'
##' ## NB because ggheat returns an ordinary ggplot you can add ggplot tweaks
##' data(mtcars)
##' x= as.matrix(mtcars)
##' ggheat(x)+ opts(panel.background=theme_rect(fill='pink'))

ggheat <- function(m, rescaling=c('none', 'row', 'column'),
                   clustering=c('none', 'row', 'column', 'both'),
                   labCol=TRUE, labRow=TRUE, border=FALSE,
                   heatscale=c(low='blue', high='red')) {
  ## the function can be be viewed as a two step process
  ## 1. using the rehape package and other funcs the data is clustered, scaled, and reshaped
  ## using simple options or by a user supplied function
  ## 2. with the now resahped data the plot, the chosen labels and plot style are built
  require(reshape)
  require(ggplot2)
  
  ## you can either scale by row or column not both! 
  ## if you wish to scale by both or use a differen scale method then simply supply a scale
  ## function instead NB scale is a base funct
  if(is.function(rescaling)) { 
    m <- rescaling(m)
  } else {
    rescaling <- match.arg(rescaling)
    if (rescaling == 'column') {
      m <- scale(m, center=TRUE)
    }
    if (rescaling == 'row') {
      m <- t(scale(t(m), center=TRUE))
    }
  }
 
  ## I have supplied the default cluster and euclidean distance- and chose to cluster after scaling
  ## if you want a different distance/cluster method-- or to cluster and then scale
  ## then you can supply a custom function 
  if (is.function(clustering)) {
    m <- clustering(m)
  } else {
    clustering <- match.arg(clustering)
    m <- switch(clustering, row=m[hclust(dist(m))$order, ],
                column=m[, hclust(dist(t(m)))$order],
                both=m[hclust(dist(m))$order, hclust(dist(t(m)))$order])
  }
  
  ## this is just reshaping into a ggplot format matrix and making a ggplot layer
  numrows <- dim(m)[1]
  numcols <- dim(m)[2]
  melt.m <- cbind(rowInd=rep(1:numrows, times=numcols),
                  colInd=rep(1:numcols, each=numrows), melt(m))
  g <- ggplot(data=melt.m)
  
  ## add the heat tiles with or without a white border for clarity
  if (border) {
    g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, fill=value),
                        colour='white')
  } else {
    g2 <- g + geom_rect(aes(xmin=colInd-1,xmax=colInd,ymin=rowInd-1,ymax=rowInd, fill=value))
  }
 
  ## add axis labels either supplied or from the colnames rownames of the matrix
  if (length(labCol) == numcols) {
    g2 <- g2 + scale_x_continuous(breaks=(1:numcols)-0.5, labels=labCol)
  } else {
    if (labCol) { 
      g2 <- g2 + scale_x_continuous(breaks=(1:numcols)-0.5, labels=colnames(m))
    } else {
      g2 <- g2 + scale_x_continuous(breaks=(1:numcols)-0.5, labels=rep('',numcols))
    }
  }
 
  if (length(labRow) == numrows) {
    g2 <- g2 + scale_y_continuous(breaks=(1:numrows)-0.5, labels=labRow)
  } else {
    if (labRow) {
      g2 <- g2 + scale_y_continuous(breaks=(1:numrows)-0.5, labels=rownames(m))	
    } else {
      g2 <- g2 + scale_y_continuous(breaks=(1:numrows)-0.5, labels=rep('',numrows))	
    }
  }
  
  ## get rid of grey panel background and gridlines
  g2 <- g2 + opts(panel.grid.minor=theme_line(colour=NA),
                  panel.grid.major=theme_line(colour=NA),
                  panel.background=theme_rect(fill=NA, colour=NA))
  
  ## finally add the fill colour ramp of your choice (default is blue to red)-- and return
  g2 + scale_fill_continuous("", heatscale[1], heatscale[2])
}
