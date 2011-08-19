## Inspired from http://sickel.net/blogg/?p=688

barplot.split <- function(height, width=1L, ylim.lower=NULL, ylim.upper=NULL,
                          y.units=NULL, space=NULL, names.arg=NULL,
                          legend.text=NULL, beside=FALSE, horiz=FALSE,
                          density=NULL, angle=45, col=NULL, border=par("fg"),
                          main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
                          xlim=NULL, ylim=NULL, xpd=TRUE, log="",
                          axes=TRUE, axisnames=TRUE, cex.axis=par("cex.axis"),
                          cex.names=par("cex.axis"), inside=TRUE, plot=TRUE,
                          axis.lty=0, offset=0, add=FALSE, args.legend=NULL,
                          ...) {
  if (is.null(ylim.lower) || is.null(ylim.upper)) {
    stop("Autodetecting lower/upper ylim's is not yet supported.")
  }
  if (is.null(y.units)) {
    stop("Autocalculating y.units not supported yet.")
  }
  if (is.vector(height)) {
    height <- matrix(heigh, nrow=1, dimnames=list(NULL, names(height)))
  }
  if (!is.matrix(height)) {
    stop("Only supporting matrix (columns = heights), rows = stacks")
  }
  if (is.null(col)) {
    col <- heat.colors(nrow(height))
  }
  
  lower.proportion <- .8
  .range <- range(ylim.lower, ylim.upper)
  lowspan <- c(0, ceiling(y.units * lower.proportion))
  topspan <- c(lowspan[2] + 1L, y.units)
  
  # on.exit(par(mar=par()$mar - c(0, 0, 0, 4))
  
  cnvrt.coords <- function(x, y=NULL) {
    xy <- xy.coords(x, y, recycle=TRUE)
    cusr <- par('usr')
    cplt <- par('plt')
    plt <- list()
    plt$x <- (xy$x-cusr[1])/(cusr[2]-cusr[1])
    plt$y <- (xy$y-cusr[3])/(cusr[4]-cusr[3])
    fig <- list()
    fig$x <- plt$x*(cplt[2]-cplt[1])+cplt[1]
    fig$y <- plt$y*(cplt[4]-cplt[3])+cplt[3]
    list(fig=fig)
  }
  
  subplot <- function(fun, x, y=NULL) {
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
    xy <- xy.coords(x,y)
    xy <- cnvrt.coords(xy)$fig
    par(plt=c(xy$x,xy$y), new=TRUE)
    fun
    tmp.par <- par(no.readonly=TRUE)
    invisible(tmp.par)
  }
  par(xpd=NA)
  opar <- par(mar=par()$mar + c(0,0,0,4))
  
  ## Setup wireframe for plots
  plot(c(0, 1), c(0, y.units), type='n', axes=FALSE, ylab=ylab, xlab='')
  
  ## Plot lower range, xpd=FALSE to clip the bars
  subplot(barplot(as.matrix(height), col=col, ylim=ylim.lower, xpd=FALSE,
                  las=1),
          x=c(0, 1), y=lowspan)
  
  ## Plot the upper range
  subplot(barplot(as.matrix(height), col=col, ylim=ylim.upper, xpd=FALSE,
                  names.arg=character(ncol(height)), las=1),
          x=c(0, 1), y=topspan)
  
  ## Legend
  if (is.null(legend.text)) {
    legend.text <- rownames(height)
  }
  if (!is.null(col)) {
    ## colors are in opposite order than in plot, care to fix?
  }
  # legend('topright', legend.text, fill=col)
  legend(1, max(y.units), legend.text, fill=col)
  
  ## Tidy up ends of axes. Remaining plots are in units of outer coordinate
  ## system
  lower.top <- lowspan[2] + 0.1 ## where to end the lower axis
  break.height <- 0.5           ## height of the break
  upper.bot <- lower.top + break.height  ## where to start the upper axes
  marker.height <- 0.4          ## height difference for the break markers
  marker.width <- 0.04          ## width of break markers
  
  ## Draw the markers
  lines(c(0, 0), c(1, lower.top))
  lines(c(marker.width / -2, marker.width / 2),
        c(lower.top - marker.height / 2, lower.top + marker.height / 2))
  lines(c(0, 0), c(upper.bot, ylim.upper[2]))
  lines(c(marker.width / -2, marker.width / 2),
        c(upper.bot - marker.height / 2, upper.bot + marker.height / 2))
  
}

