################################################################################
## PCA Stuff from affycoretools
## I'm changing this so that the user can define a different set of sizes for
## the points on a 3dplot.

##' Performs and plots PCA stuff.
##'
##' This function was taken/modified from affycoretools. Added:
##'
##' @param var.sizes Only considered when \code{plot3d=TRUE}. If \code{TRUE},
##' the spheres in 3d space change in size from large to small (at midpoint)
##' then large. If this is a numeric vector of length 1, it is considered
##' to be the "midpoint" at which point the spheres would start growing larger
##' from there. If its a numeric vector as long as there are experiments, it
##' is considered to predetermined sizes of each sphere.
plotPCA <- function(object, groups=NULL, groupnames=NULL, addtext=NULL,
                    x.coord=NULL, y.coord=NULL, screeplot=FALSE,
                    squarepca=FALSE, pch=NULL, col=NULL, pcs=c(1,2),
                    legend=TRUE, main="Principal Components Plot",
                    plot3d=FALSE, center=TRUE, scale.=FALSE, var.sizes=FALSE,
                    ...) {
  if (length(pcs) != 2 && !plot3d) {
    stop("You can only plot two principal components.\n", call. = FALSE)
  }
  if (length(pcs) != 3 && plot3d) {
    stop("For 3D plotting, you should specify 3 principal components.\n",
         call. = FALSE)
  }

  if (is(object, "ExpressionSet")) {
    if (max(pcs) > dim(exprs(object))[2]) { 
      stop(paste("There are only", dim(exprs(object))[2],
                 "principal components to plot.\n"),
           call. = FALSE)
    }
    if (is.null(groupnames)) groupnames <- sampleNames(object)
    if (is.factor(groupnames)) groupnames <- as.character(groupnames)
    pca <- prcomp(t(exprs(object)), center=center, scale.=scale.)
    len <- length(sampleNames(object))
  } else {
    if (class(object) == "matrix") {
      if (max(pcs) > dim(object)[2]) {
        stop(paste("There are only", dim(object)[2],
                   "principal components to plot.\n"),
             call. = FALSE)
      }
      if (is.null(groupnames)) groupnames <- colnames(object)
      if (is.factor(groupnames)) groupnames <- as.character(groupnames)
      pca <- prcomp(t(object), center=center, scale.=scale.)
      len <- dim(object)[2]
    } else {
      if (class(object) == "prcomp") {
        if (max(pcs) > dim(object$x)[2]) {
          stop(paste("There are only", dim(object$x)[2],
                     "principal components to plot.\n"),
               call. = FALSE)
        }
        if(is.null(groupnames)) groupnames <- row.names(object$x)
        if(is.factor(groupnames)) groupnames <- as.character(groupnames)
        pca <- object
        len <- dim(object$x)[2]
      } else {
        stop("plotPCA currently only supports ExpressionSets, matrices ",
             "and prcomp objects")
      }
    }
  }

  plotstuff <- NULL
  if (screeplot) {
    plot(pca, main = "Screeplot")
  } else {
    if (plot3d) {
      if (!require("rgl", quiet=TRUE)) {
        stop("The rgl package must be installed to do 3D plots.\n",
             call.=FALSE)
      }
      plotstuff <- pcaPCH(len, groups, pch, col)
      ##########################################################################
      ## plot3d(pca$x[,pcs], type = "s", col = plotstuff$col, size = 2)
      if (!is.null(var.sizes)) {
        point.size <- pca.point.size(var.sizes, len)
      }
      if (!is.null(col)) {
        cols <- col
      } else {
        cols <- plotstuff$col
      }
      if (length(cols) != len) {
        warning("Color vector doesn't add up.")
        cols <- plotstuff$col
      }
      
      plot3d(pca$x[,pcs], type="s", col=cols, size=point.size)
      cat(paste("Sometimes rgl doesn't plot the first time.\nIf there",
                "isn't anything in the plotting window, close it and",
                "re-run plotPCA().\n"))
      if (!is.null(addtext)) {
        text3d(pca$x[,pcs], texts=addtext)
      }
    } else {
      if (squarepca) {
        ylim <- max(abs(range(pca$x[,pcs[1]])))
        ylim <- c(-ylim, ylim)
      } else {
        ylim <- NULL
      }
      plotstuff <- pcaPCH(len, groups, pch, col)
      plot(pca$x[,pcs], pch=plotstuff$pch, col=plotstuff$col, bg=plotstuff$col,
           ylab= paste("PC", pcs[2], sep=""), xlab=paste("PC", pcs[1], sep=""),
           main = main, ylim = ylim, ...)

      if (!is.null(addtext)) {
        smidge <-  (par("usr")[4] - par("usr")[3])/50
        text(pca$x[,pcs[1]], pca$x[,pcs[2]] + smidge, label=addtext, cex=0.7)
      }
      
      if (legend) {
        pca.legend(pca, groups, groupnames, plotstuff, x.coord = x.coord,
                   y.coord = y.coord, ...)
      }
    }
  }
  
  invisible(list(pch=plotstuff, pca=pca))
}

pca.point.size <- function(var.sizes, len, max.size=4, min.size=1) {
  mid.point <- floor(len / 2)
  if (is.logical(var.sizes) && !var.sizes) {
    return(rep(2, len))
  }
  if (is.numeric(var.sizes)) {
    if (length(var.sizes) == len) {
      return(var.sizes)
    }
    if (length(var.sizes) == 1) {
      if (var.sizes < 1 || var.sizes > len) {
        stop("Midpoint specification out of bounds")
      }
      mid.point <- var.sizes
    }
  }

  sizes <- c(seq(max.size, min.size, length=mid.point),
             seq(min.size, max.size, length=len - mid.point))
  sizes
}

pca.legend <- function(pca, groups, groupnames, pch.df,  x.coord=NULL,
                       y.coord=NULL, saveup=FALSE) {
  ## A function to try to automagically place legend in a pca plot
  if(is.null(groups)) {
    unq <- unique(pch.df)
  } else {
    unq <- unique(pch.df[order(groups),])
  }
  pch <- unq[,1]
  col <- unq[,2]
  x.lab <- legend(1, 1, legend = groupnames, pch = pch, plot = FALSE)$rect$w
  y.lab <- legend(1, 1, legend = groupnames, pch = pch, plot = FALSE)$rect$h

  right <- par("usr")[2] - (par("usr")[2] - par("usr")[1])/100 - x.lab
  left <- par("usr")[1] + (par("usr")[2] - par("usr")[1])/100 + x.lab
  up <- par("usr")[4] - (par("usr")[4] - par("usr")[3])/100 - y.lab
  down <- par("usr")[3] + (par("usr")[4] - par("usr")[3])/100 + y.lab
  
  upright <- !any(pca$x[,1] > right & pca$x[,2] > up)
  upleft <- !any(pca$x[,1] < left & pca$x[,2] > up)
  downright <- !any(pca$x[,1] > right & pca$x[,2] < down)
  downleft <- !any(pca$x[,1] < left & pca$x[,2] < down)

  whereto <- match(TRUE, c(upright, upleft, downleft, downright))
  if(!is.null(x.coord) & !is.null(y.coord)){
    legend(x.coord, y.coord, legend=groupnames, pch=pch, col=col, pt.bg=col)
  } else if (!is.na(whereto)) {
    if (whereto == 1) {
      legend(right, up + y.lab, legend=groupnames, pch=pch, col = col, pt.bg = col)
    }
    if (whereto == 2) {
      legend(left - x.lab, up + y.lab, legend=groupnames, pch=pch, col=col,
             pt.bg=col)
    }
    if (whereto == 3) {
      legend(left - x.lab, down, legend=groupnames, pch=pch, col=col, pt.bg=col)
    }
    if(whereto == 4) {
      legend(right, down, legend=groupnames, pch=pch, col=col, pt.bg=col)
    }
  } else {
    answer <- readline("Please give the x-coordinate for a legend.")
    x.c <- as.numeric(answer)
    answer <- readline("Please give the y-coordinate for a legend.")
    y.c <- as.numeric(answer)
    legend(x.c, y.c, legend=groupnames, pch=pch, col = col, pt.bg = col)
  }
  if (saveup) {
    return((par("usr")[4] - par("usr")[3])/50)
  }
}

pcaPCH <- function(len, groups, pch, col) {
  ## Function to minimize glyphs/colors used
  ## nulls <- is.null(c(pch, col)) || all(!is.null(pch), !is.null(col))
  ## if (nulls) {
  if (TRUE) {
    if (!is.null(pch)) {
      if (is.null(groups)) {
        out <- data.frame(pch, col, stringsAsFactors = FALSE)
      } else {
        out <- data.frame(pch[groups], col[groups], stringsAsFactors = FALSE)
      }
    } else {
      if (is.null(groups)) {
        pch <- rep(21:25, times = ceiling(len/5))[1:len]
        col <- colvec(len)[1:len]
        out <- data.frame(pch, col, stringsAsFactors = FALSE)
      } else {
        lg <- length(unique(groups))
        pch <- rep(21:25, times = ceiling(lg/5))[1:lg]
        col <- colvec(lg)[1:lg]
        out <- data.frame(pch, col, stringsAsFactors = FALSE)[groups,]
      }
    }
  } else {
    stop("You must either specify both pch and col arguments or neither\n",
         call. = FALSE)
  }
  out$groups <- groups
  out
}

colvec <- function(len){
  if (len > 64) {
    tmp <- ceiling(sqrt(len))
  } else {
    tmp <- len
  }
  mat <- matrix(1:tmp, ncol = tmp, nrow = tmp)
  tfs <- rbind(lower.tri(mat, TRUE), upper.tri(mat))
  mat <- rbind(mat,mat)
  out <- rainbow(tmp)[mat[tfs]]
  out
}

################################################################################
## END: affycoretools-modified code
################################################################################

screeProportionOfVariance.prcomp <- function(pca, barplot=TRUE,
                                             main="Screeplot", ...) {
  vars <- pca$sdev^2
  df <- data.frame(sdev=pca$sdev, var=vars, prop.var=vars/sum(vars))
  df$cum.prop <- cumsum(df$prop.var)
  rownames(df) <- colnames(pca$rotation)

  if (barplot) {
    barplot(df$prop.var, names.arg=rownames(df), main=main,
            ylab="Proportion of Variance", ...)
  }
  
  invisible(df)
}
