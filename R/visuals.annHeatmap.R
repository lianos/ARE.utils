## This code was taken from Heatplus (v2.0.0)
## if (!require(Heatplus)) {
##   stop("Heatplus required")
## }

###################################################
### chunk number 1:
###################################################
#line 45 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
## options(width=75)


###################################################
### chunk number 2: heatmapLayout_Def
###################################################
#line 55 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
heatmapLayout <- function(dendrogram, annotation, leg.side=NULL, show=FALSE)
{
    ## Start: maximum matrix, 5 x 5, all zero
    ## Names for nice display post ante
    ll = matrix(0, nrow=5, ncol=5)
    ll.width = ll.height = rep(0, 5)
    cnt = 1
    rownames(ll) = c("leg3", "colDendro","image", "colAnn", "leg1")
    colnames(ll) = c("leg2", "rowDendro","image", "rowAnn", "leg4")

    ## The main plot
    ll[3,3] = cnt
    ll.width[3] = ll.height[3] = 5
    cnt = cnt+1
    ## The column dendrogram
    if (dendrogram$Col$status=="yes") {
        ll[2, 3] = 2
        ll.width[3]  = 5
        ll.height[2] = 2
        cnt = cnt+1
    }
    ## The row dendrogram
    if (dendrogram$Row$status=="yes") {
        ll[3, 2] = cnt
        ll.width[2]  = 2
        ll.height[3] = 5
        cnt = cnt+1
    }
    # Column annotation
    if (!is.null(annotation$Col$data)) {
        ll[4, 3] = cnt
        ll.width[3] = 5
        ll.height[4] = 2
        cnt = cnt+1
    }
    ## Row annotation
    if (!is.null(annotation$Row$data)) {
        ll[3, 4] = cnt
        ll.width[4]  = 2
        ll.height[3] = 5
        cnt = cnt+1
    }
    ## Legend: if no pref specified, go for empty, if possible
    if (is.null(leg.side)) {
        if (dendrogram$Row$status != "yes") {
            leg.side = 2
        } else if (is.null(annotation$Row$data)) {
            leg.side = 4
        } else if (is.null(annotation$Col$data)) {
            leg.side = 1
        } else if (dendrogram$Col$status != "yes") {
            leg.side = 3
        } else {
            leg.side = 4
        }
    }
    ## Add the legend space
    if (leg.side==1) {
        ll[5,3] = cnt
        ll.width[3] = 5
        ll.height[5] = 1
    } else if (leg.side==2) {
        ll[3,1] = cnt
        ll.width[1] = 1
        ll.height[3] = 5
    } else if (leg.side==3) {
        ll[1,3] = cnt
        ll.width[3]  = 5
        ll.height[1] = 1
    } else if (leg.side==4) {
        ll[3,5] = cnt
        ll.width[5]  = 1
        ll.height[3] = 5
    }

    ## Compress
    ndx = rowSums(ll)!=0
    ll  = ll[ndx, , drop=FALSE]
    ll.height = ll.height[ndx]
    ndx = colSums(ll)!=0
    ll  = ll[, ndx, drop=FALSE]
    ll.width = ll.width[ndx]
    ## Do it - show it
    if (show) {
        layout(ll, width=ll.width, height=ll.height, respect=TRUE)
        layout.show(max(ll))
    }
    return(list(plot=ll, width=ll.width, height=ll.height, legend.side=leg.side))
}


###################################################
### chunk number 3: modifyExistingList_Def
###################################################
#line 154 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
modifyExistingList <- function(x, val)
{
    if (is.null(x)) x = list()
    if (is.null(val)) val = list()
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    vnames <- names(val)
    for (v in intersect(xnames, vnames)) {
      if (is.list(x[[v]]) && is.list(val[[v]])) {
          x[[v]] <- modifyExistingList(x[[v]], val[[v]])
        } else {
          x[[v]] <- val[[v]]
        }
    }
    x
}


###################################################
### chunk number 4: extractArg_Def
###################################################
#line 173 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
extractArg <- function(arglist, deflist)
{
    if (missing(arglist)) arglist = NULL
    al2 <- modifyExistingList(deflist, arglist)
    row <- col <- al2
    row <- modifyExistingList(row, arglist[["Row"]])
    col <- modifyExistingList(col, arglist[["Col"]])
    list(Row=row, Col=col)
}


###################################################
### chunk number 5: picketPlot_Def
###################################################
#line 199 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
picketPlot <- function (x, grp=NULL, grpcol, grplabel=NULL, horizontal=TRUE,
                        asIs=FALSE, control=list(), ...)
#
# Name: picketPlot (looks like a picket fence with holes, and sounds like the
#                   pocketplot in geostatistics)
# Desc: visualizes a pattern of 0/1/NAs by using bars, great for annotating a
#       heatmap
# Auth: Alexander.Ploner@meb.ki.se  181203
#
# Chng: 221203 AP loess() with degree < 2
#       260104 AP
#       - made loess() optional
#       - better use of space if no covariate
#       030304 AP
#       - added RainbowPastel() as default colors
#       - check grplabel before passing it to axis
#       2010-07-08 AP
#       - complete re-write
#       2010-08-28
#       - re-arranged code for vertical/horizontal drawing
{
    args <- list(...)
    # deal with the setup
    cc <- list(boxw=1, boxh=4, hbuff=0.1, vbuff=0.1, span=1/3, nacol=gray(0.85),
               degree=1, cex.label=1.5, numfac=2)
    cc[names(control)] <- control

    ## Convert/check the data
    x <- convAnnData(x, asIs=asIs)

    # Count variables, panels, types
    nsamp  <- nrow(x)
    npanel <- ncol(x)
    bpanel <- apply(x, 2, function(y) all(y[is.finite(y)] %in% c(0,1)) )

    # Compute panel heights, widths
    panelw = nsamp*(cc$boxw+2*cc$hbuff)
    panelh = cc$boxh+2*cc$vbuff
    totalh = sum(panelh * ifelse(bpanel, 1, cc$numfac))
    LL = cbind(0, 0)
    UR = cbind(panelw, totalh)

    # Set up the x-values for a single panel
    xbase = seq(cc$hbuff, by=cc$boxw + 2*cc$hbuff, length=nsamp)
    xcent = xbase + cc$boxw/2

    # if we get a cluster variable, we have to set differently colored
    # backgrounds; this assumes that the  grp variable is sorted in the
    # way it appears on the plot
    if (!is.null(grp)) {
        grp = as.integer(factor(grp, levels=unique(grp)))
        tt = table(grp)
        gg = length(tt)
        grpcoord = c(0,cumsum(tt/sum(tt))*panelw)
        grp0 = cbind(grpcoord[1:gg], rep(0, gg))
        grp1 = cbind(grpcoord[2:(gg+1)], rep(totalh, gg))
        if (missing(grpcol)) {
            grpcol=RainbowPastel
        }
        if (is.function(grpcol)) grpcol = grpcol(gg)
    }

    # Loop over vars and fill in the panels
    panels = list()
    voff = 0

    for (i in 1:npanel) {
        targs <- args[[colnames(x)]]
        do.loess <- is.list(targs) && isTRUE(targs$loess)
        if (bpanel[i]) {
            ## Coordinates
            x0 = xbase
            x1 = x0+cc$boxw
            y0 = voff + cc$vbuff
            y1 = y0   + cc$boxh
            ## Set fill
            fill = ifelse(x[, i, drop=FALSE]==1, "black", "transparent")
            fill[is.na(fill)] = cc$nacol
            label = colnames(x)[i]
            labcc = if (!is.null(label)) (y0+y1)/2 else NULL
            panels[[i]] <- list(ll=cbind(x0, y0), ur=cbind(x1, y1), fill=fill,
                                label=label, labcc=labcc)
            voff = voff + panelh
        } else {
            xv = x[,i]
            rr = range(xv, na.rm=TRUE)
            yval = voff + cc$vbuff*cc$numfac + ((xv - rr[1])/(rr[2] - rr[1]))*cc$boxh*cc$numfac
            if (do.loess && (cc$degree>0) && (cc$span>0)) {
                yy <- predict(loess(yval~xcent, span=cc$span, degree=cc$degree))
            } else {
              yy <- rep(NA, length(xcent))
            }
            label = colnames(x)[i]
            labcc = if (!is.null(label)) mean(range(yval, na.rm=TRUE)) else NULL
            axlab = pretty(range(xv, na.rm=TRUE))
            axcc <- voff + cc$vbuff*cc$numfac +
              ((axlab - rr[1])/(rr[2] - rr[1]))*cc$boxh*cc$numfac
            panels[[i]] <- list(raw=cbind(xcent, yval), smo=cbind(xcent, yy),
                                label=label, labcc=labcc, axlab=axlab, axcc=axcc)
            voff = voff + panelh*cc$numfac
        }
    }

    # if grplabels are given, we add another horizontal axis to the
    # last plot (independent of whether it is binvar or contvar)
    if (!is.null(grp) & !is.null(grplabel)) {
        mids = (grpcoord[1:gg] + grpcoord[2:(gg+1)])/2
        # Is the grplabel ok?
        labelnum = length(grplabel)
        if (labelnum < gg) {
            warning("more groups than labels (filling up with blanks)")
            grplabel = c(grplabel, rep(" ", gg-labelnum))
        } else if (gg < labelnum) {
            warning("more labels than groups (ignoring the extras)")
            grplabel = grplabel[1:gg]
        }
    }

    ## Switch coordinates, if you have to
    h2v <- function(cc) cbind(cc[,2]-totalh, cc[,1])
    if (horizontal) {
        grpaxis = 1
        labaxis = 2
        covaxis = 4
        las = 1
    } else {
        grpaxis = 4
        labaxis = 3
        covaxis = 1
        las = 3
        ## Rotate
        LL = h2v(LL)
        UR = h2v(UR)
        if (!is.null(grp)) {
            grp0 = h2v(grp0)
            grp1 = h2v(grp1)
        }
        for (i in 1:npanel) {
            panels[[i]][[1]] = h2v(panels[[i]][[1]])
            panels[[i]][[2]] = h2v(panels[[i]][[2]])
            panels[[i]]$labcc = panels[[i]]$labcc - totalh
            panels[[i]]$axcc  = panels[[i]]$axcc - totalh
        }
    }

    # Set up the plot
    plot(rbind(LL, UR), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
    # Add the colored rectangles, if required
    if (!is.null(grp)) {
        rect(grp0[,1], grp0[,2], grp1[,1], grp1[,2], col=grpcol,
             border="transparent")
    }
    # Loop over vars and fill in the panels
    for (i in 1:npanel) {
        if (bpanel[i]) {
            ## Do the rectangles
            with(panels[[i]], {
              rect(ll[,1], ll[,2], ur[,1], ur[,2], col=fill,
                   border="transparent")
            })
        } else {
            with(panels[[i]], points(raw[,1], raw[,2], pch=19))
            if (do.loess && (cc$degree>0) && (cc$span>0)){
                with(panels[[i]], lines(smo[,1], smo[,2]))
            }
            with(panels[[i]], axis(covaxis, at=axcc, label=axlab))
        }
        ## Name panel (regardless of type)
        if (!is.null(panels[[i]]$label)) {
            axis(labaxis, at=panels[[i]]$labcc, label=panels[[i]]$label, las=las, tick=FALSE, font=2, col=par("bg"), col.axis=par("fg"))
        }
    }
    # if grplabels are given, we add another horizontal axis to the
    # last plot (independent of whether it is binvar or contvar)
    if (!is.null(grp) & !is.null(grplabel)) {
        axis(grpaxis, grpcoord, label=FALSE, tcl=-1.5)
        axis(grpaxis, mids, label=grplabel, font=2, cex.axis=cc$cex.label, tick=FALSE)
    }

    invisible(panels)
}


###################################################
### chunk number 6: findBreaks_Def
###################################################
#line 379 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
niceBreaks <- function(xr, breaks, symbreaks=min(xr) < 0 & max(xr) > 0) {
  ## browser()
  ## If you want it, you get it
  if (length(breaks) > 1) {
    return(breaks)
  }
  if (symbreaks) {
    xr <- c(-1 * max(abs(xr)), max(abs(xr)))
  }

  ## Ok, so you proposed a number
  ## Neg and pos?
  if ((xr[1] < 0) && (xr[2] > 0)) {
    xminAbs <- abs(xr[1])
    xmax <- xr[2]
    nneg <- max(round(breaks * xminAbs/(xmax+xminAbs)), 1)
    npos <- max(round(breaks * xmax/(xmax+xminAbs)), 1)
    nbr <- pretty(c(xr[1], 0), nneg)
    pbr <- pretty(c(0, xr[2]), npos)
    ## Average of the proposed interval lengths,
    ##  nice enough for us
    diff <- ( (nbr[2]-nbr[1]) + (pbr[2] - pbr[1]) ) / 2
    nbr <- diff * ( (xr[1] %/% diff)  : 0 )
    pbr <- diff * ( 1 : (xr[2] %/% diff + 1) )
    breaks <- c(nbr, pbr)
  } else { ## only pos or negs
    breaks <- pretty(xr, breaks)
  }
  breaks
}


###################################################
### chunk number 7: breakColors_Def
###################################################
#line 425 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
breakColors <- function(breaks, colors, center=0, tol=0.001,
                        symkey=min(breaks) < 0 & max(breaks) > 0) {
  ## browser()
  ## In case of explicit color definitions
  nbreaks <- length(breaks)
  nclass <- nbreaks - 1
  if (!is.function(colors)) {
    ncolors <- length(colors)
    if (ncolors > nclass) {
      warning("more colors than classes: ignoring ", ncolors-nclass, " last colors")
      colors <- colors[1:nclass]
    } else if (nclass > ncolors) {
      stop(nclass-ncolors, " more classes than colors defined")
    }
  } else {
    ## Are the classes symmetric and of same lengths?
    clens <- diff(breaks)
    aclen <- mean(clens)
    if (aclen == 0) {
      stop("Your `breaks` are wonky")
    }
    relerr <- max((clens-aclen)/aclen)
    is.symmetric <- (center %in% breaks) && (relerr < tol) ## original

    ## steve
    ## Is this symmetric?
    nneg <- sum(breaks < center)
    npos <- sum(breaks > center)
    ratio <- min(c(nneg, npos)) / max(c(nneg, npos))
    is.symmetric <- isTRUE(symmkey) || (ratio > .25 && ratio < .78)

    if (is.symmetric) { ## yes, symmetric
      ndxcen <- which(breaks==center)
      kneg <- ndxcen -1
      kpos <- nbreaks - ndxcen
      kmax <- max(kneg, kpos)
      colors <- colors(2*kmax)
      if (kneg < kpos) {
        colors <- colors[(kpos-kneg+1) : (2*kmax)]
      } else if (kneg > kpos) {
        colors <- colors[1 : (2*kmax - (kneg-kpos))]
      }
    } else {                                      ## no, not symmetric
      colors <- colors(nclass)
    }
  }

  colors
}


###################################################
### chunk number 8: g2r.colors_Def
###################################################
#line 466 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
g2r.colors <- function(n=12, min.tinge = 0.33)
{
    k <- trunc(n/2)
    if (2 * k == n) {
        g <- c(rev(seq(min.tinge, 1, length = k)), rep(0, k))
        r <- c(rep(0, k), seq(min.tinge, 1, length = k))
        colvec <- rgb(
        r, g, rep(0, 2 * k))
    }
    else {
        g <- c(rev(seq(min.tinge, 1, length = k)), rep(0,
            k + 1))
        r <- c(rep(0, k + 1), seq(min.tinge, 1, length = k))
        colvec <- rgb(r, g, rep(0, 2 * k + 1))
    }
    colvec
}

###################################################
### chunk number 9: doLegend_Def
###################################################
#line 497 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
doLegend <- function(breaks, col, side)
{
    zval = ( breaks[-1] + breaks[-length(breaks)] ) / 2
    z  = matrix(zval, ncol=1)
    if (side %in% c(1,3)) {
        image(x=zval, y=1, z=z, xaxt="n", yaxt="n", col=col, breaks=breaks,
              xaxs="i", xlab="", ylab="")
    } else {
        image(x=1, y=zval, z=t(z), xaxt="n", yaxt="n", col=col, breaks=breaks,
              yaxs="i", xlab="", ylab="")
    }
    axis(side, las=1)
}


###################################################
### chunk number 10: convAnnData_Def
###################################################
#line 520 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
convAnnData <- function(x, nval.fac=3, inclRef=TRUE, asIs=FALSE)
{
    if (is.null(x)) return(NULL)
    if (asIs) {
        if (is.matrix(x) & is.numeric(x)) return(x)
        else stop("argument x not a numerical matrix, asIs=TRUE does not work")
    }

    x = as.data.frame(x)
    if (!is.null(nval.fac) & nval.fac>0) {
      doConv <- TRUE
    }
    vv = colnames(x)
    for (v in vv) {
        if (is.logical(x[,v])) {
            x[,v] = factor(as.numeric(x[,v]))
        }
        if (doConv & length(unique(x[is.finite(x[,v]),v])) <= nval.fac) {
            x[,v] = factor(x[,v])
        }
    }
    ret  = NULL
    ivar = 0
    for (v in vv) {
         if (is.factor(x[,v])) {
            if (length(unique(x[,v])) > 1) {
                naAction = attr(na.exclude(x[, v, drop=FALSE]), "na.action")
                modMat   = model.matrix(~x[,v]-1)
                if (!inclRef) modMat = modMat[ , -1, drop=FALSE]
                binvar   = naresid(naAction, modMat)
                colnames(binvar) = paste(v, "=", levels(x[,v])[if (!inclRef) -1 else TRUE], sep="")
            } else {
                ret = matrix(as.numeric(x[,v]), ncol=1)
                colnames(ret) = paste(v, "=", levels(x[,v]), sep="")
            }
            ret = cbind(ret, binvar)
            ivar = ivar + ncol(binvar)
         } else {
            ret = cbind(ret, x[,v])
            ivar = ivar + 1
            colnames(ret)[ivar] = v
         }
    }
    ret
}


###################################################
### chunk number 11: cut.dendrogram_Def
###################################################
#line 574 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
cutree.dendrogram <- function(x, h)
{
    # Cut the tree, get the labels
    cutx = cut(x, h)
    cutl = lapply(cutx$lower, getLeaves)
    # Set up the cluster vector as seen in the plot
    nclus = sapply(cutl, length)
    ret   = rep(1:length(nclus), nclus)
    # Return cluster membership in the order of the original data, if possible
    ord = order.dendrogram(x)
    # Is the order a valid permutation of the data?
    if (!all(sort(ord)==(1:length(ret)))) {
        stop("dendrogram order does not match number of leaves - is this a subtree?")
    }
    # Ok, proceed
    ret[ord] = ret
    ret = as.integer(factor(ret, levels=unique(ret))) # recode for order of clus
    names(ret)[ord] = unlist(cutl)
    ret
}


###################################################
### chunk number 12: getLeaves_Def
###################################################
#line 599 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
getLeaves <- function(x)
{
    unlist(dendrapply(x, function(x) attr(x, "label")))
}


###################################################
### chunk number 13: print.annHeatmap_Def
###################################################
#line 613 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
print.annHeatmap <- function(x, ...)
{
    cat("annotated Heatmap\n\n")
    cat("Rows: "); show(x$dendrogram$Row$dendro)
    cat("\t", if (is.null(x$annotation$Row$data)) 0 else ncol(x$annotation$Row$data), " annotation variable(s)\n")
    cat("Cols: "); show(x$dendrogram$Col$dendro)
    cat("\t", if (is.null(x$annotation$Col$data)) 0 else ncol(x$annotation$Col$data), " annotation variable(s)\n")
    invisible(x)
}


###################################################
### chunk number 14: RainbowPastel_Def
###################################################
#line 632 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
RainbowPastel <- function (n, blanche=200, ...)
#
# Name: RainbowPastel
# Desc: constructs a rainbow clolr vector, but more pastelly
# Auth: Alexander.Ploner@mep.ki.se      030304
#
# Chng:
#

{
    cv = rainbow(n, ...)
    rgbcv = col2rgb(cv)
    rgbcv = pmin(rgbcv+blanche, 255)
    rgb(rgbcv[1,], rgbcv[2,], rgbcv[3, ], maxColorValue=255)
}


###################################################
### chunk number 15: cutplot_dendrogam_Def
###################################################
#line 652 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
cutplot.dendrogram <- function(x, h, cluscol, leaflab= "none", horiz=FALSE, lwd=3, ...)
#
# Name: cutplot.dendrogram
# Desc: takes a dendrogram as described in library(mva), cuts it at level h,
#       and plots the dendrogram with the resulting subtrees in different
#       colors
# Auth: obviously based on plot.dendrogram in library(mva)
#       modifications by Alexander.Ploner@meb.ki.se  211203
#
# Chng: 050204 AP
#       changed environment(plot.hclust) to environment(as.dendrogram) to
#       make it work with R 1.8.1
#       250304 AP added RainbowPastel() to make it consistent with picketplot
#       030306 AP slightly more elegant access of plotNode
#       220710 AP also for horizontal plots
#       120811 AP use edgePar instead of par() for col and lwd
#
{
    ## If there is no cutting, we plot and leave
    if (missing(h) | is.null(h)) {
        return(plot(x, leaflab=leaflab, horiz=horiz, edgePar=list(lwd=lwd), ...))
    }
    ## If cut height greater than tree, don't cut, complain and leave
    treeheight = attr(x, "height")
    if (h >= treeheight) {
        warning("cutting height greater than tree height ", treeheight, ": tree uncut")
        return(plot(x, leaflab=leaflab, horiz=horiz, edgePar=list(lwd=lwd), ...))
    }

    ## Some param processing
    if (missing(cluscol) | is.null(cluscol)) cluscol = RainbowPastel

    # Not nice, but necessary
    pn  = stats:::plotNode

    x = cut(x, h)
    plot(x[[1]], leaflab="none", horiz=horiz, edgePar=list(lwd=lwd), ...)

    x = x[[2]]
    K = length(x)
    if (is.function(cluscol)) {
       cluscol = cluscol(K)
    }
    left = 1
    for (k in 1:K) {
        right = left + attr(x[[k]],"members")-1
        if (left < right) {         ## not a singleton cluster
            pn(left, right, x[[k]], type="rectangular", center=FALSE,
                 leaflab=leaflab, nodePar=NULL, edgePar=list(lwd=lwd, col=cluscol[k]), horiz=horiz)
        } else if (left == right) { ## singleton cluster
            if (!horiz) {
                segments(left, 0, left, h, lwd=lwd, col=cluscol[k])
            } else {
                segments(0, left, h, left, lwd=lwd, col=cluscol[k])
            }
        } else stop("this totally should not have happened")
        left = right + 1
   }

}


## steve
## Helper function to build ordered dendrograms from the data
## returns a dendrogram
ordered.dendrogram <- function(x, distfun, clustfun, dist.method='euclidean',
                               clust.method='complete',
                               order.type=c('optimal', 'mean'),
                               na.method=0) {
  order.type <- match.arg(order.type)
  cat("Order:", order.type, "\n")
  if (order.type == 'optimal') {
    if (!require(cba)) {
      stop("`cba` package required for optimal reordering")
    }
  }

  if (is.numeric(na.method)) {
    x[is.na(x)] <- 0
  }

  cat("dist.method:", dist.method, "\n")
  cat("clust.method:", clust.method, "\n")

  x.dist <- distfun(x, method=dist.method)
  x.clust <- clustfun(x.dist, clust.method)

  ## There is a bug in order.opimal when length(x.dist) == 1L
  if (length(x.dist) == 1L || order.type != 'optimal') {
    d <- reorder(as.dendrogram(x.clust), rowMeans(x, na.rm=TRUE))
  } else if (order.type == 'optimal') {
    x.opt <- order.optimal(x.dist, x.clust$merge)
    n.clust <- x.clust
    n.clust$merge <- x.opt$merge
    n.clust$order <- x.opt$order
    d <- as.dendrogram(n.clust)
  }

  d
}

###################################################
### chunk number 16: annHeatmap2_Def
###################################################
#line 727 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
myAnnHeatmap2 <- function(x, dendrogram, annotation, cluster, labels,
                          scale=c("rows", "columns", "none"), breaks=256,
                          col=g2r.colors, col.map=c('bi', 'cold2hot', 'hot2cold'),
                          legend=FALSE,
                          symbreaks=min(x, na.rm=TRUE) < 0 & max(x, na.rm=TRUE) > 0,
                          symkey=symbreaks,
                          extremes=c(0.05, 0.95),
                          extremes.are=c('quantiles', 'values'),
                          na.action=0)
#
# Name: annHeatmap2
# Desc: a (possibly doubly) annotated heatmap
# Auth: Alexander.Ploner@ki.se 2010-07-12
#
# Chng:
#
{
    ## -------------------------------------------------------------------------
    ## steve
    ## -------------------------------------------------------------------------
    extremes.are <- match.arg(extremes.are)

    if (missing(col) || is.null(col)) {
      if (missing(col.map)) {
        ratio <- (sum(x < 0, na.rm=TRUE) + 1e6) / (sum(!is.na(x)) + 1e6)
        if (ratio > .2 && ratio < .8) {
          col.map <- 'cold2hot'
        } else {
          col.map <- 'bi'
        }
      } else {
        col.map <- match.arg(col.map)
      }

      if (col.map == 'bi') {
        col <- heat.colors
      } else if (col.map == 'cold2hot') {
        col <- colorRampPalette(c(blue, light.grey, red))
      } else {
        col <- colorRampPalette(c(red, light.grey, blue))
      }
    }

    ## -------------------------------------------------------------------------

    ## Process arguments
    if (!is.matrix(x) | !is.numeric(x)) stop("x must be a numeric matrix")
    nc = ncol(x); nr = nrow(x)
    if (nc < 2 | nr < 2) stop("x must have at least two rows/columns")

    ## Process the different lists: dendrogram, cluster, annotation
    ## See lattice:::xyplot.formula, modifyLists, lattice:::construct.scales
    def <- list(clustfun=hclust, distfun=dist, status="yes", dendro=NULL,
                dist.method='euclidean', clust.method='complete', order="optimal")
    dendrogram <- extractArg(dendrogram, def)

    def <- list(data=NULL, control=list(), asIs=FALSE, inclRef=TRUE)
    annotation <- extractArg(annotation, def)

    def <- list(cuth=NULL, grp=NULL, label=NULL, col=RainbowPastel)
    cluster <- extractArg(cluster, def)

    def <- list(cex=NULL, nrow=3, side=NULL, labels=NULL)
    labels <- extractArg(labels, def)
    ## Check values for the different lists


    ## Generate the layout: TRUE means default, FALSE means none
    ## Otherwise, integer 1-4 indicates side
    if (is.logical(legend)) {
        if (legend) leg = NULL else leg = 0
    } else {
        if (!(legend %in% 1:4)) {
          stop("invalid value for legend: ", legend)
        } else {
          leg <- legend
        }
    }
    layout <- heatmapLayout(dendrogram, annotation, leg.side=leg)

    ## Copy the data for display, scale as required
    x2 <- x
    scale <- match.arg(scale)
    cat("scale:", scale, "\n")
    if (scale == "rows") {
      x2 <- sweep(x2, 1, rowMeans(x, na.rm = TRUE))
      sx <- apply(x2, 1, sd, na.rm = TRUE)
      x2 <- sweep(x2, 1, sx, "/")
    }
    else if (scale == "columns") {
      x2 <- sweep(x2, 2, colMeans(x, na.rm = TRUE))
      sx <- apply(x2, 2, sd, na.rm = TRUE)
      x2 <- sweep(x2, 2, sx, "/")
    }

    ## steve : trim the extremes?
    if (is.numeric(extremes)) {
      if (extremes.are == 'quantiles') {
        if (length(extremes) == 1L && extremes > 0 && extremes < 1) {
          extremes <- sort(c(1 - extremes, extremes))
        }
        extremes <- quantile(x2, extremes, na.rm=TRUE)
      }
      if (length(extremes) != 2 || is.unsorted(extremes)) {
        stop("extremes must be a sorted vector of length 2")
      }
      ## browser()
      x2[x2 < extremes[1]] <- extremes[1]
      x2[x2 > extremes[2]] <-  extremes[2]
    } else {
      extremes <- range(x2, na.rm=TRUE)
    }
    cat("extremes:", extremes, "\n")
    ## Construct the breaks and colors for display
    ## breaks = niceBreaks(range(x2), breaks) ## steve
    breaks <- niceBreaks(extremes, breaks, symbreaks=symbreaks) ## steve
    col <- breakColors(breaks, col, symkey=symmkey)

    ## Generate the dendrograms, if required; re-indexes in any cases
    ## We could put some sanity checks on the dendrograms in the else-branches
    ## FIXME: store the names of the functions, not the functions in the object
    ## steve
    ## dendrogram$Row = within(dendrogram$Row,
    ##     if (!inherits(dendro, "dendrogram")) {
    ##         dendro = clustfun(distfun(x))
    ##         dendro = reorder(as.dendrogram(dendro), rowMeans(x, na.rm=TRUE))
    ##     }
    ## )
    ## dendrogram$Col = within(dendrogram$Col,
    ##     if (!inherits(dendro, "dendrogram")) {
    ##         dendro = clustfun(distfun(t(x)))
    ##         dendro = reorder(as.dendrogram(dendro), colMeans(x, na.rm=TRUE))
    ##     }
    ## )
    dendrogram$Row <- within(dendrogram$Row, {
      if (!inherits(dendro, 'dendrogram')) {
        dendro=ordered.dendrogram(x, distfun, clustfun, dist.method,
                                  clust.method, order)
      }
    })
    dendrogram$Col <- within(dendrogram$Col, {
      if (!inherits(dendro, 'dendrogram')) {
        dendro=ordered.dendrogram(t(x), distfun, clustfun, dist.method,
                                  clust.method, order)
      }
    })

    ## Reorder the display data to agree with the dendrograms, if required
    rowInd <- with(dendrogram$Row, {
      if (status != "no") order.dendrogram(dendro) else 1:nr
    })
    colInd <- with(dendrogram$Col, {
      if (status != "no") order.dendrogram(dendro) else 1:nc
    })
    x2 = x2[rowInd, colInd]

    ## Set the defaults for the sample/variable labels
    labels$Row = within(labels$Row, {
        if (is.null(cex)) cex = 0.2 + 1/log10(nr)
        if (is.null(side)) side = if (is.null(annotation$Row$data)) 4 else 2
        if (is.null(labels)) labels = rownames(x2)
    })
    labels$Col = within(labels$Col, {
        if (is.null(cex)) cex = 0.2 + 1/log10(nc)
        if (is.null(side)) side = if (is.null(annotation$Col$data)) 1 else 3
        if (is.null(labels)) labels = colnames(x2)
    })

    ## Generate the clustering, if required (cut, or resort the cluster var)
    ## FIXME: does not deal with pre-defined grp form outside
    cluster$Row = within(cluster$Row,
        if (!is.null(cuth) && (cuth > 0)) {
            grp = cutree.dendrogram(dendrogram$Row$dendro, cuth)[rowInd]
        })
    cluster$Col = within(cluster$Col,
        if (!is.null(cuth) && (cuth > 0)) {
            grp = cutree.dendrogram(dendrogram$Col$dendro, cuth)[colInd]
        })

    ## Process the annotation data frames (factor/numeric, re-sort?)
    annotation$Row = within(annotation$Row, {
        data = convAnnData(data, asIs=asIs, inclRef=inclRef)
    })
    annotation$Col = within(annotation$Col, {
        data = convAnnData(data, asIs=asIs, inclRef=inclRef)
    })


    ## Generate the new object

    ## print, return invisibly
    ret <- list(data=list(x=x, x2=x2, rowInd=rowInd, colInd=colInd,
                  breaks=breaks, col=col),
                dendrogram=dendrogram, cluster=cluster, annotation=annotation,
                labels=labels, layout=layout, legend=legend)
    class(ret) <- "annHeatmap"
    ret
}


###################################################
### chunk number 17: plot.annHeatmap_Def
###################################################
#line 852 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"

## To tweak how each row/column of the anno.df is printed, pass in a list
## with of arguments into the `...` that has the same name as the colname
## of the column in the annotation df that you want to tweak, ie. to turn
## off plotting of the loess line for the `umi` column, you do:
##  plot(x, umi=list(loess=FALSE))
plot.annHeatmap <- function(x, widths, heights, main=NULL, ...)
{
    opar <- par()
    on.exit(suppressWarnings(par(opar)))
    ## Set up the layout
    if (!missing(widths)) x$layout$width = widths
    if (!missing(heights)) x$layout$height = heights
    with(x$layout, layout(plot, width, height, respect=TRUE))

    ## Plot the central image, making space for labels, if required
    nc = ncol(x$data$x2); nr = nrow(x$data$x2)
    doRlab = !is.null(x$labels$Row$labels)
    doClab = !is.null(x$labels$Col$labels)
    mmar = c(1, 0, 0, 2)
    if (doRlab) mmar[x$labels$Row$side] = x$labels$Row$nrow
    if (doClab) mmar[x$labels$Col$side] = x$labels$Col$nrow
    with(x$data, {
        par(mar=mmar)
        image(1:nc, 1:nr, t(x2), axes=FALSE, xlim=c(0.5, nc + 0.5),
              ylim=c(0.5, nr + 0.5), xlab="", ylab="", col=col,
              breaks=breaks, ...)
    })
    with (x$labels, {
        if (doRlab) {
          axis(Row$side, 1:nr, las=2, line=-0.5, tick=0, labels=Row$labels,
               cex.axis=Row$cex)
        }
        if (doClab) {
          axis(Col$side, 1:nc, las=2, line=-0.5, tick=0, labels=Col$labels,
               cex.axis=Col$cex)
        }
    })

    ## Plot the column/row dendrograms, as required
    with(x$dendrogram$Col,
        if (status=="yes") {
            par(mar=c(0, mmar[2], 3, mmar[4]))
            cutplot.dendrogram(dendro, h=x$cluster$Col$cuth,
                               cluscol=x$cluster$Col$col, horiz=FALSE,
                               axes = FALSE, xaxs = "i", leaflab = "none")
        })

    if (!is.null(main)) {
      title(main)
    }

    with(x$dendrogram$Row,
        if (status=="yes") {
            par(mar=c(mmar[1], 3, mmar[3], 0))
            cutplot.dendrogram(dendro, h=x$cluster$Row$cuth,
                               cluscol=x$cluster$Row$col, horiz=TRUE,
                               axes = FALSE, yaxs = "i", leaflab = "none")
        })

    ## Plot the column/row annotation data, as required
    if (!is.null(x$annotation$Col$data)) {
        par(mar=c(1, mmar[2], 0, mmar[4]), xaxs="i", yaxs="i")
        picketPlot(x$annotation$Col$data[x$data$colInd,,drop=FALSE],
                   grp=x$cluster$Col$grp, grpcol=x$cluster$Col$col,
                   control=x$annotation$Col$control, asIs=TRUE, ...)
    }
    if (!is.null(x$annotation$Row$data)) {
        par(mar=c(mmar[1], 0, mmar[3], 1), xaxs="i", yaxs="i")
        picketPlot(x$annotation$Row$data[x$data$rowInd,,drop=FALSE],
                   grp=x$cluster$Row$grp, grpcol=x$cluster$Row$col,
                   control=x$annotation$Row$control, asIs=TRUE,
                   horizontal=FALSE, ...)
    }

    ## Plot a legend, as required
    if (x$legend) {
        if (x$layout$legend.side %in% c(1,3)) {
            par(mar=c(2, mmar[2]+2, 2, mmar[4]+2))
        } else {
            par(mar=c(mmar[1]+2, 2, mmar[3]+2, 2))
        }
        doLegend(x$data$breaks, col=x$data$col, x$layout$legend.side)
    }

    invisible(x)

}

hm <- function(x, annotation=NULL,
               dendrogram=list(clustfun=hclust, distfun=dist, order="optimal"),
               cluster=NULL, labels=NULL, legend=TRUE, scale="row",
               symbreaks=min(x, na.rm=TRUE) < 0 & max(x, na.rm=TRUE) > 0,
               symkey=symbreaks, ...) {
  if (!is.null(annotation)) {
    ## dendrogram$Col <- list(status="yes")
    ## dendrogram$Row <- list(status="hidden")
    ## dendrogram$Row <- list(status="yes")
    if (any(rownames(annotation) != colnames(x))) {
      stop("Ensure that the rownames of the annotation df = colnames of x")
    }
    annotation <- list(Col=list(data=annotation, fun=picketPlot))
    if (isTRUE(legend)) {
      legend <- 4
    }
  } else {
    dendrogram$status <- "yes"
  }

  args <- list(...)
  col <- args$col
  ret <- myAnnHeatmap2(x, dendrogram=dendrogram, annotation=annotation,
                       cluster=cluster, labels=labels, legend=legend, col=col,
                       scale=scale, symmkey=symmkey, symmbreaks=symmbreaks)
  ret
}

###################################################
### chunk number 18: Generics_Def
###################################################
#line 923 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
## regHeatmap <- function(x, ...) UseMethod("regHeatmap")
## annHeatmap <- function(x, ...) UseMethod("annHeatmap")


###################################################
### chunk number 19: regHeatmap_Def
###################################################
#line 933 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
## regHeatmap.default <-
##   function(x, dendrogram=list(clustfun=hclust, distfun=dist, status="yes"),
##            labels=NULL, legend=TRUE, ...) {
##     ret <- annHeatmap2(x, dendrogram=dendrogram, annotation=NULL, cluster=NULL,
##                        labels=labels, legend=legend, ...)
##     ret
## }


###################################################
### chunk number 20: annHeatmap_Def
###################################################
#line 946 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
annHeatmap.default <-
  function(x, annotation, dendrogram=list(clustfun=hclust, distfun=dist, Col=list(status="yes"), Row=list(status="hidden")), cluster=NULL, labels=NULL, legend=TRUE, ...)
{
    ret = annHeatmap2(x, dendrogram=dendrogram, annotation=list(Col=list(data=annotation, fun=picketPlot)), cluster=cluster,  labels=labels, legend=TRUE, ...)
    ret
}


###################################################
### chunk number 21: annHeatmapExpressionSet_Def
###################################################
#line 958 "Heatplus/inst/doc/annHeatmapCommentedSource.Rnw"
## annHeatmap.ExpressionSet =function(x, ...)
## {
##     expmat = exprs(x)
##     anndat = pData(x)
##     annHeatmap(expmat, anndat, ...)
## }

################################################################################
## Tests
if (FALSE) {
  red <- '#AC000E'
  blue <- '#22588C'
  light.grey <- '#F8F8F8'

  grey <- '#b3b3b3'
  green <- '#186A38'
  purple <- '#786CB2'

  bwr.ramp <- colorRampPalette(c(blue, light.grey, red))
  rwb.ramp <- colorRampPalette(c(red, light.grey, blue))

  ## is colorbar sckewed?
  pop.1 <- matrix(rnorm(500, 5), ncol=10)
  pop.2 <- matrix(rnorm(100, 0), ncol=10)
  M <- rbind(pop.1, pop.2)

  plot(hm(rbind(pop.1, pop.2), col=bwr.ramp, scale="none"))
}
