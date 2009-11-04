z.transform <- function(x, along=1, na.rm=TRUE) {
  if (is.vector(x)) {
    x <- matrix(x, nrow=1)
    along <- 1
  } else {
    along <- match.dim(along)
  }
  
  # # Determine which dimension the mean function should run over.
  # if (along == 1) {
  #   do.mean <- rowMeans
  # } else if (along == 2) {
  #   do.mean <- colMeans
  # } else {
  #   stop('z.transform undefined for data greater than 2d')
  # }
  # 
  # # Subtract the means from the appropriate dimension
  # x <- sweep(x, along, do.mean(x, na.rm=na.rm), "-", check.margin=FALSE)
  
  # calculate the sd for the appropriate dimension and divide it out
  # sds <- apply(x, along, sd, na.rm=na.rm)
  # sweep(x, along, sds, "/", check.margin=FALSE)
  
  if (along == 1) {
    t(scale(t(x)))
  } else {
    scale(x)
  }
}
z.score <- z.transform

dhist <- function(x, a=5*diff(quantile(x, c(0.25,0.75))), nbins=10,
                  rx=range(x)) {
  # Computes (irregular) histogram breaks
  # Provided by Lorraine Denby
  # 
  # Usage
  # 
  #   > hist(mtcars$mpg, breaks=dhist)
  #   > hist(mtcars$mpg, breaks=dhist(mtcars$mpg, a=500))
  # 
  # Taken from a post by Hadley Wickham on Gelman's blog:
  # http://www.stat.columbia.edu/~cook/movabletype/archives/2009/10/variations_on_t.html
  x <- sort(x)
  if (a == 0)
    a <- diff(range(x))/100000000
 
  if (a != Inf) {
    n <- length(x)
    h <- (rx[2] + a - rx[1])/nbins
    ybr <- rx[1] + h * (0:nbins)
    yupper <- x + (a * (1:n))/n
 
    # upper and low Fer corners in the ecdf
    ylower <- yupper - a/n
 
    cmtx <- cbind(
      cut(yupper, breaks = ybr), 
      cut(yupper, breaks = ybr, left.include = T), 
      cut(ylower, breaks = ybr),
      cut(ylower, breaks = ybr, left.include = T)
    )
    cmtx[1, 3] <- cmtx[1, 4] <- 1
    # to replace NAs when default r is used
    cmtx[n, 1] <- cmtx[n, 2] <- nbins
 
    checksum <- rowSums(cmtx) %% 4
    # will be 2 for obs. that straddle two bins
    straddlers <- (1:n)[checksum == 2]
 
    # to allow for zero counts
    if(length(straddlers) > 0) {
      counts <- table(c(1:nbins, cmtx[- straddlers, 1])) 
    } else {
      counts <- table(c(1:nbins, cmtx[, 1]))
    }
    counts <- counts - 1
    
    if(length(straddlers) > 0) {
      for(i in straddlers) {
        binno <- cmtx[i, 1]
        theta <- ((yupper[i] - ybr[binno]) * n)/a
        counts[binno - 1] <- counts[binno - 1] + (1 - theta)
        counts[binno] <- counts[binno] + theta
      }
    }
    xbr <- ybr
    xbr[-1] <- ybr[-1] - (a * cumsum(counts))/n
  } else {
    bin.size <- length(x)/nbins
    cut.pt <- c(
      min(x) - abs(min(x))/1000, 
      approx(seq(length(x)),
      x, 
      (1:(nbins - 1)) * bin.size, rule = 2)$y, 
      max(x)
    )
    aa <- hist(x, breaks = cut.pt, plot = F, probability = T)
    xbr <- aa$breaks
  }
 
  xbr
}
