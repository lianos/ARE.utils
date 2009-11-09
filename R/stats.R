###############################################################################
# Functions to evaluate regression performance
# --------------------------------------------
# 
# Predicted values can be a column of predictions representing different target
# predictions for different params of a regression model, eg. as returned
# by glmnet for different values of the lambda penalty parameter
###############################################################################

evalMse <- function(predicted, Y) {
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  stopifnot(nrow(predicted) == length(Y))
  colMeans((predicted - Y)^2)
}

evalR2 <- function(predicted, Y) {
  ## http://en.wikipedia.org/wiki/Coefficient_of_determination
  ## R2 "as explained variance" := 1 - SSerr / SStot
  ## 
  ## compares the explained variance (variance of the model's predictions)
  ## with the total variance (of the data)
  ##
  ## NOTE: This formulation matches exactly with glmnet()$dev when used
  ##       calculated on training data and preds on the data model was
  ##       trained on
  ## TODO: This gives negative values for R2 on held out data! 
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  stopifnot(nrow(predicted) == length(Y))
  ss.err <- colSums((predicted - Y)^2)
  ss.tot <- sum((Y - mean(Y))^2)
  1 - (ss.err / ss.tot)
}

evalSpearman <- function(predicted, Y) {
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  stopifnot(nrow(predicted) == length(Y))
  if (sd(Y) == 0) {
    stop("Standard deviation of target variables are 0 : screws the spearman test")
  }
  apply(predicted, 2, function(col) {
    if (sd(col) == 0) {
      NA
    } else {
      cor.test(col, Y, method='spearman')$estimate
    }
  })
}

regressionPerformance <- function(eval.by=c('mse', 'r2', 'spearman')) {
  eval.by <- match.arg(eval.by)
  if (eval.by == 'mse') {       # Evaluate by Mean Squared Error
    evalF <- evalMse
    best <- min
    which.best <- which.min
  } else if (eval.by == 'r2') { # Evalute by fraction of explained varaiance
    evalF <- evalR2
    best <- max
    which.best <- which.max
  } else if (eval.by == 'spearman') {
    evalF <- evalSpearman
    best <- max
    which.best <- which.max
  }
  list(eval=evalF, best=best, which.best=which.best)
}

###############################################################################
# Miscelaneous
###############################################################################
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
