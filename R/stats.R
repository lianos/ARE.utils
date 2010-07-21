# The first argument (Ta) can be either a table or a matrix of 2X2.
# Or instead, the values of the table can be entered one by one to the function 

# Barnard's test calculates the probabilities for contingency tables.  It has been shown that for 2x2 tables, Barnard's test
# has a higher power than Fisher's Exact test.  Barnard's test is a non-parametric test that relies upon a computer to generate
# the distribution of the Wald statistic.  Using a computer program, one could find the nuisance parameter that maximizes the 
# probability of the observations displayed from a table.
# Despite giving lower p-values for 2x2 tables, Barnard's test hasn't been used as often as Fisher's test because of its
# computational difficulty.  This code gives the Wald statistic, the nuisance parameter, and the p-value for any 2x2 table.
# The table can be written as:
# 			Var.1
# 		 ---------------
# 		   a		b	 r1=a+b
# 	Var.2
# 		   c		d	 r2=c+d
# 		 ---------------
# 		 c1=a+c   c2=b+d	 n=c1+c2 
  
# One example would be 
# 				Physics
# 			 Pass	     Fail
# 			 ---------------
# 		Crane	   8		14
#   Collage	
# 		Egret	   1		3
# 			 ---------------
# 
# After implementing the function, simply call it by the command:
# Barnardextest(8,14,1,3)
# This will display the results: 
  
# "The contingency table is:"
#       [,1] [,2]
# [1,]    8   14
# [2,]    1    3
# "Wald Statistic:"
# 0.43944
# "Nuisance parameter:"
# 0.9001
# "The 1-tailed p-value:"
# 0.4159073 
  
# On a side note, I believe there are more efficient codes than this one.  For example, I've seen codes in Matlab that run
# faster and display nicer-looking graphs.  However, this code will still provide accurate results and a plot that gives the
# p-value based on the nuisance parameter.  I did not come up with the idea of this code, I simply translated Matlab code 
# into R, occasionally using different methods to get the same result.  The code was translated from:
# 
# Trujillo-Ortiz, A., R. Hernandez-Walls, A. Castro-Perez, L. Rodriguez-Cardozo. Probability Test.  A MATLAB file. URL
# http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=6198
# 
# My goal was to make this test accessible to everyone.  Although there are many ways to run this test through Matlab, I hadn't
# seen any code to implement this test in R.  I hope it is useful for you, and if you have any questions or ways to improve
# this code, please contact me at calhoun.peter@gmail.com. 
#
# Usage:
#   Barnardextest(matrix(c(8,1,14,3),2,2)) 
#   fisher.test(matrix(c(8,1,14,3),2,2)) 
#   Convictions <- matrix(c(2, 10, 15, 3), nrow = 2, dimnames = list(c("Dizygotic", "Monozygotic"), c("Convicted", "Not convicted")))
#   Convictions
#   fisher.test(Convictions, alternative = "less")
barnardExTest<-function(Ta, Tb=NULL, Tc=NULL, Td=NULL, to.print=FALSE, to.plot=TRUE) {
  # Tal edit: choosing if to work with a 2X2 table or with 4 numbers:
  if (is.null(Tb) | is.null(Tc) | is.null(Td)) {
    # If one of them is null, then Ta should have an entire table, and we can take it's values
    if (is.table(Ta) | is.matrix(Ta)) {
      if(sum(dim(Ta) == c(2,2)) == 2) {
        Tb <- Ta[1,2]
        Tc <- Ta[2,1]
        Td <- Ta[2,2]
        Ta <- Ta[1,1]
      } else {
        stop("The table is not 2X2, please check it again...")
      }
    } else {
      stop("We are missing value in the table")
    }
  } 

  c1<-Ta+Tc
  c2<-Tb+Td
  n<-c1+c2
  pao<-Ta/c1
  pbo<-Tb/c2
  pxo<-(Ta+Tb)/n
  TXO<-abs(pao-pbo)/sqrt(pxo*(1-pxo)*(1/c1+1/c2))
  n1<-prod(1:c1)
  n2<-prod(1:c2) 

  P<-{}
  for( p in (0:99+.01)/100) {
    TX<-{}
    S<-{}
    for(i in c(0:c1)) {
      for( j in c(0:c2)) {
        if (prod(1:i)==0){fac1<-1} else {fac1<-prod(1:i)}
        if (prod(1:j)==0){fac2<-1} else {fac2<-prod(1:j)}
        if (prod(1:(c1-i))==0){fac3<-1} else {fac3<-prod(1:(c1-i))}
        if (prod(1:(c2-j))==0){fac4<-1} else {fac4<-prod(1:(c2-j))} 
        
        small.s<-(n1*n2*(p^(i+j))*(1-p)^(n-(i+j))/(fac1*fac2*fac3*fac4))
        S<-cbind(S,small.s)
        pa<- i/c1
        pb<-j/c2
        px <- (i+j)/n
        if (is.nan((pa-pb)/sqrt(px*(1-px)*((1/c1)+(1/c2))))) {
          tx<-0
        } else {
          tx <- (pa-pb)/sqrt(px*(1-px)*((1/c1)+(1/c2)))
        }
        TX<-cbind(TX,tx)
      }
    }
    
    P<-cbind(P,sum(S[which(TX>=TXO)]))
  } 
  
  np<-which(P==max(P))
  p <- (0:99+.01)/100
  nuisance<-p[np]
  pv<-P[np] 

  if (to.print) {
    print("The contingency table is:")
    print(matrix(c(Ta,Tc,Tb,Td),2,2))
    print("Wald Statistic:")
    print(TXO)
    print("Nuisance parameter:")
    print(nuisance)
    print("The 1-tailed p-value:")
    print(pv)
  }
  
  if (to.plot) {
    plot(p,P,type="l",main="Barnard's exact P-value", xlab="Nuisance parameter", ylab="P-value")
    points(nuisance,pv,col=2)
  }
  
  # Tal edit: Returning the results in a list:
  return(list(contingency.table=as.table(matrix(c(Ta,Tc,Tb,Td),2,2)),
              Wald.Statistic=TXO, Nuisance.parameter=nuisance,
              p.value.one.tailed=pv))
} 

#' Standardize an input vector/matrix along its columns
#' 
#' Defaults to 0-centering the data. \code{standardizeData(X, scale.by="sd")}
#' is the z-transform
#' 
#' @param X The data (vector or matrix)
#' @param center logical indicating whether to center, or a numeric indicating
#'    the value to center the data by.
#' @param scale.by Method to scale the data by. \code{sd} scales the data by
#'    its standard deviation, \code{norm} scales by the norm of the vectors
#'    in the columns of \code{data}
#' @paran center.value Optional value to use for centering if the default
#'    zero-centering isn't desired
#' @param scale.value Optional value to scale by, if the default column
#'    standard deviation
#' @return A list containing the new data along with details of the
#'    transformations applied to it.
standardizeData <- function(data, center=TRUE, 
                            scale.by=c('none', 'sd', 'norm', 'numeric'),
                            scale.value=NA, na.rm=TRUE) {
  is.valid.numeric <- function(value) {
    if (!is.numeric(value)) {
      return(FALSE)
    } else {
      if (length(value) == 1) {
        return(TRUE)
      } else if (length(value) != ncol(X)) {
        return(FALSE)
      }
    }
  }
  if (is.vector(data)) {
    ret.vector <- TRUE
    data <- matrix(data, ncol=1)
  } else {
    ret.vector <- FALSE
  }
  center.value <- NA
  scale.by <- match.arg(scale.by)
  if ((is.logical(center) && center) || is.valid.numeric(center)) {
    if (is.logical(center)) {
        center.value <- colMeans(data, na.rm=TRUE)
    }
    data <- scale(data, center=center.value, scale=FALSE)
  }
  
  if (scale.by != 'none') {
    if (!is.na(scale.value)) {
      if (!is.valid.numeric(scale.value)) {
        stop("Illegal value for scale.value")
      }
    } else {
      scale.value <- switch(scale.by,
        sd=apply(data, 2, sd, na.rm=na.rm),
        norm=sqrt(drop(rep(1, nrow(data)) %*% data^2))
      )
    }
    data <- scale(data, center=FALSE, scale=scale.value)
  }
  if (ret.vector) {
    data <- as.vector(data)
  }
  
  list(data=data, center.value=center.value, scale.by=scale.by,
       scale.value=scale.value)
}

standardizeData.old <- function(X, Y, center=TRUE, scale=c('norm', 'sd', 'none'),
                                mu=mean(Y, na.rm=TRUE)) {
  # In lars, to normalize data:
  #   X : subtract mean from columns then divide by the column norm
  #   Y : subtract mean from Y
  if (is.logical(center) && center) {
    center <- colMeans(X)
    X <- scale(X, center=center, FALSE)
  } else if (is.numeric(center)) {
    X <- scale(X, center=center, FALSE)
  }
  
  if (is.character(scale)) {
    scale.by <- match.arg(scale)
    do.scale <- scale.by != 'none'
  } else if (is.numeric(scale)) {
    scale.by <- 'numeric'
    do.scale <- TRUE
  } else {
    scale.by <- NA
    do.scale <- FALSE
  }
  
  if (do.scale) {
    scale.val <- switch(scale.by, 
      norm={
        browser()
        one <- rep(1, nrow(X))
        sqrt(drop(one %*% X^2)) # column norm
      },
      sd=apply(X, 1, sd, na.rm=TRUE),
      numeric=scale,
      stop("Unknown scaling term")
    )
    X <- scale(X, FALSE, scale.val)
  } else {
    scale.val <- rep(1, ncol=(X))
  }
  
  if (is.numeric(mu)) {
    Y <- Y - mu
  }
  
  list(X=X, Y=Y, center=center, scale.by=scale.by, scale.val=scale.val, mu=mu)
}

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
  if (!is.vector(Y)) {
    Y <- as.vector(Y)
  }
  stopifnot(nrow(predicted) == length(Y))
  colMeans((predicted - Y)^2)
}

evalR2 <- function(predicted, Y) {
  # http://en.wikipedia.org/wiki/Coefficient_of_determination
  # R2 "as explained variance" := 1 - SSerr / SStot
  # 
  #    SSerr = Residual sum of squares
  #            \sum_i (y_i - f_i)^2
  #    SStot = Total sum of squares (proportinal to sample variance)
  #            \sum_i (y_i - mean(y))^2
  #
  # compares the explained variance (variance of the model's predictions)
  # with the total variance (of the data)
  #
  # NOTE: This formulation matches exactly with glmnet()$dev when used
  #       calculated on training data and preds on the data model was
  #       trained on
  # TODO: This gives negative values for R2 on held out data! 
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  if (!is.vector(Y)) {
    Y <- as.vector(Y)
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
  if (!is.vector(Y)) {
    Y <- as.vector(Y)
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

evalAccuracy <- function(predicted, Y) {
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  if (!is.vector(Y)) {
    Y <- as.vector(Y)
  }
  stopifnot(nrow(predicted) == length(Y))
  apply(predicted, 2, function(col) {
    sum(sign(col) == Y) / length(Y)
  })
}

regressionPerformance <- function(eval.by=c('mse', 'r2', 'spearman', 'accuracy')) {
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
  } else if (eval.by == 'accuracy') {
    evalF <- evalAccuracy
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
