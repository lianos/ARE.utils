ind2sub <- function(idxs, nrow, ncol=NULL) {
  # Returns a (length(idxs) x 2) matrix of (x,y) indices the linear indices in
  # `idxs` (R stores matrices in column-major).
  # 
  # You might want to look at `which(something, arr.ind=TRUE)` instead
  if (is.matrix(idxs)) {
    if (is.logical(idxs)) {
      nrow <- dim(idxs)[1]
      ncol <- dim(idxs)[2]
      idxs <- which(idxs)
    } else {
      stop("Don't know what to do with a non-logical idxs matrix")
    }
  } else if (is.matrix(nrow)) {
    d <- dim(nrow)
    nrow <- d[1]
    ncol <- d[2]
  } else if (is.null(ncol)) {
    stop("Dimensions of matrix under-specified")
  }

  x <- (idxs-1) %% nrow + 1
  y <- (idxs-1) %/% nrow + 1
  data.frame(x=x, y=y)
}

sub2ind <- function(x, y, nrow, ncol=NULL) {
  ## Returns a linear index for the (x,y) coordinates passed in.
  if (is.matrix(x) || is.data.frame(x)) {
    stopifnot(ncol(x) == 2)
    if (!missing(y)) {
      if (missing(nrow)) {
        nrow <- y
      } else {
        ncol <- nrow
        nrow <- y
      }
    }
    y <- x[,2]
    x <- x[,1]
  }
  
  if (is.matrix(nrow)) {
    d <- dim(nrow)
    nrow <- d[1]
    ncol <- d[2]
  } else if (is.null(ncol)) {
    stop("Dimensions of matrix under-specified")
  }
  
  # Sanity check to ensure we got each var doing what it should be doing
  if (length(x) != length(y) || length(nrow) != 1 || length(ncol) != 1) {
    stop("I'm confused")
  }
  
  ((x - 1) + ((y - 1) * nrow)) + 1
}

matrix.power <- function(mat, n) {
  # test if mat is a square matrix
  # treat n < 0 and n = 0 -- this is left as an exercise
  # trap non-integer n and return an error
  # NOTE: Still not the most efficient
  if (n == 1) return(mat)
  result <- diag(1, ncol(mat))
  while (n > 0) {
    if (n %% 2 != 0) {
      result <- result %*% mat
      n <- n - 1
    }
    
    mat <- mat %*% mat
    n <- n / 2
  }
  return(result)
}
"%^%" <- matrix.power

ident <- function(what, as.sparse=FALSE) {
  if (is.matrix(what)) {
    if (nrow(what) != ncol(what)) {
      stop("Can't make identity from non-sqaure matrix")
    }
    what <- nrow(what)
  } else if (is.numeric(what)) {
    if (length(what) > 1) {
      what <- length(what)
    }
  } else {
    stop("Don't know what your `what` is")
  }
  
  if (as.sparse) {
    if (!libLoaded('Matrix')) library(Matrix)
    I <- Matrix(0, what, what, sparse=TRUE)
    diag(I) <- 1
  } else {
    I <- diag(nrow=what, ncol=what)
  }
  I
}
eye <- ident

replace.na <- function(M, which.dim=1, statistic=c('mean','median')) {
  which.dim <- match.dim(which.dim)
  if (which.dim == 2) M <- t(M)
  statf <- getFunction(match.arg(statistic))
  for (idx in 1:nrow(M)) {
    M[idx, is.na(M[idx,])] <- statf(M[idx,], na.rm=TRUE) 
  }
  if (which.dim == 2) M <- t(M)
  M
}
