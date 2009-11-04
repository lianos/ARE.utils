createMap <- function(name.vector, index=NULL, verbose=NULL) {
  # Create a name <-> index map from char.vector
  if (is.null(verbose)) {
    verbose <- getOption('verbose')
  }
  if (is.null(index)) {
    index <- seq_along(name.vector)
  } else {
    if (length(index) > length(name.vector)) {
      index <- index[1:length(name.vector)]
      if (verbose) {
        warning("Length of index longer than length of name.vector")
      }
    } else if (length(index) < length(name.vector)) {
      name.vector <- name.vector[1:length(index)]
      if (verbose) {
        warning("Length of name.vector longer than index, dropping some names")
      }
    }
  }
  
  names(index) <- name.vector
  index
}

trimVectors <- function(..., to=NULL) {
  ## Returns a list of vectors each of the same length
  ## (that of the shortest vector)
  args <- extractEmbeddedLists(list(...))
  if (is.null(to)) {
    to <- min(sapply(args, length))      # Trim lambdas to shortest
  } else {
    stopifnot(is.numeric(to) && length(to) == 1)
  }
  
  if (any(sapply(args, length) < to)) {
    padVectors(args, to=to)
  } else {
    lapply(args, '[', 1:to)
  }
}

padVectors <- function(..., to=NULL, pad.value=NA) {
  args <- extractEmbeddedLists(list(...))
  if (is.null(to)) {
    to <- max(sapply(args, length))
  } else {
    stopifnot(is.numeric(to) && length(to) == 1)
  }
  
  if (all(sapply(args, length) >= to)) {
    trimVectors(args, to=to)
  } else {
    lapply(args, function(arg) {
      if (length(arg) < to) {
        arg <- c(arg, rep(pad.value, to - length(arg)))
      }
      arg
    })
  }
}