trim.data <- function(x, qtile=0.01, trim.to=c("NA", "quantile", "remove"),
                      side=c('both', 'upper', 'lower')) {
  trim.to <- match.arg(trim.to)
  side <- match.arg(side)
  if (side == 'both') {
    side <- c('upper', 'lower')
  }
  
  qtile <- min(qtile, 1-qtile, na.rm=TRUE)
  lo <- quantile(x, qtile, na.rm=TRUE)
  hi <- quantile(x, 1-qtile, na.rm=TRUE)

  if ('lower' %in% side) {
    x[x < lo] <- if (trim.to == "quantile") lo else NA
  }
  if ('upper' %in% side) {
    x[x > hi] <- if (trim.to == "quantile") hi else NA
  }

  if (trim.to == "remove") {
    x <- x[!is.na(x)]
  }
  
  x
}

create.densities <- function(..., along=1, density.params=list(), na.rm=TRUE) {
  along <- match.dim(along)
  stopifnot(along %in% c(1,2))
  if (is.null(density.params$na.rm)) {
    density.params$na.rm <- na.rm
  }
  
  data <- list(...)
  if (length(data) == 1) {
    data <- data[[1]]
  }
  if (is.data.frame(data)) data <- as.matrix(data)
  if (is.list(data)) {
    data <- lapply(data, function(item) {
      if (is(data, 'density')) {
        item
      } else {
        do.call(density, c(list(item), density.params))
      }
    })
  } else if (is.matrix(data)) {
    n.samples <- dim(data)[along]
    if (along == 2) data <- t(data)
    data <- lapply(seq(n.samples), function (idx) {
      do.call(density, c(list(data[idx,]), density.params))
    })
  }
  data
}
