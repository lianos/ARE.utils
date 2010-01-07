trim.data <- function(x, qtile=0.01, na.rm=TRUE, trim.to.na=FALSE) {
  # Use this when trying to balance out heatmaps of data, eg:
  #   library(gplots)
  #   heatmap.2(trim.data(expression), ...)
  qtile <- min(qtile, 1-qtile)
  lo <- quantile(x, qtile, na.rm=na.rm)
  hi <- quantile(x, 1-qtile, na.rm=na.rm)
  if (trim.to.na) {
    x[x < lo] <- NA
    x[x > hi] <- NA
  } else {
    x[x < lo] <- lo
    x[x > hi] <- hi
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
