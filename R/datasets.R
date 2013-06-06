trim.data <- function(x, qtile=0.01, trim.to=c("quantile", "NA", "remove"),
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

create.densities <- function(..., along=1, density.params=list(), na.rm=TRUE,
                             dens.fun=c('density', 'ecdf')) {
  along <- match.dim(along)
  stopifnot(along %in% c(1,2))
  dens.fun <- match.arg(dens.fun)
  #dfun <- getFunction(dens.fun, generic=FALSE)
  dfun <- match.fun(dens.fun)
  if (is.null(density.params$na.rm) && dens.fun == 'density') {
    density.params$na.rm <- na.rm
  }
  if (dens.fun == 'ecdf') {
    density.params <- NULL
  }
  data <- list(...)
  if (length(data) == 1) {
    data <- data[[1]]
  }
  if (is.data.frame(data)) {
    data <- as.list(data)
  }

  if (is.list(data)) {
    data <- lapply(data, function(item) {
      if (is(item, dens.fun)) {
        ans <- item
      } else {
        if (length(item) < 2) {
          warning("Trying to create a density out of < 2 items",
                  immediate.=TRUE)
          ans <- NULL
        } else {
          if (dens.fun == 'ecdf') {
            ans <- ecdf(item)
          } else {
            ans <- do.call(density, c(x=list(item), density.params))
          }
        }
      }
      ans
    })
  } else if (is.matrix(data)) {
    n.samples <- dim(data)[along]
    if (along == 2) data <- t(data)
    data <- lapply(seq(n.samples), function (idx) {
      if (dens.fun == 'ecdf') {
        ans <- ecdf(data[idx,])
      } else {
        ans <- do.call(density, c(x=list(data[idx,]), density.params))
      }
    })
  }

  stopifnot(all(sapply(data, is, dens.fun)))
  data
}

create.ecdfs <- function(..., along=1, density.params=list(), na.rm=TRUE) {
  create.densities(..., along=along, density.params=density.params,
                   na.rm=na.rm, dens.fun='ecdf')
}
