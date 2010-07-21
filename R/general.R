dopar <- function(what, backend=c('doMC')) {
  backend <- match.arg(backend)
  if (!libLoaded(backend)) library(backend, character.only=TRUE)
  
  if (missing(what)) {
    cat("Using", getDoParName(), "backend with", getDoParWorkers(), 
        ifelse(getDoParWorkers() == 1, "worker", "workers"), "\n")
  } else {
    if (is.logical(what)) {
      if (what) what <- 2 else registerDoSEQ()
    }
    if (is.numeric(what)) {
      if (what < 2) {
        registerDoSEQ()
      } else {
        register <- paste('register', toupper(substring(backend, 1, 1)),
                          substring(backend, 2), sep="")
        do.call(register, list(what))
      }
    }
  }
  invisible(getDoParWorkers() > 1)
}

':=' <- function(lhs, rhs) {
  # Taken from: http://code.google.com/p/miscell/source/browse/rvalues/rvalues.r
  # Who the hell knew you could do this?
  #   c(a,b,c) := c(1,2,3)
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1) {
    lhs <- lhs[-1]
  }
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula')) {
    rhs <- list(rhs)
  }
  if (length(lhs) > length(rhs)) {
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  }
  for (i in 1:length(lhs)) {
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  }
  return(invisible(NULL))
}

swap <- function(a,b) {
  frame <- parent.frame()
  tmp <- a
  do.call(`=`, list(substitute(a), substitute(b)), envir=frame)
  do.call(`=`, list(substitute(b), substitute(tmp)), envir=frame)
  return(invisible(NULL))
}

filter.na <- function(items) {
  ## filters NA vars out of list
  # Filter(function(x) !is.na(x), items)
  items[!is.na(items)]
}

match.dim <- function(margin) {
  # You can specify the margin by the index used to reference it, or by
  # it's name. This function returns which dimension to "run-over":
  #   - 1 is rows
  #   - 2 is cols
  # (This function is vectorized, tho I'm not sure why)
  rows <- which(margin %in% c('r', 'row', 'rows', 1))
  cols <- which(margin %in% c('c', 'col', 'cols', 'column', 'columns', 2))
  result <- numeric(length(margin))
  result[rows] <- 1
  result[cols] <- 2
  result[-c(rows,cols)] <- NA
  result
}

dim.names <- function(object, along=1, dnames=NULL) {
  # Returns a character vector, will be length 0 if we don't want
  # to use any dim.names, othwerwise returns a vector that is the
  # same length as the dim(object)[along]
  along <- match.dim(along)
  getnames <- if (along == 1) rownames else colnames
  res.names <- character()
  if (!is.null(dnames)) {
    if (is.logical(dnames)) {
      res.names <- if (dnames) getnames(object) else character()
    } else {
      if (!is.character(dnames)) {
        stop("What row.names are you giving me?")
      } else if (length(dnames) != dim(object)[along]) {
        stop("Illegal number of dim.names supplied [!= dim(data.frame)[along]]")
      } else {
        res.names <- dnames
      }
    }
  }
  res.names
}


whos <- function(pattern='*', show.memory=TRUE, ENV=.GlobalEnv) {
  if (!is.character(pattern)) {
    # Maybe we pass in a list
    things <- names(pattern)
    if (is.null(things)) {
      stop("Don't want to deal with un-names things in a list")
    }
    ENV <- new.env()
    for (var in things) {
      assign(var, pattern[[var]], envir=ENV)
    }
  } else {
    things <- Filter(function(x) {
      thing <- try(get(x, env=ENV), silent=TRUE)
      !is(thing, 'try-error') && !is.function(thing)
    }, ls(parent.frame(), pattern=utils:::glob2rx(pattern)))
  }
  
  if (length(things) == 0) {
    cat("No items in workspace\n");
  } else {
    classes <- sapply(things, function(name) class(get(name, env=ENV))[1],
                      USE.NAMES=FALSE)
    types <- sapply(1:length(classes), function (idx) {
      obj <- get(things[idx], env=ENV)
      type <- typeof(obj)
      if (classes[idx] %in% c(type, 'data.frame')) {
        classes[idx]
      } else {
        paste(classes[idx], type, sep='/')
      }
    }, USE.NAMES=FALSE)
    
    sizes <- sapply(1:length(classes), function (idx) {
      obj <- get(things[idx], env=ENV)
      if (classes[idx] %in% c('matrix', 'data.frame')) {
        paste(dim(obj), collapse="x")
      } else {
        length(obj)
      }
    }, USE.NAMES=FALSE)
    
    d <- data.frame(var=things, type=types, size=sizes, stringsAsFactors=FALSE)
    
    if (show.memory) {
      memory <- sapply(1:length(classes), function (idx) {
        obj <- get(things[idx], env=ENV)
        size <- object.size(obj) / 1024
        if (size < 1024) {
          sprintf("%.2fK", size)
        } else {
          size <- size / 1024
          if (size > 1024) {
            sprintf("%.2fG", size / 1024)
          } else {
            sprintf("%.2fM", size)
          }
        }
      })
      d$memory <- memory
    }
    
    show(d)
    invisible(d)
  }
}

###############################################################################
# which.duplicated
if (!isGeneric("which.duplicated")) { 
  if (is.function("which.duplicated")) { 
    fun <- which.duplicated
  } else { 
    fun <- function(object, ...) standardGeneric("which.duplicated") 
  } 
  setGeneric("which.duplicated",fun) 
} 

if (!existsMethod('which.duplicated', 'matrix')) {
  setMethod('which.duplicated', 'matrix', function(object, ...) {
    ## returns the index of the first vector (from the top) that is a duplicate
    ## of the current row
    params <- list(...)
    if (is.null(params$MARGIN)) params$MARGIN <- 1
    if (!(params$MARGIN %in% 1:2)) {
      stop("which.duplicated not implemented for margins other than 1 or 2")
    }
    result <- rep(-1, dim(object)[params$MARGIN])
    check.me <- which(duplicated(object, MARGIN=params$MARGIN))
    if (params$MARGIN == 2) {
      object <- t(object)
    }
    for (i in check.me) {
      for (j in 1:i) {
        if (all(object[i,!is.na(object[i,])] == object[j,!is.na(object[j,])])) {
          result[i] <- j
          break()
        }
      }
    }
    result
  })
}
###############################################################################

wideScreen <- function(how.wide=Sys.getenv("COLUMNS")) {
  # Set R to print to the same width as the terminal is set to
  # Taken from: http://onertipaday.blogspot.com/2008/12/tips-from-jason.html
  if (!is.character(how.wide) && !is.numeric(how.wide)) {
    how.wide <- 150
  }
  options(width=as.integer(how.wide))
}
