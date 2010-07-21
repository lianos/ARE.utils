extractEmbeddedLists <- function(the.list) {
  ## Lifts embedded lists out of a list, and returns you 1 list with
  ## Warning : recursion! 
  parsed <- list()
  if (is.list(the.list)) {
    for (arg in the.list) {
      parsed <- c(parsed, extractEmbeddedLists(arg))
    }
  } else {
    parsed <- list(the.list)
  }
  parsed
}

##' Combines two lists together. Any keys that are shared between both lists
##' are merged (using \code{c()})
##'
##' NOTE: Keep version from GenomeAnnotations::utils the most up to date
##' and copy any changes here.
combine.list <- function(list.1, list.2, as.intersect=FALSE, ...) {
  names.1 <- names(list.1)
  names.2 <- names(list.2)
  
  if (!is.null(names.1) && !is.null(names.2)) {
    new.names <- union(names.1, names.2)
    shared.names <- intersect(names.1, names.2)
    from.1 <- setdiff(names.1, shared.names)
    from.2 <- setdiff(names.2, shared.names)
    
    combined <- vector('list', length(new.names))
    names(combined) <- new.names
    
    for (name in shared.names) {
      if (as.intersect) {
        combined[[name]] <- unique(c(list.1[[name]], list.2[[name]]))
      } else {
        combined[[name]] <- c(list.1[[name]], list.2[[name]])
      }
    }

    for (name in from.1) {
      combined[[name]] <- list.1[[name]]
    }
    for (name in from.2) {
      combined[[name]] <- list.2[[name]]
    }
  } else {
    combined <- c(list.1, list.2)
  }
  
  combined
}
