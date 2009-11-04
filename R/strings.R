strip.slashes <- function(char.vector) {
  slashes <- grep("/", char.vector)
  if (length(slashes) > 0) {
    char.vector[slashes] <- unlist(lapply(strsplit(char.vector[slashes], "/", fixed=T), 
                                  function(x) sub('[[:space:]]+$', '', x[1])))
  }
  return(char.vector)
}

strip.whitespace <- function(from, internal=FALSE) {
  if (internal) {
    from <- gsub('[[:space:]]+', '', from)
  } else {
    from <- sub('^[[:space:]]+', '', from) # strip from head
    from <- sub('[[:space:]]+$', '', from) # strip from tail
  }
  return(from)
}

