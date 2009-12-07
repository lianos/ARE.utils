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

string.startswith <- function(subject, what, strip=TRUE, ignore.case=TRUE) {
  if (nchar(subject) == 0 || nchar(what) == 0) {
    return(FALSE)
  }
  if (strip) {
    subject <- strip.whitespace(subject)
    what <- strip.whitespace(what)
  }
  if (ignore.case) {
    subject <- tolower(subject)
    what <- tolower(what)
  }
  substring(subject, 1, nchar(what)) == what
}

string.endswith <- function(subject, what, strip=TRUE, ignore.case=TRUE) {
  if (nchar(subject) == 0 || nchar(what) == 0) {
    return(FALSE)
  }
  if (strip) {
    subject <- strip.whitespace(subject)
    what <- strip.whitespace(what)
  }
  if (ignore.case) {
    subject <- tolower(subject)
    what <- tolower(what)
  }
  substring(subject, nchar(subject) - nchar(what) + 1) == what
}