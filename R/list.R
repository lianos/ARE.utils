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
