rconfig <- function(file='default.conf') {
  ## Parses config files of the same format that Python's ConfigParser
  ## chews on. Returns values in a list-of-list fashion
  ## Taken from: http://code.google.com/p/miscell 
  header.pattern <- '^\\s*\\[(.*)\\]\\s*$'
  lines <- grep('\\S+', readLines(file), value=TRUE)
  headers <- grep(header.pattern, lines, perl=TRUE)
  blocks <- mapply(':', headers+1, c(headers[-1]-1, length(lines)))
  structure(
    lapply(blocks, function(block) {
      entries <- strsplit(lines[block], split='\\s*=\\s*')
      structure(lapply(entries, '[', 2),
                names=sapply(entries, '[', 1))
    }),
    names=sub(header.pattern, '\\1', lines[headers], perl=TRUE))
}
