# Functions to dump different types of R objects into a useful
# format for displaying in the moin wiki

moin.table <- function(object, ...) {
  stop(paste("moinTable is a generic function that dispathces on class type.",
             "Don't know what to to do with object type:", class(object)[1]))
}

if (is.function('hyperGTest')) {
  # This should only be loaded if the GOstats library has been loaded
  setMethod('moin.table', 'GOHyperGResult', function(object, ...) {
    # params <- c(list(object), list(...))
    params <- list(...)
    if (is.null(params$htmlLinks)) {
      params$htmlLinks <- TRUE
    }
    params$row.names <- FALSE
    # header <- paste("||<rowstyle=\"background-color: #FFFFE0;\">'''GOBPID'''",
    #                 "|| '''Pvalue''' || '''OddsRatio''' || '''ExpCount''' ||",
    #                 "'''Count''' || '''Size''' || '''Term''' ||\n", sep=''),  
    # df <- do.call('summary')
    df <- summary(object)
    do.call(moin.table, c(list(df), params))
    # moin.table(df, ...)
  })
}

setMethod('moin.table', 'data.frame', function(object, ...) {
  params <- list(...)
  file <- params$file
  header.bgcolor <- params$header.bgcolor
  row.names <- params$row.names
  col.names <- params$col.names
  row.names.bgcolor <- params$row.names.bgcolor
  
  if (is.null(file)) {
    file <- ''
  }
  if (is.null(header.bgcolor)) {
    header.bgcolor <- '#FFFFE0'
  }
  if (is.null(row.names.bgcolor)) {
    row.names.bgcolor <- header.bgcolor
  }
  if (is.null(col.names)) {
    col.names <- TRUE
  }
  
  row.names <- dim.names(object, 1, row.names)
  col.names <- dim.names(object, 2, col.names)
  
  if (length(col.names) != 0) {
    header <- sprintf("||<rowstyle=\"background-color: %s\">", header.bgcolor)
    cnames <- sprintf("'''%s'''", col.names)
    if (length(row.names) != 0) {
      header <- paste(header, "||", sep='')
    }
    header <- paste(header, paste(cnames, collapse="||"), "||", sep="")
    cat(sprintf("%s\n", header), file=file)
  }
  
  if (nrow(object) == 0) {
    cat(sprintf("||<-%d :>'''No Results'''||\n'", ncol(object)), file=file)
  } else {
    for (i in 1:nrow(object)) {
      row.label <- NULL
      if (length(row.names) != 0) {
        row.label <- paste(sprintf('<bgcolor="%s">', row.names.bgcolor),
                           "'''", row.names[i], "'''||", sep="")
      }
      cat(paste("||", row.label,
                paste(object[i,], collapse="||"), "||\n", sep=""),
          file=file)
    }
  }
})

setMethod('moin.table', 'matrix', function(object, ...) {
  moin.table(as.data.frame(object), ...)
})