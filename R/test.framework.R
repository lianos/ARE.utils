"
=== A Poorman's Testing Framework ===
[Note to self: You should learn how to use a 'real' R testing framework]

Create a  tests/ subdirectory in your project's R directory and have 1 test
file per 'source unit'.

Each test file should define a .TEST.INDEX that defines the 'evironment' of
the test. If tests are run serially, then only one test file will have control
of the .TEST.INDEX and you can ignore passing this value into the doTest and
fail functions in your testing code.

Example: Testing some graph implementation [graph.test.R]

    .TEST.INDEX <- 'graph.test'
    initTestEnvironment(.TEST.INDEX)
    
    conn <- get.connected.test.graph(mode='undirected', weighted=TRUE)
    conn.w1 <- set.edge.attribute(conn, 'weight', value=1)
    
    doTest({
      if (!all(degree(conn) == degree.weighted(conn.w1))) {
        fail('Weighted degree calcuation failed.')
      }
    })
    
    testReport()
"

if (!exists('.ARE.TEST.ENV')) {
  # Not sure how namespaces/environments work in packages, so ... this is dumb
  .ARE.TEST.ENV <- new.env()
  assign('TEST.FAIL', list(), envir=.ARE.TEST.ENV)
  assign('TEST.COUNT', list(), envir=.ARE.TEST.ENV)
}

initTestEnvironment <- function(test.index=NULL) {
  envir <- parent.frame()
  if (is.null(test.index)) {
    test.index <- get('.TEST.INDEX', envir=envir)
  }
  tc <- get('TEST.COUNT', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tf <- get('TEST.FAIL', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tc[[test.index]] <- 0
  tf[[test.index]] <- 0
  assign('TEST.COUNT', tc, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  assign('TEST.FAIL', tf, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
}

doTest <- function(expr, test.index=NULL, name=NULL, verbose=TRUE) {
  envir <- parent.frame()
  if (is.null(test.index)) {
    test.index <- get('.TEST.INDEX', envir=envir)
  }
  if (verbose && !is.null(name)) {
    cat("  Testing:", name, "\n")
  }
  
  eval(expr, envir=envir)
  # .TEST.COUNT[[test.index]] <<- .TEST.COUNT[[test.index]] + 1
  # eval(TEST.COUNT[[test.index]] <- TEST.COUNT[[test.index]] + 1, envir=.TEST.ENV)
  tc <- get('TEST.COUNT', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tc[[test.index]] <- tc[[test.index]] + 1
  assign('TEST.COUNT', tc, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
}

fail <- function(msg, var.name=NULL, envir=NULL) {
  cat("    FAIL:", msg, '\n')
  # env <- globalenv()
  # assign(var.name, get(var.name, envir=env) + 1, envir=env)
  if (is.null(var.name)) {
    if (is.null(envir)) {
      envir <- globalenv()
    }
    var.name <- get('.TEST.INDEX', envir=envir)
  }
  # .TEST.STATUS[[var.name]] <<- .TEST.STATUS[[var.name]] + 1
  tf <- get('TEST.FAIL', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tf[[var.name]] <- tf[[var.name]] + 1
  assign('TEST.FAIL', ts, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
}

failReport <- function(var.name) {
  tf <- get('TEST.FAIL', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  cat("  Total Errors for", var.name, ':', tf[[var.name]], "\n")
}

testReport <- function(var.name=NULL) {
  envir <- parent.frame()
  if (is.null(var.name)) {
    var.name <- get('.TEST.INDEX', envir=envir)
  }
  tc <- get('TEST.COUNT', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  cat("\n\n")
  cat("====================================================\n")
  cat("Test Report for:", var.name, "\n")
  cat("====================================================\n")
  cat("  Total tests for", var.name, ':', tc[[var.name]], "\n")
  failReport(var.name)
}

runAllTests <- function(path='.') {
  test.files <- list.files(path, pattern="ztest\\..+\\.R", full.names=T)
  for (file in test.files) {
    cat("Running test file:", basename(file), "\n")
    source(file)
    cat("\n")
  }
}


