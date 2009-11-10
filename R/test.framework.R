"
A Poorman's Testing Framework
-----------------------------

[Note to self: You should learn how to use a 'real' R testing framework]

Create a  tests/ subdirectory in your project's R directory and have 1 test
file per 'source unit'.

Each test file should define a .TEST.INDEX that defines the 'evironment' of
the test. If tests are run serially, then only one test file will have control
of the .TEST.INDEX and you can ignore passing this value into the doTest and
fail functions in your testing code.

Example
-------

graph.test.R [testing file]::

    .TEST.INDEX <- 'graph.test'
    initTestEnvironment(.TEST.INDEX)
    
    conn <- get.connected.test.graph(mode='undirected', weighted=TRUE)
    conn.w1 <- set.edge.attribute(conn, 'weight', value=1)
    
    doTest(name='Weighted Degree', {
      if (!all(degree(conn) == degree.weighted(conn.w1))) {
        fail('Weighted degree calcuation failed.')
      }
    })
    
    testReport()
  
Running graph.test.R::
  
    == Testing: Weighted Degree 
    
    ====================================================
    Test Report for: graph.test 
    ----------------------------------------------------
      Total tests RUN for graph.test : 1 
      Total tests FAILED for graph.test : 0 
    ====================================================
    
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
  tcount <- get('TEST.COUNT', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tfail <- get('TEST.FAIL', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tcount[[test.index]] <- 0
  tfail[[test.index]] <- 0
  assign('TEST.COUNT', tcount, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  assign('TEST.FAIL', tfail, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
}

doTest <- function(expr, name=NULL, test.index=NULL, verbose=TRUE) {
  envir <- parent.frame()
  if (is.null(test.index)) {
    test.index <- get('.TEST.INDEX', envir=envir)
  }
  if (verbose && !is.null(name)) {
    cat("== Testing:", name, "\n")
  }
  
  eval(expr, envir=envir)
  # .TEST.COUNT[[test.index]] <<- .TEST.COUNT[[test.index]] + 1
  # eval(TEST.COUNT[[test.index]] <- TEST.COUNT[[test.index]] + 1, envir=.TEST.ENV)
  tcount <- get('TEST.COUNT', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tcount[[test.index]] <- tcount[[test.index]] + 1
  assign('TEST.COUNT', tcount, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
}

fail <- function(msg, var.name=NULL, envir=NULL) {
  cat("   FAIL:", msg, '\n')
  # env <- globalenv()
  # assign(var.name, get(var.name, envir=env) + 1, envir=env)
  if (is.null(var.name)) {
    if (is.null(envir)) {
      envir <- globalenv()
    }
    var.name <- get('.TEST.INDEX', envir=envir)
  }
  # .TEST.STATUS[[var.name]] <<- .TEST.STATUS[[var.name]] + 1
  tfail <- get('TEST.FAIL', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tfail[[var.name]] <- tfail[[var.name]] + 1
  assign('TEST.FAIL', tfail, envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
}

testReport <- function(var.name=NULL) {
  envir <- parent.frame()
  if (is.null(var.name)) {
    var.name <- get('.TEST.INDEX', envir=envir)
  }
  
  tcount <- get('TEST.COUNT', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  tfail <- get('TEST.FAIL', envir=getAnywhere('.ARE.TEST.ENV')$objs[[1]])
  
  cat("\n")
  
  if (tolower(var.name) != 'all') {
    cat("====================================================\n")
    cat("Test Report for:", var.name, "\n")
    cat("----------------------------------------------------\n")
    cat("  Total tests RUN for", var.name, '   :', tcount[[var.name]], "\n")
    cat("  Total tests FAILED for", var.name, ':', tfail[[var.name]], "\n\n")
  } else {
    tests.run <- sum(unlist(tcount))
    tests.fail <- sum(unlist(tfail))
    cat("||||||||||||||||||||||||||||||||||||||||||||||||||||\n")
    cat("||               Final Test Report                ||\n")
    cat("|| ---------------------------------------------- ||\n")
    cat("||                                                ||\n")
    cat(format(sprintf("|| Total Tests Run    : %d", tests.run), width=50), "||\n", sep="")
    cat(format(sprintf("|| Total Tests Failed : %d", tests.fail), width=50),"||\n", sep="")
    cat("||                                                ||\n")
    cat("||||||||||||||||||||||||||||||||||||||||||||||||||||\n")
  }
}

runTests <- function(..., path='tests') {
  args <- list(...)
  if (length(args) > 0) {
    test.files <- paste(path, unlist(args), sep="/")
  } else {
    test.files <- list.files(path, pattern=glob2rx("*.test.R"), full.names=TRUE)
  }
  for (file in test.files) {
    cat("/////////////////////////////////////////// ")
    cat("Test File:", basename(file), "\n")
    source(file)
    cat("\n")
  }
  if (length(test.files) > 1) {
    testReport('all')
  }
}

