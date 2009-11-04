e <- attach(NULL, name="tools:ARE.utils")
assign('add.util', function(name, FN) {
  # Got this trick from the add.fn function in
  # /Applications/R.app/Contents/Resources/GUI-tools.R
  e2 <- attach(NULL, name="tools:ARE.utils")
  assign(name, FN, e2)
  environment(e2[[name]]) <- e2
}, e)
environment(e[['add.util']]) <- e
rm(e)

add.util('include', function(..., utils.path=Sys.getenv('AREutils'), verbose=FALSE) {
  # utility function to be able to source my util files on each machine                                                                                            
  if (is.null(utils.path) || utils.path == "") {
    stop(paste("Set ARE.utils environment variable to /path/to/ARE.utils/R",
               "directory to use this library via the `include` command"))
  }
  
  files <- list(...)
  if (length(files) == 1 && files[[1]] == 'utils') {
    include('general', 'datasets', 'matrix', 'stats', 'test')
    invisible(return(NULL))
  }
  
  which.file <- unlist(lapply(files, function (fname) {
    if (!(substring(fname, nchar(fname)) %in% c('r', 'R'))) {
      paste(fname, 'R', sep='.')
    } else {
      fname
    }
  }))
  
  fpath <- file.path(utils.path, which.file)
  finfo <- file.info(fpath)
  util.files <- dir(utils.path, pattern="\\.*R")
  
  for (i in 1:nrow(finfo)) {
    be.verbose <- verbose
    picked.file <- NA
    if (!is.na(finfo$size[i])) {
      picked.file <- fpath[i]
    } else {
      ## file not found -- will search for it by:                                                                                                                  
      ##  - lopping off the *.R                                                                                                                                    
      ##  - removing a trailing "s" (if it's there)                                                                                                                
      ##  - returning file with the name that has the highest "match score"
      ##    which is (length of match) / (length of file)
      be.verbose <- TRUE
      ## Remove the .R (and optional trailing "s") from the file name
      the.file <- substring(which.file[i], 1, nchar(which.file[i])-2)
      if (substring(the.file, nchar(the.file)) == 's') {
        the.file <- substring(the.file, 1, nchar(the.file)-1)
      }
      matches <- gregexpr(the.file, util.files)
      match.score <- 0
      for (j in seq(matches)) {
        if (matches[[j]][1] != -1) {
          ms <- attr(matches[[j]], 'match.length') / nchar(util.files[j])
          if (ms > match.score) {
            picked.file <- file.path(utils.path, util.files[j])
            match.score <- ms
          }
        }
      }
    }
    
    if (is.na(picked.file)) {
      cat("Unknown file to include:", fpath[i], '\n')
    } else {
      if (be.verbose) {
        cat("Including file: ", basename(picked.file), '\n')
      }
      source(picked.file)
    }
  }
})
