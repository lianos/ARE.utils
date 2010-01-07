###############################################################################
## Functions to ease upgrading to new R and re-installing all your favorite
## packages.

installedPackages <- function(to="~/R.packages.dump.txt") {
  writeLines(installed.packages()[,1], con=to)
}

reloadPackages <- function(from="~/R.packages.dump.txt") {
  source("http://bioconductor.org/biocLite.R")
  packages <- readLines(from)
  biocLite(packages)
}

libLoaded <- function(lib.name) {
  tolower(lib.name) %in% tolower(gsub("package:", "", search()))
}

sourceDir <- function(path='.', trace = TRUE, ...) {
  v <- options()$verbose
  options(verbose=FALSE)
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
  options(verbose=v)
}

# packages.dump <- function(dump.file) {
#   tmp <- installed.packages()
#   installed.old <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
#   save(installed.old, file=dump.file)
# }
# 
# packages.load.R <- function(dump.file) {
#   # Attempts to install all packages that were present in old install
#   # into the new install. All non-cran package will return a warning
#   # that they are not available, but will allow for the remainder
#   # of packages to be installed
#   load(dump.file)
#   tmp <- installed.packages()
#   installed.current <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
#   missing.packages <- setdiff(installed.old, installed.current)
#   install.packages(missing.packages)
# }
# 
# packages.load.bioc <- function(dump.file) {
#   # assumes that whatever packages are missing from old install and 
#   # current install are BioC packages
#   load(dump.file)
#   tmp <- installed.packages()
#   installed.current <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
#   missing.packages <- setdiff(installed.old, installed.current)
#   source("http://bioconductor.org/biocLite.R")
#   biocLite(missing.packages)
# }
