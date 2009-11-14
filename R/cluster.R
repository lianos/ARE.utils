# leafs.dendrogram <- function(dd) {
#   at <- attributes(dd)
#   len <- length(dd)
#   
#   if (len == 1) {
#     show(at)
#     show(is.leaf(dd))
#     cat(at$label, "\n")
#   }
#   
#   for (i in 1:len) {
#     ret[i] <- leafs.dendrogram(dd[[i]])
#   }
# }

# if (!isGeneric("mycut")) { 
#   if (is.function("mycut")) { 
#     fun <- mycut
#   } else { 
#     fun <- function(x, h, ...) standardGeneric("mycut") 
#   } 
#   setGeneric("mycut",fun) 
# } 

# setMethod('mycut', 'dendrogram', function(x, h, ...) {
#   LOWER <- list()
#   X <- 1
# 
#   assignNodes <- function(subtree, h) {
#     if(!is.leaf(subtree)) {
#       if(!(K <- length(subtree))) stop("non-leaf subtree of length 0")
#       new.mem <- 0L
#       for (k in 1L:K) {
#         sub <- subtree[[k]]
#         if(attr(sub, "height") <= h) {
#           ## cut it, i.e. save to LOWER[] and make a leaf
#           at <- attributes(sub)
#           at$leaf <- TRUE
#           at$class <- NULL # drop it from leaf
#           at$x.member <- at$members
#           new.mem <- new.mem + (at$members <- 1L)
#           at$label <- at$label #paste("Branch", X)
#           subtree[[k]] <- X #paste("Branch", X)
#           attributes(subtree[[k]]) <- at
#           class(sub) <- "dendrogram"
#           LOWER[[X]] <<- sub
#           X <<- X+1
#         } else { ## don't cut up here, possibly its children:
#           subtree[[k]] <- assignNodes(sub, h)
#           new.mem <- new.mem + attr(subtree[[k]], "members")
#         }
#       }
#       ## re-count members:
#       attr(subtree,"x.member") <- attr(subtree,"members")
#       attr(subtree,"members") <- new.mem
#     }
#     subtree
#   } # assignNodes()
#   
#   list(upper = assignNodes(x, h), lower = LOWER)
# })

# mycut.dendrogram <- function(x, h, ...)
# {
#   LOWER <- list()
#   X <- 1
# 
#   assignNodes <- function(subtree, h) {
#     if(!is.leaf(subtree)) {
#       if(!(K <- length(subtree))) stop("non-leaf subtree of length 0")
#       new.mem <- 0L
#       for (k in 1L:K) {
#         sub <- subtree[[k]]
#         if(attr(sub, "height") <= h) {
#           ## cut it, i.e. save to LOWER[] and make a leaf
#           at <- attributes(sub)
#           show(at)
#           at$leaf <- TRUE
#           at$class <- NULL # drop it from leaf
#           at$x.member <- at$members
#           new.mem <- new.mem + (at$members <- 1L)
#           at$label <- paste("Branch", X)
#           subtree[[k]] <- X #paste("Branch", X)
#           attributes(subtree[[k]]) <- at
#           class(sub) <- "dendrogram"
#           LOWER[[X]] <<- sub
#           X <<- X+1
#         } else { ## don't cut up here, possibly its children:
#           subtree[[k]] <- assignNodes(sub, h)
#           new.mem <- new.mem + attr(subtree[[k]], "members")
#         }
#       }
#       ## re-count members:
#       attr(subtree,"x.member") <- attr(subtree,"members")
#       attr(subtree,"members") <- new.mem
#     }
#     subtree
#   } # assignNodes()
#   
#   list(upper = assignNodes(x, h), lower = LOWER)
# } ## cut.dendrogram()
