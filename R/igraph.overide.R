"
These functions are defined in igraph 0.5.2, but offer an augmented
implementation.
"

head.igraph.es <- function (x, n=6, ...) {
  stopifnot(length(n) == 1)
  n <- if (n < 0) max(length(x) + n, 0) else min(n, length(x))
  x[seq_len(n)-1]
}
head.igraph.vs <- head.igraph.es

#' Returns the degree of the graph
#' 
#' This function is an enhanced version from the igraph library which can
#' calculate weighted degrees. It also adds the names of the vertices
#' to the elements vector returned.
#' 
#' @param graph The igraph object
#' @param v The ids of vertices of which the degree will be calculated.
#' @param mode Character string, "out" for out-degree, "in" for in-degree
#'    or "total" for the sum of the two. For undirected graphs this argument
#'    is ignored.
#' @param loops Logical; whether the loop edges are also counted.
#' @param weighted Logical; whether to calculate the weighted degree for 
#'    each node, which is the sum of the weight of all of its edges.
#' @return A named vector of the degree at each node.
degree <- function(graph, v=V(graph), mode=c("all", "out", "in", "total"),
                   loops=TRUE, weighted=FALSE) {
  stopifnot(inherits(graph, 'igraph'))
  if (is.logical(weighted) && !weighted) {
    deg <- igraph::degree(graph, v=v, mode=mode, loops=loops)
  } else {
    ## The weight of a node is the sum of its edge weights
    ## d(u) = \sum_{u~v} w(u,v)
    attr <- if (is.logical(weighted)) 'weight' else weighted
    if (!attr %in% list.edge.attributes(graph)) {
      stop("Unknown weight attribute:", attr)
    }
    adj.w <- get.adjacency(graph, attr=attr)
    if (is.directed(graph)) {
      if (mode == 'out') {
        # adj.w[lower.tri(adj.w)] <- 0
        deg <- rowSums(adj.w)
      } else if (mode == 'in'){
        # adj.w[upper.tri(adj.w)] <- 0
        deg <- colSums(adj.w)
      }
    } else {
      # Undirected graph
      deg <- rowSums(adj.w)
    }
  }
  names(deg) <- V(graph)$name
  deg
}

#' Graph laplacian
#' 
#' The laplacian of the graph. Overriden from the igraph library in order
#' to return weighted laplacian, as well as the S-decompsed (\code{L=SS\'})
#' version.
#'
#' @param graph The input igraph
#' @param normalized Logical; whether to calculate the normalized laplacian
#' @param weighted Logical; whether to calculate the weighted laplacian
#' @param S.decomposed Logical; whether to calculate the S matrix
#'    (\code{L=SS\'})
graph.laplacian <- function(graph, normalized=FALSE, weighted=FALSE,
                            S.decomposed=FALSE, .normalize.weights=TRUE) {
  if (is.logical(weighted) && weighted &&
      !'weight' %in% list.edge.attributes(graph)) {
    warning("No 'weight' attribute in graph -- fetching unweighted laplacian")
    weighted <- FALSE
  }
  
  if (S.decomposed) {
    L <- .laplacian.decomposed(graph, weighted=weighted,
                               .normalize.weights=.normalize.weights)
  } else if (is.logical(weighted) && !weighted) {
    L <- igraph::graph.laplacian(graph, normalized=normalized)
    dimnames(L) <- list(V(graph)$name, V(graph)$name)
  } else {
    ## 1 - w(u,v)/d_u              if u == v (=1 if no self loops)
    ## -w(u,v) / sqrt(d_u * d_v)   if u ~ v  
    ## 0                           otherwise
    if (!inherits(graph, 'igraph')) {
      stop("Not a graph object")
    }
    attr <- if (is.logical(weighted)) 'weight' else weighted
    if (.normalize.weights) {
      graph <- normalize.edge.weights(graph, attr=attr)
    }
    deg.w <- degree(graph, weighted=attr)
    adj.w <- get.adjacency(graph, attr=attr)
    norm <- sqrt(tcrossprod(deg.w)) # tcrossprod == outer-product
    L <- -1 * adj.w / norm
    diag(L) <- 1 - diag(adj.w) / diag(norm)
    dimnames(L) <- list(V(graph)$name, V(graph)$name)
  }
  L
}

.laplacian.decomposed <- function(graph, weighted=TRUE,
                                  .normalize.weights=TRUE, do.check=TRUE) {
  # Returns S in l = SS' when L is the normalized Laplacian
  # 
  # The weighted/normalized laplacian is nonnegative definite, and can be
  # can be written as L = SS'; where:
  # 
  #   S_{p x m}
  #     p = # of vertices
  #     m = # of edges
  #   Each column corresponding to edge e={u,v} has entry:
  #     sqrt(w(u,v)) / sqrt(d_u)        in the row matching u
  #     sqrt(-w(u,v)) / sqrt(d_v)       in the row matching v
  # 
  # Columns of S are in the same order as returned from get.edgelist(graph)
  stopifnot(is(graph, 'igraph'))
  do.weight <- !is.logical(weighted) || weighted
  attr <- if (is.logical(weighted)) 'weight' else weighted
  
  if (!do.weight) {
    graph <- set.edge.attribute(graph, attr, value=1)
  } else {
    if (.normalize.weights) {
      graph <- normalize.edge.weights(graph, attr=attr)
    }
  }
  
  S <- matrix(0, nrow=vcount(graph), ncol=ecount(graph))
  rownames(S) <- V(graph)$name
  deg.sqrt <- sqrt(degree(graph, weighted=weighted))
  names(deg.sqrt) <- V(graph)$name
  edgelist <- get.edgelist(graph)
  eweights.sqrt <- sqrt(get.edge.attribute(graph, attr))
  
  for (i in seq(nrow(edgelist))) {
    v1 <- edgelist[i,1]
    v2 <- edgelist[i,2]
    w <- eweights.sqrt[i]
    S[v1,i] <- w / deg.sqrt[v1]
    S[v2,i] <- -w / deg.sqrt[v2]
  }
  
  S
}
