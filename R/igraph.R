"
Functions to add to, or override default versions in igraph 0.5.2-2
"
##############################################################################
# New Functionality
##############################################################################

normalize.edge.weights <- function(graph, attr='weight', norm.to=NULL) {
  ## RETURNS A NEW GRAPH with normalized edge weights.
  ## 
  ## By default, the edge weights are normalized to 1.
  ## norm.by : set to the value desired as the max weight
  stopifnot(is(graph, 'igraph'))
  stopifnot(attr %in% list.edge.attributes(graph))
  
  if (!is.null(attr)) {
    if (is.null(norm.to)) {
      norm.to <- max(get.edge.attribute(graph, attr))
    }
    if (norm.to == 0) {
      stop("0 weights here.")
    }
    if (norm.to != 1) {
      weights <- get.edge.attribute(graph, attr)
      graph <- set.edge.attribute(graph, attr, value=weights / norm.to)
    }
  }
  
  graph
}

graph.rename.vertices <- function(graph, map, attr='name') {
  # Renames vertices on the graph using the `map`.
  # 
  # If any name in the graph isn't found in the map, the current name
  # is left as is.
  # 
  # Parameters
  # ----------
  # map  : data.frame/matrix that maps current node names (column 1)
  #        to the new/desired node names (column 2)
  # attr : the name of the "name" attribute on the graph vertices
  node.names <- get.vertex.attribute(graph, attr)
  xref <- match(node.names, map[,1])
  if (!all(is.na(xref))) {
    new.names <- map[xref,2]
    missing <- which(is.na(new.names))
    new.names[missing] <- node.names[missing]
    graph <- set.vertex.attribute(graph, attr, value=new.names)
  }
  graph
}

.graph.randomized.check <- function(graph, rewired) {
  if (no.clusters(graph) != no.clusters(rewired)) {
    cat("  WARNING: Number of clusters have changed\n")
  }
  
  # Check edge overlap
  adj.o <- get.adjacency(graph)
  adj.r <- get.adjacency(rewired)
  if (sum(adj.o) != sum(adj.r)) {
    cat("  WARNING: New graph doesn't have same number of edges")
  }
  cat("  Overlap in edges: ", sum(adj.o & adj.r) / sum(adj.o), "%\n", sep='')
  cat("  Are graphs isomorphic?", graph.isomorphic(graph, rewired), "\n")
}

graph.randomize.edges <- function(graph, method=c('shuffle', 'degree'),
                                  do.check=FALSE) {
  # Randomizes the connections in the graph keeping the same degree/node
  # 
  # NOTE: All graph attributes will be stripped, it's your job to use
  #       the *.annotate.* functions to put the ones back that you want.
  # warning("Using degree.sequence.game(graph, method='vl')")
  mode <- ifelse(is.directed(graph), 'directed', 'upper')
  if (mode == 'directed') {
    stop("I don't think this works for directed graphs -- that's for the next paper")
  }
  
  method <- match.arg(method)
  if (method == 'degree') {
    game.method <- ifelse(no.clusters(graph) == 1, 'vl', 'simple')
    rand <- degree.sequence.game(degree(graph), method=game.method)
    rand <- simplify(rand)
    V(rand)$name <- V(graph)$name
  } else if (method == 'shuffle'){
    # Keeps the exact same structure of graph, but shuffles the node IDs
    # NOTE: You will have to make a new expression matrix from this graph
    # (maybe this will make a better randomized graph)
    rand <- graph
    V(rand)$name <- sample(V(graph)$name)
    expr <- get.vertex.attribute(graph, 'expr')
    if (!is.null(expr)) {
      V(rand)$expr <- expr[match(V(rand)$name, V(graph)$name)]
    }
  }
  
  if (do.check) {
    cat("Running tests on rewired graph\n")
    .graph.randomized.check(graph, rand)
  }
  
  return(rand)
}

nuke.islands <- function(graph) {
  # removes vertices from the graph that have no edges -- are islands
  nuke.ids <- which(degree(graph) == 0) - 1
  if (length(nuke.ids) > 0) {
    delete.vertices(graph, nuke.ids)
  } else {
    graph
  }
}

