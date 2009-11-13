###############################################################################
# Cytoscape
###############################################################################

serialize.cytoscape.node.attributes <- function(graph, attributes, basename,
                                                attribute.name=NULL, default=NULL,
                                                formatter=NULL, path='.') {
  ## ``graph`` can be missing
  ## node.map can be:
  ##   * a vector whose ``name``s are the nodes on the graph and value to be
  ##     the value to output
  ##   * a string, in which case it picks off the appropriate value from
  ##     V(graph)$WHATEVER and uses that
  ##
  ## If dealing with double/floating values, set ``formater='%.5f`` or similar
  if (is.igraph(graph)) {
    if (is.character(attributes) && length(attributes) == 1) {
      attribute.name <- attributes
      attributes <- get.vertex.attribute(graph, attribute.name)
      names(attributes) <- V(graph)$name
    } else {
      if (is.null(attribute.name)) {
        stop("Need to know the attribute name in order to serialize to file")
      }
      if (!all(V(graph)$name %in% names(attributes))) {
        if (is.null(default)) {
          stop("No default set and some nodes don't have values in ``attributes``")
        }
        missed <- setdiff(V(graph)$name %in% names(attributes))
        rest <- rep(default, length(missed))
        names(rest) <- missed
        attributes <- c(attributes, rest)
      }
    }
  } else if (is.vector(graph)) {
    if (!missing(attributes)) {
      stop("What the heck is this?")
    }
    attributes <- graph
  }
  
  ## Make sure that our shoelaces are tied
  if (missing(attributes)) {
    stop("You're attributes: they're missing.")
  }
  if (length(names(attributes)) != length(attributes)) {
    stop("Your attribute vector needs node names")
  }
  if (is.null(attribute.name)) {
    stop("Need to know the attribute name in order to serialize to file")
  }
  
  if (is.null(formatter)) {
    if (is.numeric(attributes)) {
      formatter <- '%.5f'
    } else {
      formatter <- '%s'
    }
  }
  
  outname <- paste(basename, 'nodes', attribute.name, 'txt', sep='.')
  outfile <- file(file.path(path, outname), 'w')
  on.exit(close(outfile))
  
  ## R likes dot.name.separators, Java dosn't like dot_name_separators
  cat(gsub('.', '_', attribute.name, fixed=TRUE), '\n', file=outfile)
  cat(paste(names(attributes), sprintf(formatter, attributes), sep=' = '),
      sep="\n", file=outfile)
}

serialize.cytoscape <- function(graph, basename, path='.', node.attributes=FALSE,
                                edge.attributes=FALSE) {
  ## Cytoscape will take two files:
  ##   1. `basename`.edges.txt : table of edges (w/ their attributes)
  ##   2. `basename.nodes.txct : table of nodes w/ attributes
  ## 
  ## (node|edge).attributes can be:
  ##  * TRUE   : all attributes
  ##  * FALSE  : no attributes
  ##  * character vector : a subset of the attributes
  if (missing(basename)) {
    basename <- paste('graph', vcount(graph), 'nodes', sep='.')
    warning("Graph basename not given, using ", basename)
  }
  
  if (is.logical(node.attributes)) {
    if (node.attributes) {
      node.attributes <- list.vertex.attributes(graph)
    } else {
      node.attributes <- character(0)
    }
  }
  if (is.logical(edge.attributes)) {
    if (edge.attributes) {
      edge.attributes <- list.edge.attributes(graph)
    } else {
      edge.attributes <- character(0)
    }
  }
  
  ## Edges
  edge.file <- file.path(path, sprintf("%s.edges.txt", basename))
  edges <- get.edgelist(graph)
  edge.attributes <- data.frame(lapply(edge.attributes, function(name) {
    get.edge.attribute(graph, name)
  }))
  if (length(edge.attributes) != 0) {
    edges <- cbind(edges, edge.attribs)
  }
  
  ## add single nodes
  colnames(edges) <- paste("V", 1:ncol(edges), sep=".")
  islands <- degree(graph)
  if (any(islands == 0)) {
    islands <- names(islands)[islands == 0]
    more <- data.frame(islands)
    for (i in 2:ncol(edges)) {
      more[[i]] <- ""
    }
    colnames(more) <- colnames(edges)
    edges <- rbind(edges, more)
  }
  
  write.table(edges, quote=FALSE, sep="\t", row.names=FALSE,
              col.names=FALSE, file=edge.file)
  
  ## Nodes
  for (node.attribute in node.attributes) {
    serialize.cytoscape.node.attributes(graph, node.attribute, basename,
                                        path=path)
  }
  
  invisible(list(node.attributes=node.attributes, edge.attributes=edges,
                 edge.file=edge.file))
}

load.cytoscape.edgelist <- function(base.name, path='.', directed=FALSE) {
  # The edge list doesn't have to be a balanced data.frame! The left
  # column can just be node names w/o a right column (in the case of islands)
  files <- dir(path, pattern=base.name, full.names=TRUE)
  if (length(files) == 0) {
    stop("No edge file found")
  }
  
  cat("  Reading edge file", files[grep('edges', files)], "\n")
  edges.list <- readLines(files[grep('edges', files)])
  edges <- strsplit(edges.list, "\\t")
  edge.m <- do.call(rbind, Filter(function(x) length(x) == 2, edges))
  islands <- unlist(Filter(function(x) length(x) == 1, edges))
  graph <- graph.edgelist(edge.m, directed=directed)
  graph <- add.vertices(graph, length(islands), attr=list(name=islands))
  
  nodes <- V(graph)$name
  
  for (node.file in files[grep('nodes', files)]) {
    # Add node attributes
    
    node.info <- strip.whitespace(readLines(node.file))
    attrib <- node.info[1]
    data <- node.info[-1]
    val.names <- sapply(strsplit(data, " = "), "[[", 1)
    vals <- sapply(strsplit(data, " = "), "[[", 2)
    
    if (!is.na(as.numeric(vals[1]))) {
      vals <- as.numeric(vals)
      data.vector <- numeric(length(nodes))
    } else {
      data.vector <- character(length(nodes))
    }
    names(data.vector) <- nodes
    data.vector[val.names] <- vals
    graph <- set.vertex.attribute(graph, attrib, value=data.vector)
  }
  graph
}
