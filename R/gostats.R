# Wrapper for functions for doing gene ontology enrichment tests using
# GOstats
# (Originally extracted from spelunker/medusa.R)
# 
# Help with GO in R:
#   http://www.economia.unimi.it/projects/marray/2007/material/day4/Lecture7.pdf

# library(foreach)
# library(GOstats)

# org.Hs.eg.db
do.gostats <- function(gene.sets, universe, conditional=FALSE, p.value=0.01,
                       ontology=c('BP', 'MF', 'CC'), annotation='org.Sc.sgd',
                       testDirection='over') {
  # for graphs, genes=V(graph)$id
  library(GOstats)
  library(foreach)
  
  if (missing(universe)) {
    stop("Gimme the universe of genes we picked from")
  }
  ontology <- match.arg(ontology)
  
  do.hyperG <- function(gene.list) {
    params <- new("GOHyperGParams", geneIds=gene.list,
                  universeGeneIds=universe, annotation=annotation,
                  ontology=ontology, pvalueCutoff=p.value,
                  conditional=conditional, testDirection=testDirection)
    # paramsCond <- params
    # conditional(paramsCond)
    hyperGTest(params)
  }
  
  if (is.list(gene.sets)) {
    GO <- foreach (gset=gene.sets, .packages=c('GOstats')) %dopar% {
      do.hyperG(gset)
    }
  } else {
    GO <- do.hyperG(gene.sets)
  }
  
  GO
}
