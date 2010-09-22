## Functions for GO analysis in bioconductor

##' Return the GO annotations of type \code{ontology} for gene(s) identified
##' by \code{id}.
##'
##' @param id The gene id (entrez or orf) of the genes of interest
##' @param ontolgoy Which ontology GO terms are wanted for
##' @param annotation The annotation db for the chip/organism
##' @return A list of GO terms. Names of list elements is `id`. If no GO terms
##' are found for the given gene, then the character vector here is empty.
getGOAnnotations <- function(id, ontology=c('BP', 'MF', 'CC'),
                                annotation='org.Sc.sgd') {
  ontology <- match.arg(ontology)
  map <- getAnnMap('GO', annotation)
  gos <- mget(id, map, ifnotfound=NA)
  universe <- lapply(gos, function(dat) {
    if (!is.list(dat)) return(character())
    these <- lapply(dat, function(anno) {
      if (anno$Ontology == ontology) anno$GOID else NULL
    })
    these <- unlist(these)
    these <- these[!is.null(these)]
    these
  })
  universe
}
