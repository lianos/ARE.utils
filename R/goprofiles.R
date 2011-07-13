##' Plot two profiles and order the terms by ones that have the most difference
##' in enrichment
## Plot go profiles with biggest differences
plotProfilesMostDiff <- function(aProf, aTitle="Functional Profiles",
                                 anOnto=NULL, percentage=FALSE, HORIZVERT=TRUE,
                                 legendText=NULL, colores=c('white', 'red'),
                                 labelWidth=25, n=NULL, ...) {
  freq <- t(as.matrix(aProf[, 3:4]))
  if (is.null(n)) {
    n <- ncol(freq)
  }
  desc <- as.character(aProf$Description)
  opt <- par(mar = c(4, 12, 4, 4), xpd = TRUE, cex.axis = 0.01)
  if (percentage) {
    numGenes1 <- attr(aProf, "numGenes1")
    numGenes2 <- attr(aProf, "numGenes2")
    if (!is.null(numGenes1) & !(numGenes1 == 0) &
        !is.null(numGenes2) & !(numGenes2 == 0)) {
      freq[1, ] <- round((freq[1, ]/numGenes1 * 100), 2)
    }
    freq[2, ] <- round((freq[2, ]/numGenes2 * 100), 2)
    xlim <- c(0, 100)
  } else {
    xlim <- c(0, max(freq))
  }

  inorder <- order(abs(freq[1,] - freq[2,]), decreasing=TRUE)
  inorder <- head(inorder, n)

  bp <- barplot(freq[, inorder], horiz=HORIZVERT, beside=TRUE,
                legend.text=legendText, col=colores, xlim=xlim, ...)

  text(freq[, inorder], round(bp, 1), freq[, inorder], pos = 4, cex = 0.6)
  axis(1, cex.axis=0.8, labels = seq(0, 100, by=20),
       at=seq(0, 100, by=20))
  axis(2, at=(bp[1, ] + bp[2, ])/2, cex.axis = 0.6, las=2,
       labels=goProfiles:::shortName(desc[inorder], labelWidth))

  if (is.null(anOnto)) {
    title(aTitle)
  } else {
    title(main=paste(aTitle, ". ", anOnto, " ontology", sep = ""))
  }
  par(opt)
}
