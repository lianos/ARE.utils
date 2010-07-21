accuracy <- function(labels, predictions, balanced=FALSE, label.dim='rows') {
  # Taken from kernlab:::.classAgreement
  if (is.table(labels)) {
    agreement <- labels
  } else {
    agreement <- table(labels, predictions, dnn=c('labels', 'predictions'))
  }
  
  agreement <- as.matrix(agreement)
  label.dim <- match.dim(label.dim)
  if (label.dim != 1) {
    stop("Really, put the labels in the first dimension!")
  }
  
  if (nrow(agreement) != ncol(agreement)) {
    # I had to add this since balanced=TRUE would bomb if the # of
    # unique predictions didn't equal the number of labels: this might
    # happen when the model predicts everything to be of the same class
    all.labels <- union(rownames(agreement), colnames(agreement))
    new.table <- matrix(0, nrow=nrow(agreement), ncol=nrow(agreement),
                        dimnames=list(all.labels, all.labels))
    common.rows <- intersect(rownames(new.table), rownames(agreement))
    common.cols <- intersect(colnames(new.table), colnames(agreement))
    new.table[common.rows, common.cols] <- agreement[common.rows, common.cols]
    agreement <- new.table
  }
  
  N <- sum(agreement)
  if (!is.null(dimnames(agreement))) {
    lev <- intersect(colnames(agreement), rownames(agreement))
    agreement <- agreement[lev,lev,drop=FALSE]
  } else {
    m <- min(dim(agreement))
    agreement <- agreement[1:m,1:m,drop=FALSE]
  }
  
  if (balanced) {
    classAcc <- diag(agreement) / apply(agreement, label.dim, sum)
    sum(classAcc) / dim(agreement)[label.dim]
  } else {
    sum(diag(agreement)) / N
  }
}
