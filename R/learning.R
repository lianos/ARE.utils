createFolds <- function(y, k=10, list=TRUE, returnTrain=FALSE) {
  ## Taken from caret:::createFolds function
  if (is.numeric(y)) {
    y <- cut(y, 
             quantile(y, probs = seq(0, 1, length = min(5, length(y)))), 
             include.lowest = TRUE)
  }
  
  if (k < length(y)) {
    # reset levels so that the possible levels and 
    # the levels in the vector are the same
    y <- factor(y)
    numInClass <- table(y)
    foldVector <- vector(mode = "integer", length(y))
    
    # For each class, balance the fold allocation as far 
    # as possible, then resample the remainder.
    # The final assignment of folds is also randomized. 
    for(i in 1:length(numInClass)) {
      # create a vector of integers from 1:k as many times as possible without 
      # going over the number of samples in the class. Note that if the number 
      # of samples in a class is less than k, nothing is producd here.
      seqVector <- rep(1:k, numInClass[i] %/% k)
      # add enough random integers to get  length(seqVector) == numInClass[i]
      if(numInClass[i] %% k > 0) seqVector <- c(seqVector, sample(1:k, numInClass[i] %% k))
      # shuffle the integers for fold assignment and assign to this classes's data
      foldVector[which(y == dimnames(numInClass)$y[i])] <- sample(seqVector)
   }
  } else {
    foldVector <- seq(along = y)
  }
  
  if (list) {
    out <- split(seq(along = y), foldVector)
    if (returnTrain) {
      out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
    }
  } else {
    out <- foldVector
  }
  
  out
}

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
