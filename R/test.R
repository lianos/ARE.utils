# Comparison / Testing functions
almost.equal <- function(x, y, tolerance=.Machine$double.eps^0.5,
                         na.rm=TRUE) {
  # You might want to look at all.equal
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  abs(x - y) < tolerance
}
'%~%' <- function(x, y) almost.equal(x, y)

upper.tri.equal <- function(M1, M2, tolerance=.Machine$double.eps^0.5) {
  return(all(dim(M1) == dim(M2))
         && all(almost.equal(diag(M1), diag(M2), tolerance=tolerance))
         && all(almost.equal(M1[upper.tri(M1)], M2[upper.tri(M2)], tolerance=tolerance)))
}

