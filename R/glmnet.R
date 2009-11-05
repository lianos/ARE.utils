"
Generic glmnet code for doing cross validation.
"
.getLambdaStats <- function(lambdas, method=c('pad', 'trim')) {
  ## Returns lambda.mean and lambda.error from a list of lambdas per fold
  method <- match.arg(method)
  if (method == 'pad') {
    lambdas <- padVectors(lambdas)
  } else {
    lambdas <- trimVectors(lambdas)
  }
  
  lambda.stack <- do.call(rbind, lambdas)
  lambda.mean <- colMeans(lambda.stack, na.rm=TRUE)
  lambda.error <- apply(lambda.stack, 2, var, na.rm=TRUE) / length(lambdas)
  lambda.error <- sqrt(lambda.error)
  lambda.error[is.na(lambda.error)] <- 0 # na when only 1 val
  
  list(mean=lambda.mean, error=lambda.error)
}

plot.cv.lambda.glmnet <- function(lambdas, plot.se=TRUE, plot.title='') {
  ## lambdas is a list of lambda-vectors used over each fold.
  
  ## TODO: Highlight lambdas that only appear in 1 fold. The lambda.error
  ##       vector will have NA in these positions.
  lambda <- .getLambdaStats(lambdas)
  errs <- add.error.bars(seq(lambda$mean), lambda$mean, lambda$error,
                         plot.it=FALSE)
  err.range <- range(unlist(errs), na.rm=TRUE)
  plot(seq(lambda$mean), lambda$mean, type='b', 
       main=paste("glmnet: Lambda values over folds", plot.title, sep='\n'),
       ylim=c(floor(err.range[1]), err.range[2]),
       pch=19)
  if (plot.se) {
    add.error.bars(seq(lambda$mean), upper=errs$upper, lower=errs$lower)
  }
}

plot.cv.error.glmnet <- function(lambdas, eval.metric, plot.se=TRUE,
                                 plot.title='') {
  ## lambdas     : list of lambda-vectors over folds
  ## eval.metric : list of the eval metric at each lambda point over folds
  lambda <- .getLambdaStats(lambdas, method='trim')
  eval.metric <- trimVectors(eval.metric, to=length(lambda$mean))
  eval.stack <- do.call(rbind, eval.metric)
  eval.error <- apply(eval.stack, 2, var, na.rm=TRUE) / length(eval.metric)
  eval.error <- sqrt(eval.error)
  eval.mean <- colMeans(eval.stack, na.rm=TRUE)
  
  errs <- add.error.bars(lambda$mean, eval.mean, eval.error, plot.it=FALSE)
  err.range <- range(unlist(errs), na.rm=TRUE)
  
  cols <- grDevices::colorRampPalette(c('black','red'))(length(lambda$mean))
  plot(lambda$mean, eval.mean, ylim=c(floor(err.range[1]), err.range[2]),
       main=paste("glmnet: CV Error", plot.title, sep='\n'),
       col=cols, pch=19, type='b')
  
  # min.error.idx <- which.min(eval.metric)
  # min.error <- MSE[min.error.idx]
  # text(par()$usr[2], min.error-0.1, as.character(min.error))
  # abline(h=min.error, col='red', lty='dashed')
  
  if (plot.se) {
    add.error.bars(lambda$mean, lower=errs$lower, upper=errs$upper, col=cols)
  }
}

###############################################################################
# Model evaulation functions for cv.glmenet
###############################################################################

.mse <- function(predicted, Y) {
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  stopifnot(nrow(predicted) == length(Y))
  colMeans((predicted - Y)^2)
}

.r2 <- function(predicted, Y) {
  ## http://en.wikipedia.org/wiki/Coefficient_of_determination
  ## R2 "as explained variance" := 1 - SSerr / SStot
  ## 
  ## compares the explained variance (variance of the model's predictions)
  ## with the total variance (of the data)
  ##
  ## NOTE: This formulation matches exactly with glmnet()$dev when used
  ##       calculated on training data and preds on the data model was
  ##       trained on
  ## TODO: This gives negative values for R2 on held out data! 
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  stopifnot(nrow(predicted) == length(Y))
  ss.err <- colSums((predicted - Y)^2)
  ss.tot <- sum((Y - mean(Y))^2)
  1 - (ss.err / ss.tot)
}

.spearman <- function(predicted, Y) {
  if (!is.matrix(predicted)) {
    predicted <- matrix(predicted, ncol=1)
  }
  stopifnot(nrow(predicted) == length(Y))
  if (sd(Y) == 0) {
    stop("Standard deviation of target variables are 0 : screws the spearman test")
  }
  apply(predicted, 2, function(col) {
    if (sd(col) == 0) {
      NA
    } else {
      cor.test(col, Y, method='spearman')$estimate
    }
  })
}

cv.glmnet <- function(X, Y, alpha=.75, K=10, all.folds=NULL, nlambda=100,
                      standardize=TRUE, eval.by=c('mse', 'r2', 'spearman'),
                      verbose=TRUE,  do.plot=FALSE, plot.se=TRUE,
                      plot.lambda=FALSE, multiplot=TRUE,
                      plot.title=NULL) {
  # A super-cross-validation wrapper, inspired by lars::cv.lars
  # 
  # Parameters
  # ----------
  # X          : The feature matrix (rows are obversations, cols are features)
  # Y          : Target/output vector
  # alpha      : The alpha value in glment
  #              lasso   : alpha=1
  #              ridge   : alpha=0
  #              elastic : 0 < alpha < 1
  # K          : Number of CV folds
  # all.folds  : a list of indices to holdout in each fold (use of K and this is
  #              mutually exclusive)
  # nlambda    : Number of lambda values to compute the regularization path over.
  #              Defaults to 100.
  # eval.by    : The metric to use when evaluating the "best" model for each fold.
  # do.plot    : Plots the MSE over the values of lambda along the
  #              regularization path. If this is set to FALSE, no plots are 
  #              drawn, no matter the setting of other plot.* arguments.
  # plot.se    : Plots the standard error of MSE (calc'd from the folds)
  # plot.lambda: Plots the lambdas used in the regularization path, too.
  # 
  # Returns
  # -------
  #   model       : glmnet trained over all of the data
  #   cv          : (K x nlambda) matrix with MSE for each value of lambda
  #               : lambda values are set in colnames(cv)
  #   cv.error    : nlambda-length vector of SEMs CV error at each lambda
  #   models      : list(K) of model trained at fold
  #   coefs       : list(K) of coefs extracted using best lambda for the fold
  #   best.lambda : list(K) optimal lambda value in fold
  #   lambda.mean : vector(K) of avg. lambda-regularization path
  #   all.folds   : list(K) of examples held out per fold
  # 
  # Usage
  # -----
  # cv.result <- cv.glmnet(X,Y)
  # 
  # Notes
  # -----
  # The lambdas used in each fold can be slightly different, perhaps it's good
  # to specify the lambdas after all folds are finished to plot something similar
  # to the cv.lars function
  # 
  # Results from glment
  #   $beta    : matrix of coeficiens
  #   $dim     : dimension of coefficient matrix
  #   $lambda  : sequence of lambda values used
  #   $df      : # of nonzero coefs for each lambda
  #   $dev     : fraction of (null) deviance explained (for "elnet", this is the R-square)
  # 
  # glmnet::predict returns a matrix of predictions over all values of lambda
  if (do.plot && plot.lambda && multiplot) {
    opar <- par(mfrow=c(1,2))
    on.exit(par(opar))
  }
  
  if (is.null(all.folds)) {
    all.folds <- caret::createFolds(Y, K)
  } else {
    K <- length(all.folds)
  }
  
  ## Determine metric to evaluate model by
  eval.by <- match.arg(eval.by)
  if (eval.by == 'mse') {
    ## Evaluate by Mean Squared Error
    evalF <- .mse
    best <- min
    which.best <- which.min
  } else if (eval.by == 'r2') {
    ## Evalute by fraction of explained varaiance
    evalF <- .r2
    best <- max
    which.best <- which.max
  } else if (eval.by == 'spearman') {
    evalF <- .spearman
    best <- max
    which.best <- which.max
  }
  
  all.scores <- list() # Vectors per fold holding the mse/r2/etc per lambda
  all.lambdas <- list()
  best.scores <- numeric(length(all.folds))
  best.lambdas <- numeric(length(all.folds))
  
  for (i in seq(all.folds)) {
    omit <- all.folds[[i]]
    model <- glmnet(X[-omit, , drop=FALSE], Y[-omit], alpha=alpha,
                    standardize=standardize)
    preds <- predict(model, X[omit, , drop=FALSE])
    escore <- evalF(preds, Y[omit])
    
    best.scores[i] <- best(escore)
    best.lambdas[i] <- model$lambda[which.best(escore)]
    
    ## to regenerate the plot methods
    all.scores[[i]] <- escore
    all.lambdas[[i]] <- model$lambda
  }
  
  if (do.plot && plot.lambda) {
    ## Show the lambda path
    plot.cv.lambda.glmnet(all.lambdas, plot.se=plot.se, plot.title=plot.title)
  }

  if (do.plot) {
    if (plot.lambda && !multiplot) {
      dev.new()
    }
    plot.cv.error.glmnet(all.lambdas, all.scores, plot.se=plot.se,
                         plot.title=paste(plot.title, sprintf("[%s]", eval.by)))
  }
  
  retval <- list(eval.by=eval.by, best.scores=best.scores,
                 best.lambdas=best.lambdas, lambdas.per.fold=all.lambdas,
                 scores.per.fold=all.scores)
  class(retval) <- c('cv.glmnet', 'list')
  invisible(retval)
}
