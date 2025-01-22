MF_cluster <- function(formula, whichweights = "random", num.trees = 500,
                       mtry = NULL, method = "REML", tau2 = NULL, ...,
                       v, df, id) {
  args <- match.call()[-1]
  dots <- list(...)
  if(!(num.trees%%2 == 0)){
    message("Conducting a clustered MetaForest analysis with an odd value of num.trees; num.trees has been rounded up to the nearest even number.")
    num.trees <- num.trees+1
  }
  y <- df[[1]]

  if(!class(id) %in% c("integer", "numeric", "factor")) stop("Please provide a valid \"study\" column name.")
  if ("case.weights" %in% names(args)) {
    stop("Error: Argument 'case.weights' not supported for clustered MetaForest.")
  }
  if ("holdout" %in% names(args)) {
    stop("Error: Argument 'holdout' not supported for clustered MetaForest.")
  }
  if ("importance" %in% names(args)) {
    if(!dots$importance %in% c("none", "permutation")){
      stop("Error: Argument 'importance' not supported for clustered MetaForest. Always set to 'permutation'.")
    }
  }
  if ("replace" %in% names(args)) {
    stop("Error: Argument 'replace' not supported for clustered MetaForest.")
  }

  if(is.null(tau2)){
    rma_before <- tryCatch({rma(yi = y, vi = v, method = method)}, error = function(e){
      warning("Error when attempting to estimate initial heterogeneity using metafor::rma using method ='", method, "'. Used DerSimonian-Laird method instead. See 'help(rma)' for possible remedies.", call. = FALSE)
      return(rma_dl(y = y, v = v))
    })

    tau2 <- rma_before$tau2
  } else {
    rma_before <- list(tau2 = tau2)
  }


  metaweights <- switch(whichweights,
                        unif = rep(1, length(y)),
                        fixed = (1/v),
                        random = 1/(v + tau2)
  )

  #Sample subsamples by cluster (study), calculate weights by subsample
  unique_studies <- unique(id)
  select_these <- sample.int(length(unique_studies), round(length(unique_studies)/2))
  subsample_studies <- list(sample1 = unique_studies[select_these],
                            sample2 = unique_studies[-select_these])

  res <- lapply(subsample_studies, function(samplestudies){
    ss_weights <- metaweights
    ss_weights[id %in% samplestudies] <- 0

    Args <- list(
      formula = formula,
      data = df,
      num.trees = num.trees/2,
      mtry = mtry,
      write.forest = TRUE,
      case.weights = (ss_weights / sum(ss_weights)) * length(which((id %in% samplestudies))),
      holdout = TRUE, ...)
    if(!"importance" %in% names(Args)){
      Args$importance <- "permutation"
    }

    do.call(ranger::ranger, Args)
  })
  names(res) <- c("rf1", "rf2")
  forest <- do.call(merge_mf_cluster, c(res, list(y = y)))

  ## Compute importance
  predicted <- forest$predictions
  residuals <- y - predicted

  rma_after <- tryCatch({rma(yi = residuals, vi = v, method = method)}, error = function(e){
    warning("Error when attempting to estimate residual heterogeneity using metafor::rma using method ='", method, "'. Used DerSimonian-Laird method instead. See 'help(rma)' for possible remedies.", call. = FALSE)
    return(rma_dl(y = residuals, v = v))
  })

  output <- list(forest = forest, rma_before = rma_before, rma_after = rma_after, data = df, vi = v, study = id, weights = metaweights)
  class(output) <- c("cluster_mf", "MetaForest")
  output
}
