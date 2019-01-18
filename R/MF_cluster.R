MF_cluster <- function(formula, whichweights = "random", num.trees = 500,
                       mtry = NULL, method = "REML", tau2 = NULL, ...,
                       v, df, id) {
    args <- match.call()[-1]
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
      stop("Error: Argument 'importance' not supported for clustered MetaForest. Always set to 'permutation'.")
    }
    if ("replace" %in% names(args)) {
      stop("Error: Argument 'replace' not supported for clustered MetaForest.")
    }

    rma_before <- metafor::rma(yi = y, vi = v, method = method)

    if(is.null(tau2)) tau2 <- rma_before$tau2

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

      ranger(
        formula = formula,
        data = df,
        num.trees = num.trees/2,
        mtry = mtry,
        importance = "permutation",
        write.forest = TRUE,
        case.weights = (ss_weights / sum(ss_weights)) * length(which((id %in% samplestudies))),
        holdout = TRUE, ...)
      })
    names(res) <- c("rf1", "rf2")

    ## Compute importance
    predicted <- res$rf1$predictions
    predicted[is.na(predicted)] <- res$rf2$predictions[!is.na(res$rf2$predictions)]
    residuals <- y - predicted

    rma_after <- rma(yi = residuals, vi = v, method = method)
    forest <- list(predictions = predicted,
                   num.trees = num.trees,
                   num.independent.variables = res$rf1$num.independent.variables,
                   mtry = res$rf1$mtry,
                   min.node.size = res$rf1$min.node.size,
                   variable.importance = (res$rf1$variable.importance + res$rf2$variable.importance)/2,
                   prediction.error = mean(c(res$rf1$prediction.error, res$rf2$prediction.error)),
                   forest = list(dependent.varID = res$rf1$forest$dependent.varID,
                                 num.trees = num.trees,
                                 child.nodeIDs = c(rbind(res$rf1$forest$child.nodeIDs, res$rf2$forest$child.nodeIDs)),
                                 split.varIDs = c(rbind(res$rf1$forest$split.varIDs, res$rf2$forest$split.varIDs)),
                                 split.values = c(rbind(res$rf1$forest$split.values, res$rf2$forest$split.values)),
                                 is.ordered = res$rf1$forest$is.ordered,
                                 independent.variable.names = res$rf1$forest$independent.variable.names,
                                 treetype = res$rf1$forest$treetype),
                   #rf2 = res$rf2,
                   splitrule = res$rf1$splitrule,
                   treetype = res$rf1$treetype,
                   r.squared = 1 - mean(c(res$rf1$prediction.error, res$rf2$prediction.error)) / var(y),
                   call = formula,
                   importance.mode = "permutation",
                   num.samples = res$rf1$num.samples,
                   cluster_forests = res
                   )
    class(forest) <- "ranger"
    class(forest$forest) <- "ranger.forest"
    output <- list(forest = forest, rma_before = rma_before, rma_after = rma_after, data = df, vi = v, study = id, weights = metaweights)
    class(output) <- c("cluster_mf", "MetaForest")
    output
}
