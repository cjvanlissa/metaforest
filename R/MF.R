MF <- function(formula, data, vi = "vi", whichweights = "random",
                       num.trees = 500, mtry = NULL, method = "REML",
                       tau2 = NULL, ..., v, df) {
    y <- df[[1]]
    rma_before <- metafor::rma(yi = y, vi = v, method = method)

    if(is.null(tau2)) tau2 <- rma_before$tau2

    metaweights <- switch(whichweights,
                          unif = rep(1, length(y)),
                          fixed = (1/v),
                          random = 1/(v + tau2)
    )
    metaweights <- (metaweights/sum(metaweights)) * length(y)

    mf <- ranger::ranger(formula = as.formula(formula),
                         data = df,
                         num.trees = num.trees,
                         mtry = mtry,
                         importance = "permutation",
                         write.forest = TRUE,
                         case.weights = metaweights, ...)
    mf$call <- formula

    residuals <- y - mf$predictions
    rma_after <- metafor::rma(yi = residuals, vi = v, method = method)

    output <- list(forest = mf, rma_before = rma_before, rma_after = rma_after, data = df, vi = v, weights = metaweights)
    class(output) <- "MetaForest"
    output
}
