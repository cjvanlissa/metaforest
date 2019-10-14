#' @importFrom metafor rma
MF <- function(formula, whichweights = "random",
                       num.trees = 500, mtry = NULL, method = "REML",
                       tau2 = NULL, ..., v, df) {
    y <- df[[1]]

    rma_before <- tryCatch({rma(yi = y, vi = v, method = method)}, error = function(e){
      warning("Error when attempting to estimate initial heterogeneity using metafor::rma using method ='", method, "'. Used method = 'DL' instead. See 'help(rma)' for possible remedies.", call. = FALSE)
      return(rma(yi = y, vi = v, method = "DL"))
    })

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
    # Temporary fix to deal with https://github.com/imbs-hl/ranger/issues/201
    # that is, forests sometimes return NaN predictions. Just replace them with
    # predictions on "new data":
    if(anyNA(mf$predictions)){
      which_na <- is.na(mf$predictions)
      pred_df <- df[which_na, -1, drop = FALSE]
      mf$predictions[which_na] <- predict(mf, pred_df)$predictions
      warning("Some OOB predictions were NaN, and were replaced with predictions across all trees.")
    }
    # End of fix
    residuals <- y - mf$predictions

    rma_after <- tryCatch({rma(yi = residuals, vi = v, method = method)}, error = function(e){
      warning("Error when attempting to estimate residual heterogeneity using metafor::rma using method ='", method, "'. Used method = 'DL' instead. See 'help(rma)' for possible remedies.", call. = FALSE)
      return(rma(yi = residuals, vi = v, method = "DL"))
    })
    output <- list(forest = mf, rma_before = rma_before, rma_after = rma_after, data = df, vi = v, weights = metaweights)
    class(output) <- "MetaForest"
    output
}
