#' Conduct a MetaForest analysis to explore heterogeneity in meta-analytic data.
#'
#' MetaForest uses a weighted random forest to explore heterogeneity in
#' meta-analytic data. MetaForest is a wrapper for \link[ranger]{ranger}
#' (Wright & Ziegler, 2015). As input, MetaForest takes the study effect sizes
#' and their variances (these can be computed, for example, using the
#' \link[metafor]{metafor} package), as well as the moderators that are to be
#' included in the model. By default, MetaForest uses random-effects weights,
#' and estimates the between-studies variance using a restricted
#' maximum-likelihood estimator. However, it may be beneficial to first conduct
#' an unweighted MetaForest, and then use the estimated residual heterogeneity
#' from this model as the estimate of \code{tau2} for a random-effects weighted
#' MetaForest.
#' @param formula Formula. Specify a formula for the MetaForest model, for
#' example, \code{yi ~ .} to predict the outcome \code{yi} from all moderators
#' in the data.
#' @param data Data.frame. Provide a data.frame containing the effect size,
#' moderators, and the variance of the effect size.
#' Defaults to 100.
#' @param vi Character. Specify the name of the column in the \code{data} that
#' contains the variances of the effect sizes. This column will be removed from
#' the data prior to analysis. Defaults to \code{"vi"}.
#' @param whichweights Character. Indicate what time of weights are required.
#' A random-effects MetaForest is grown by specifying \code{whichweights =
#' "random"}. A fixed-effects MetaForest is grown by specifying
#' \code{whichweights = "fixed"}. An unweighted MetaForest is grown by
#' specifying \code{whichweights = "unif"}. Defaults to \code{"random"}.
#' @param num.trees Atomic integer. Specify the number of trees in the forest.
#' Defaults to 500.
#' @param mtry Atomic integer. Number of candidate moderators available for each
#' split. Defaults to the square root of the number moderators (rounded down).
#' @param method Character. Specify the method by which to estimate the residual
#' variance. Can be set to one of the following: "DL", "HE", "SJ", "ML", "REML",
#' "EB", "HS", or "GENQ". Default is "REML". See the \link[metafor]{metafor} package
#' for more information about these estimators.
#' @param tau2 Numeric. Specify a predetermined value for the residual
#' heterogeneity. Entering a value here supersedes the estimated tau2 value.
#' Defaults to NULL.
#' @param ... Additional arguments are passed directly to \link[ranger]{ranger}.
#' It is recommended not to use additional arguments.
#' @return List of length 3. The "forest" element of this list is an object of
#' class "ranger", containing the results of the random forests analysis. The
#' "rma_before" element is an object of class "rma.uni", containing the results
#' of a random-effects meta-analysis on the raw data, without moderators. The
#' "rma_after" element is an object of class "rma.uni", containing the results
#' of a random-effects meta-analysis on the residual heterogeneity, or the
#' difference between the effect sizes predicted by MetaForest and the observed
#' effect sizes.
#' @import stats
#' @import ranger
#' @import metafor
#' @export
#' @examples
#' #Example 1:
#' #Simulate data with a univariate linear model
#' set.seed(42)
#' data <- SimulateSMD()
#' #Conduct unweighted MetaForest analysis
#' mf.unif <- MetaForest(formula = yi ~ ., data = data$training,
#'                       whichweights = "unif", method = "DL")
#' #Print model
#' mf.unif
#' #Conduct random-effects weighted MetaForest analysis
#' mf.random <- MetaForest(formula = yi ~ ., data = data$training,
#'                         whichweights = "random", method = "DL",
#'                         tau2 = 0.0116)
#' #Print summary
#' summary(mf.random)
#'
#' #Example 2: Real data from metafor
#' #Load and clean data
#' data <- dat.bangertdrowns2004
#' data[, c(4:12)] <- apply(data[ , c(4:12)], 2, function(x){
#'   x[is.na(x)] <- median(x, na.rm = TRUE)
#'   x})
#' data$subject <- factor(data$subject)
#' data$yi <- as.numeric(data$yi)
#' #Conduct MetaForest analysis
#' mf.bd2004 <- MetaForest(formula = yi~ grade + length + minutes + wic+
#'                                meta, data, whichweights = "unif")
#' #Print MetaForest object
#' mf.bd2004
#' #Check convergence plot
#' plot(mf.bd2004)
#' #Check summary
#' summary(mf.bd2004, digits = 4)
#' #Examine variable importance plot
#' VarImpPlot(mf.bd2004)
MetaForest <- function(formula, data, vi = "vi", whichweights = "random",
                       num.trees = 500, mtry = NULL, method = "REML",
                       tau2 = NULL, ...) {
    args <- match.call()
    yi <- as.character(formula[2])
    mods <- get_all_vars(formula, data)
    if(vi %in% names(mods)) mods <- mods[-match(vi, names(mods))]
    vi <- data[[vi]]
    data <- mods

    rma_before <- metafor::rma(yi = data[[yi]], vi = vi, method = method)

    if(is.null(tau2)) tau2 <- rma_before$tau2

    if (whichweights == "unif") {
        metaweights <- rep(1, nrow(data))
    }
    if (whichweights == "fixed") {
        metaweights <- (1/vi)
    }
    if (whichweights == "random") {
        metaweights <- 1/(vi + tau2)
    }

    metaweights <- (metaweights/sum(metaweights)) * nrow(data)

    mf <- ranger::ranger(formula = formula, data = data, num.trees = num.trees,
                 mtry = mtry, importance = "permutation", write.forest = TRUE,
                 case.weights = metaweights, ...)
    mf$call <- formula
    predicted <- mf$predictions
    observed <- data[[yi]]
    residuals <- observed - predicted

    rma_after <- metafor::rma(yi = residuals, vi = vi, method = method)

    output <- list(forest = mf, rma_before = rma_before, rma_after = rma_after, call = args, data = data, vi = vi, weights = metaweights)
    class(output) <- "MetaForest"
    output
}
