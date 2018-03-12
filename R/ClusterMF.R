#' Clustered MetaForest analysis for dependent data.
#'
#' This function conducts a clustered MetaForest analysis for dependent data.
#' Using clustered random sampling, the dataset is split into two
#' cross-validation samples by study. All dependent effect sizes from each study
#' are thus included in the same cross-validation sample. Then, two random
#' forests are grown on these cross-validation samples, and for each random
#' forest, the other sample is used to calculate prediction error and variable
#' importance (see Janitza, Celik, & Boulesteix, 2016). The \code{predict.MetaForest}
#' method uses all trees from both forests.
#' @param formula Formula. Specify a formula for the MetaForest model, for
#' example, \code{yi ~ .} to predict the outcome \code{yi} from all moderators
#' in the data.
#' @param data Data.frame. Provide a data.frame containing the effect size,
#' moderators, and the variance of the effect size.
#' Defaults to 100.
#' @param vi Character. Specify the name of the column in the \code{data} that
#' contains the variances of the effect sizes. This column will be removed from
#' the data prior to analysis. Defaults to \code{"vi"}.
#' @param study Character. Specify the name of the column in the \code{data} that
#' contains the study id. This column can be a vector of integers, or a factor.
#' This column will be removed from the data prior to analysis.
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
#' "EB", "HS", or "GENQ". Default is "REML".
#' See the \link[metafor]{metafor} package for more information.
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
#' @export
#' @examples
#' #Load and clean data from metafor
#' data <- get(data(dat.bourassa1996))
#' data <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
#'                data = data, add = 1/2, to = "all")
#' data$mage[is.na(data$mage)] <- median(data$mage, na.rm = TRUE)
#' data[c(5:8)] <- lapply(data[c(5:8)], factor)
#' data$yi <- as.numeric(data$yi)
#' mf.cluster.b1996 <- ClusterMF(formula = yi~ selection + investigator +
#'                               hand_assess + eye_assess + mage +sex,
#'                               data, study = "sample",
#'                               whichweights = "unif", num.trees = 300)
#' #Print MetaForest object
#' mf.cluster.b1996
#' #Check convergence plot
#' plot(mf.cluster.b1996)
#' #Check summary
#' summary(mf.cluster.b1996, digits = 4)
#' #Check variable importance plot
#' VarImpPlot(mf.cluster.b1996)
#' #Univariate partial dependence plot
#' PartialDependence(mf.cluster.b1996, vars = "eye_assess")
#' #Interpolated partial dependence plot for a bivariate interaction
#' \donttest{
#' PartialDependence(mf.cluster.b1996, vars = c("mage", "eye_assess"), interaction = TRUE)
#' }
ClusterMF <- function(formula, data, vi = "vi", study = NULL,
                      whichweights = "random", num.trees = 500, mtry = NULL,
                      method = "REML", tau2 = NULL, ...) {
    if(is.null(study)){ stop("Please provide a valid \"study\" column name.")}
    args <- match.call()
    yi <- as.character(formula[2])
    mods <- get_all_vars(formula, data)
    if(vi %in% names(mods)) mods <- mods[-match(vi, names(mods))]
    if(study %in% names(mods)) mods <- mods[-match(study, names(mods))]
    vi <- data[[vi]]
    study <- data[[study]]
    data <- mods
    if(!class(study) %in% c("integer", "numeric", "factor")) stop("Please provide a valid \"study\" column name.")
    if ("case.weights" %in% names(args)) {
      stop("Error: Argument 'case.weights' not supported in ClusterMF")
    }
    if ("holdout" %in% names(args)) {
      stop("Error: Argument 'holdout' not supported in ClusterMF.")
    }
    if ("importance" %in% names(args)) {
      stop("Error: Argument 'importance' not supported in ClusterMF. Always set to 'permutation'.")
    }
    if ("replace" %in% names(args)) {
      stop("Error: Argument 'replace' not supported in ClusterMF.")
    }

    rma_before <- rma(yi = data[[yi]], vi = vi, method = method)

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

    #Sample subsamples by cluster (study), calculate weights by subsample
    unique_studies <- unique(study)
    subsample_studies <- sample(unique_studies, round(length(unique_studies)/2))
    subsample_studies <- list(sample1 = sort(subsample_studies), sample2 = unique_studies[!(unique_studies %in% subsample_studies)])
    rm(unique_studies)

    subsample_weights <- lapply(subsample_studies, function(samplestudies){
      set_to_zero <- which(!(study %in% samplestudies))
      ss_weights <- metaweights
      ss_weights[set_to_zero] <- 0
      (ss_weights / sum(ss_weights)) * length(which((study %in% samplestudies)))
      })

    res <- list(
      rf1 = ranger(
            formula = formula,
            data = data,
            num.trees = num.trees,
            mtry = mtry,
            importance = "permutation",
            write.forest = TRUE,
            case.weights = subsample_weights[[1]],
            holdout = TRUE, ...),
      rf2 = ranger(
            formula = formula,
            data = data,
            num.trees = num.trees,
            mtry = mtry,
            importance = "permutation",
            write.forest = TRUE,
            case.weights = subsample_weights[[2]],
            holdout = TRUE, ...)
    )

    ## Compute importance
    predicted <- res$rf1$predictions
    predicted[is.na(predicted)] <- res$rf2$predictions[!is.na(res$rf2$predictions)]
    observed <- data[[yi]]
    residuals <- observed - predicted

    rma_after <- rma(yi = residuals, vi = vi, method = method)
    forest <- list(predictions = predicted,
                   num.trees = res$rf1$num.trees + res$rf2$num.trees,
                   num.independent.variables = res$rf1$num.independent.variables,
                   mtry = res$rf1$mtry,
                   min.node.size = res$rf1$min.node.size,
                   variable.importance = (res$rf1$variable.importance + res$rf2$variable.importance)/2,
                   prediction.error = mean(c(res$rf1$prediction.error, res$rf2$prediction.error)),
                   forest = list(dependent.varID = res$rf1$forest$dependent.varID,
                                 num.trees = res$rf1$forest$num.trees + res$rf2$forest$num.trees,
                                 child.nodeIDs = c(rbind(res$rf1$forest$child.nodeIDs, res$rf2$forest$child.nodeIDs)),
                                 split.varIDs = c(rbind(res$rf1$forest$split.varIDs, res$rf2$forest$split.varIDs)),
                                 split.values = c(rbind(res$rf1$forest$split.values, res$rf2$forest$split.values)),
                                 is.ordered = res$rf1$forest$is.ordered,
                                 independent.variable.names = res$rf1$forest$independent.variable.names,
                                 treetype = res$rf1$forest$treetype),
                   #rf2 = res$rf2,
                   splitrule = res$rf1$splitrule,
                   treetype = res$rf1$treetype,
                   r.squared = 1 - mean(c(res$rf1$prediction.error, res$rf2$prediction.error)) / var(observed),
                   call = formula,
                   importance.mode = "permutation",
                   num.samples = res$rf1$num.samples,
                   cluster_forests = res
                   )
    class(forest) <- "ranger"
    class(forest$forest) <- "ranger.forest"
    output <- list(forest = forest, rma_before = rma_before, rma_after = rma_after, call = args, data = data, vi = vi, weights = metaweights)
    class(output) <- c("MetaForest")
    output
}
