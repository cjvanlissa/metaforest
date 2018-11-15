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
#' @examples
#' #Load and clean data from metafor
#' data <- get(data(dat.bourassa1996))
#' data <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
#'                data = data, add = 1/2, to = "all")
#' data$mage[is.na(data$mage)] <- median(data$mage, na.rm = TRUE)
#' data[c(5:8)] <- lapply(data[c(5:8)], factor)
#' data$yi <- as.numeric(data$yi)
#' mf.cluster.b1996 <- MetaForest(formula = yi~ selection + investigator +
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
#' @templateVar fun ClusterMF
#' @template template-depr_fun
NULL

#' @templateVar old ClusterMF
#' @templateVar new MetaForest
#' @template template-depr_pkg
#' @export
ClusterMF <- function(...) {
  .Deprecated("MetaForest")
  args <- match.call()
  do.call(MetaForest, as.list(args)[-1])
}


