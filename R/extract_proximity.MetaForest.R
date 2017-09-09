#' Extract proximity for a MetaForest object.
#'
#' @param fit object of class \'MetaForest\'.
#' @param newdata new data with the same columns as the data used for \code{fit}
#' @return an n x n matrix where position i, j gives the proportion of times
#' observation i and j are in the same terminal node across all trees.
#' @import edarf
#' @import ranger
#' @export
#' @examples
#' \dontshow{
#' set.seed(42)
#' data <- SimulateSMD(k_train = 100, distribution = "bernoulli", model = es *
#'                     x[,1]*x[,2])
#' #Conduct unweighted MetaForest analysis
#' mf.unif <- MetaForest(formula = yi ~ ., data = data$training,
#'                       whichweights = "unif", method = "DL")
#' prox_matrix <- extract_proximity(mf.unif)
#' }
extract_proximity.MetaForest <- function(fit, newdata) {
    if (!inherits(fit, "MetaForest"))
      stop("Argument 'fit' must be an object of class \"MetaForest\".")
    newdata <- fit$data[, -match(gsub("(.+?) ~.*", "\\1", as.character(fit$call)[2]), names(fit$data))]
    fit <- fit$forest
    extract_proximity(fit = fit, newdata = newdata)
}
