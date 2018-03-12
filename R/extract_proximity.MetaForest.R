#' Extract proximity matrix for a MetaForest object.
#'
#' @param fit object of class \'MetaForest\'.
#' @param newdata new data with the same columns as the data used for \code{fit}
#' @return an n x n matrix where position i, j gives the proportion of times
#' observation i and j are in the same terminal node across all trees.
#' @importFrom stats predict
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
extract_proximity = function(fit, newdata) UseMethod("extract_proximity")
#' @export
extract_proximity.MetaForest <- function(fit, newdata) {
    if (!inherits(fit, "MetaForest"))
      stop("Argument 'fit' must be an object of class \"MetaForest\".")

    newdata <- get_all_vars(as.formula(fit$call[2]), fit$data)
    newdata <- newdata[, -match(as.character(as.formula(fit$call[2])[2]), names(newdata))]
    pred <- predict(fit$forest, newdata, type = "terminalNodes")$predictions
    n <- nrow(pred)
    apply(pred, 1, function(x){
      .colSums(t(pred) == x, m = dim(pred)[1], n = dim(pred)[2])
    }) / ncol(pred)
}
