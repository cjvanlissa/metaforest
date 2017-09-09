#' MetaForest prediction
#'
#' @param object \code{MetaForest} object.
#' @param data New test data of class \code{data.frame}.
#' @param type Type of prediction. One of 'response', 'se', 'terminalNodes' with default 'response'. See below for details.
#' @param ... further arguments passed to or from other methods.
#' @return Object of class \code{MetaForest.prediction} with elements
#'   \tabular{ll}{
#'       \code{predictions}    \tab Predicted classes/values (only for classification and regression)  \cr
#'       \code{num.trees}   \tab Number of trees. \cr
#'       \code{num.independent.variables} \tab Number of independent variables. \cr
#'       \code{treetype}    \tab Type of forest/tree. Classification, regression or survival. \cr
#'       \code{num.samples}     \tab Number of samples.
#'   }
#' @seealso \code{\link[ranger]{ranger}}
#' @import ranger
#' @export
#' @examples
#' set.seed(56)
#' data <- SimulateSMD(k_train = 100, model = es * x[,1] * x[,2])
#' #Conduct fixed-effects MetaForest analysis
#' mf.fixed <- MetaForest(formula = yi ~ ., data = data$training,
#'                       whichweights = "fixed", method = "DL")
#' predicted <- predict(mf.fixed, data = data$testing)$predictions
#' r2_cv <- sum((predicted - mean(data$training$yi)) ^ 2)/
#'          sum((data$testing$yi - mean(data$training$yi)) ^ 2)
predict.MetaForest <- function(object, data = NULL,
                           type = "response", ...) {
    if (!inherits(object, "MetaForest"))
      stop("Argument 'x' must be an object of class \"MetaForest\".")
    if(is.null(data)) data <- object$data
    forest <- object$forest

    output <- predict(forest, data = data, type = type)#, ...)
    class(output) <- c("MetaForest.predictions", class(output))
    output
}
