#' Prints MetaForest object.
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @param digits minimal number of significant digits, see \code{print.default}.
#' @export
#' @examples
#' \dontshow{
#' set.seed(11)
#' data <- SimulateSMD()
#' mf.fixed <- MetaForest(formula = yi ~ ., data = data$training,
#'                       whichweights = "fixed")
#' mf.fixed
#' }
print.MetaForest <- function(x, digits = NULL, ...) {
    if (!inherits(x, "MetaForest"))
      stop("Argument 'mf' must be an object of class \"MetaForest\".")

    cat("Call:\n")
    print(x$call)

    cat("\nR squared (OOB):                 ", formatC(x$forest$r.squared, digits = digits, format="f"), "\n")
    cat("Residual heterogeneity (tau2):   ", formatC(x$rma_after$tau2, digits = digits, format="f"), "\n")
}
