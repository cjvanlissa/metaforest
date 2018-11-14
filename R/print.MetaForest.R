#' @method print MetaForest
#' @export
print.MetaForest <- function(x, digits = NULL, ...) {
    cat("Call:\n")
    print(x$call)

    cat("\nR squared (OOB):                 ", formatC(x$forest$r.squared, digits = digits, format="f"), "\n")
    cat("Residual heterogeneity (tau2):   ", formatC(x$rma_after$tau2, digits = digits, format="f"), "\n")
}
