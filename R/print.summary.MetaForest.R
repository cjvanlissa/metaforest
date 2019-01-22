#' Prints summary.MetaForest object.
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @param digits minimal number of significant digits, see \code{print.default}.
#' @export
#' @examples
#' \dontshow{
#' set.seed(28)
#' data <- SimulateSMD()
#' mf.random <- MetaForest(formula = yi ~ ., data = data$training,
#'                       whichweights = "random")
#' print(summary(mf.random), digits = 3)
#' }
print.summary.MetaForest <- function(x, digits, ...) {
    if (missing(digits)){
      digits <- 4
    }

    x[["forest"]][c(7, 8)] <- formatC(as.numeric(x[["forest"]][c(7, 8)]), digits = digits, format = "f")
    names(x[["forest"]]) <- sprintf("%-30s", c("Type of analysis:", "Number of studies:", "Number of moderators:", "Number of trees in forest:", "Candidate variables per split:", "Minimum terminal node size:", "OOB prediction error (MSE):", "R squared (OOB):"))

    cat("MetaForest results\n")
    prmatrix(as.matrix(x[["forest"]]), collab = "", quote = FALSE)

    x[["rma"]][ , -c(6)] <- formatC(x[["rma"]][ , -c(6)], digits = digits, format = "f")

    rownames(x[["rma"]]) <- sprintf("%-30s", c("Raw effect sizes:", "Residuals (after MetaForest):"))

    cat("\nTests for Heterogeneity: \n")
    print(x[["rma"]][, c(1:7)], quote=FALSE)

    cat("\n\nRandom intercept meta-analyses:\n")
    print(x[["rma"]][, c(8:12)], quote=FALSE)
}
