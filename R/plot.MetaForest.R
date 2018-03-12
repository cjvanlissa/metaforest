#' Plots cumulative MSE for a MetaForest object.
#'
#' @param x MetaForest object.
#' @param y not used for plot.MetaForest
#' @param ... Arguments to be passed to methods, not used for plot.MetaForest
#' @return A ggplot object, visualizing the number of trees on the x-axis, and
#' the cumulative mean of the MSE of that number of trees on the y-axis. As a
#' visual aid to assess convergence, a dashed gray line is plotted at the median
#' cumulative MSE value.
#' @import ggplot2
#' @import ranger
#' @export
#' @examples
#' \dontshow{
#' set.seed(42)
#' data <- SimulateSMD()
#' #Conduct unweighted MetaForest analysis
#' mf.unif <- MetaForest(formula = yi ~ ., data = data$training,
#'                       whichweights = "unif", method = "DL")
#' plot(mf.unif)
#' }
plot.MetaForest <- function(x, y, ...) {
    if (!inherits(x, "MetaForest"))
      stop("Argument 'x' must be an object of class \"MetaForest\".")
    ranger_object <- x$forest
    data <- get_all_vars(as.formula(x$call[2]), x$data)

    observed <- data[[as.character(as.formula(x$call[2])[2])]]
    predictions <- predict(ranger_object, data = data, predict.all = TRUE)$predictions
    mses <- colMeans(sweep(predictions, 1, observed, "-")^2)
    mses <- cumsum(mses) / 1:length(mses)
    cumulative_predictions <- data.frame(num_trees = 1:length(mses), mse = mses)
    ggplot(cumulative_predictions, aes_string(x = "num_trees", y = "mse")) +
      geom_line() +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Cumulative MSE", x = "Number of trees", title = "Convergence plot") +
      geom_hline(yintercept = median(cumulative_predictions$mse), colour = "gray50", linetype = 2)
}
