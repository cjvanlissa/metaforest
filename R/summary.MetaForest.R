#' Summarize MetaForest object.
#'
#' @param object an object for which a summary is desired.
#' @param ... additional arguments affecting the summary produced.
#' @param digits the number of digits desired, defaults to 2.
#' @export
#' @examples
#' \dontshow{
#' set.seed(64)
#' data <- SimulateSMD()
#' sum <- summary(mf.random <- MetaForest(formula = yi ~ .,
#'                data = data$training, whichweights = "random"), digits = 5)
#' sum
#' }
summary.MetaForest <- function(object, ..., digits = 2) {
    if (!inherits(object, "MetaForest"))
      stop("Object must be of class \"MetaForest\".")
  mf_type = as.character(object$call[1])
  forest.table <-
    cbind(
      type = mf_type,
      k = ifelse(mf_type == "ClusterMF", paste0("Forest 1: ", sum(is.na(object$forest$cluster_forests$rf1$predictions)), ", Forest 2: ", sum(is.na(object$forest$cluster_forests$rf2$predictions))), object$forest$num.samples),
      M = object$forest$num.independent.variables,
      num.trees = ifelse(mf_type == "ClusterMF", paste0("Two forests of length ", object$forest$num.trees/2), object$forest$num.trees),
      mtry = object$forest$mtry,
      min.node.size = object$forest$min.node.size,
      MSEoob = object$forest$prediction.error,
      R2oob = object$forest$r.squared
    )

  rma.table <- cbind(
    tau2 = c(object$rma_before$tau2, object$rma_after$tau2),
    tau2_SE = c(object$rma_before$se.tau2, object$rma_after$se.tau2),
    `I^2` = c(object$rma_before$I2, object$rma_after$I2),
    `H^2` = c(object$rma_before$H2, object$rma_after$H2),
    "Q-test" = c(object$rma_before$QE, object$rma_after$QE),
    df = c(object$rma_before$k - object$rma_before$p, object$rma_after$k - object$rma_after$p),
    Q_p = c(object$rma_before$QEp, object$rma_after$QEp),
    Intercept = c(object$rma_before$beta, object$rma_after$beta),
    se = c(object$rma_before$se, object$rma_after$se),
    ci.lb = c(object$rma_before$ci.lb, object$rma_after$ci.lb),
    ci.ub = c(object$rma_before$ci.ub, object$rma_after$ci.ub),
    p = c(object$rma_before$pval, object$rma_after$pval)
  )
  sum <- list(forest = forest.table, rma = rma.table, digits = digits)
  class(sum) <- "summary.MetaForest"
  return(sum)
}
