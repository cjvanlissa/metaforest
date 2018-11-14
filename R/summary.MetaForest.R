#' @method summary MetaForest
#' @export
summary.MetaForest <- function(object, ...) {
  if(inherits(object, "cluster_mf")){
    mf_type <- "Clustered MetaForest"
    k <- paste0("Forest 1: ", sum(is.na(object$forest$cluster_forests$rf1$predictions)), ", Forest 2: ", sum(is.na(object$forest$cluster_forests$rf2$predictions)))
    num.trees <- paste0("Two forests of length ", object$forest$num.trees/2)
  } else {
    mf_type <- "MetaForest"
    k <- object$forest$num.samples
    num.trees <- object$forest$num.trees
  }

  forest.table <-
    cbind(
      type = mf_type,
      k = k,
      M = object$forest$num.independent.variables,
      num.trees = num.trees,
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
  sum <- list(forest = forest.table, rma = rma.table)
  class(sum) <- "summary.MetaForest"
  sum
}
