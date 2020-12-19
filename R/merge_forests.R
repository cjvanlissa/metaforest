merge_mf_cluster <- function(rf1, rf2, ...){
  ## Compute importance

  dots <- list(...)
  predicted <- rf1$predictions
  predicted[is.na(predicted)] <- rf2$predictions[!is.na(rf2$predictions)]

  forest <- list(predictions = predicted,
                 num.trees = rf1$num.trees + rf2$num.trees,
                 num.independent.variables = rf1$num.independent.variables,
                 mtry = rf1$mtry,
                 min.node.size = rf1$min.node.size,
                 variable.importance = (rf1$variable.importance + rf2$variable.importance)/2,
                 prediction.error = mean(c(rf1$prediction.error, rf2$prediction.error)),
                 forest = list(dependent.varID = rf1$forest$dependent.varID,
                               num.trees = rf1$num.trees + rf2$num.trees,
                               child.nodeIDs = c(rbind(rf1$forest$child.nodeIDs, rf2$forest$child.nodeIDs)),
                               split.varIDs = c(rbind(rf1$forest$split.varIDs, rf2$forest$split.varIDs)),
                               split.values = c(rbind(rf1$forest$split.values, rf2$forest$split.values)),
                               is.ordered = rf1$forest$is.ordered,
                               independent.variable.names = rf1$forest$independent.variable.names,
                               treetype = rf1$forest$treetype),
                 #rf2 = rf2,
                 splitrule = rf1$splitrule,
                 treetype = rf1$treetype,
                 r.squared = 1 - mean(c(rf1$prediction.error, rf2$prediction.error)) / var(dots[["y"]]),
                 call = formula,
                 importance.mode = "permutation",
                 num.samples = rf1$num.samples,
                 cluster_forests = list(rf1, rf2)
  )
  if(!is.null(rf1[["inbag.counts"]])){
    forest[["inbag.counts"]] <- c(rf1$inbag.counts, rf2$inbag.counts)
  }
  if(!is.null(rf1[["replace"]])){
    forest[["replace"]] <- rf1$replace
  }
  class(forest) <- "ranger"
  class(forest$forest) <- "ranger.forest"
  forest
}
