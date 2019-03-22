#' @title Preselect variables for MetaForest analysis
#' @description Takes a \code{\link{MetaForest}} object, and applies different
#' algorithms for variable selection.
#' @details Currently, available methods under \code{algorithm} are:
#' \describe{
#' \item{replicate}{This simply replicates the analysis, which means the forest
#' has access to the full data set, but the trees are grown on different
#' bootstrap samples across replications (thereby varying monte carlo error).}
#' \item{bootstrap}{This replicates the analysis on bootstrapped samples, which
#' means each replication has access to a different sub-sample of the full data
#' set. When selecting this algorithm, cases are either bootstrap-sampled by
#' \code{study}, or a new \code{study} column is generated, and a clustered
#' MetaForest is grown (because some of the rows in the data will be duplicated)
#' , and this would lead to an under-estimation of the OOB error.}
#' \item{recursive}{Starting with all moderators, the variable with the most
#' negative variable importance is dropped from the model, and the analysis
#' re-run. This is repeated until only variables with a positive variable
#' importance are left, or no variables are left. The proportion of final models
#' containing each variable reflects its importance.}
#' }
#' @param x Model to perform variable selection for. Accepts MetaForest objects.
#' @param replications Integer. Number of replications to run for variable
#' preselection. Default: 100.
#' @param algorithm Character. Preselection method to apply. Currently,
#' 'replicate', 'recursive', and 'bootstrap' are available.
#' @param ... Other arguments to be passed to and from functions.
#' @return An object of class 'mf_preselect'
#' @examples
#' \dontrun{
#' data <- get(data(dat.bourassa1996))
#' data <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
#'                data = data, add = 1/2, to = "all")
#' data$mage[is.na(data$mage)] <- median(data$mage, na.rm = TRUE)
#' data[c(5:8)] <- lapply(data[c(5:8)], factor)
#' data$yi <- as.numeric(data$yi)
#' mf.model <- MetaForest(formula = yi~ selection + investigator + hand_assess + eye_assess +
#'                         mage +sex,
#'           data, study = "sample",
#'           whichweights = "unif", num.trees = 300)
#' preselect(mf.model,
#'           replications = 10,
#'           algorithm = "bootstrap")
#' }
#' @rdname preselect
#' @export
#@inheritParams MetaForest
preselect <- function(x, replications = 100L, algorithm = "replicate", ...){
  UseMethod("preselect", x)
}

#' @method preselect MetaForest
#' @export
preselect.MetaForest <- function(x, replications = 100L, algorithm = "replicate", ...){
  #args <- as.list(formals(MetaForest))
  #args[["..."]] <- NULL
  args <- as.list(x$call)[-1]
  #args[match(names(call_args), names(args))] <- call_args
  if(!is.null(x[["study"]])){
    args$data <- cbind(x$data, vi = x$vi, study = x$study)
    args[["study"]] <- "study"
  } else {
    args$data <- cbind(x$data, vi = x$vi)
  }
  args$vi <- NULL
  mods <- names(x$data)[-1]
  args$formula <- force(as.formula(paste0(names(x$data[1]), " ~ ", paste(mods, collapse = " + "))))
  master_list <- switch(algorithm,
                        recursive = replicate(replications, recursive_mf(mods, args), simplify = FALSE),
                        replicate = replicate(replications, replicate_mf(args), simplify = FALSE),
                        bootstrap = replicate(replications, bootstrap_mf(args), simplify = FALSE))

  r2s <- sapply(master_list, `[`, "r2")

  var_selected <- data.frame(t(sapply(master_list, function(x){
    out <- rep(NA, length(mods))
    out[match(names(x)[-1], mods)] <- x[-1]
    out
  })))
  names(var_selected) <- mods
  outlist <- list(rsquared = r2s, selected = var_selected)
  class(outlist) <- "mf_preselect"
  attributes(outlist)$algorithm <- algorithm
  outlist
}

recursive_mf <- function(modvars, settings){
  settings$formula <- update(as.formula(settings$formula), paste0("~ ", paste(modvars, collapse = " + ")))
  mf_tmp <- do.call(MetaForest, settings)
  if ((!any(mf_tmp$forest$variable.importance < 0)) | (length(mf_tmp$forest$variable.importance) == 1)) {
    return(c(r2 = mf_tmp$forest$r.squared, mf_tmp$forest$variable.importance))
  } else {
    recursive_mf(names(mf_tmp$forest$variable.importance)[-which(
      mf_tmp$forest$variable.importance == min(mf_tmp$forest$variable.importance))],
      settings
    )
  }
}

replicate_mf <- function(settings){
  mf_tmp <- do.call(MetaForest, settings)
  c(r2 = mf_tmp$forest$r.squared, mf_tmp$forest$variable.importance)
}

bootstrap_mf <- function(settings){
  if(is.null(settings$data[["study"]])){
    settings$data$study <- 1L:nrow(settings$data)
  }
  select_these <- sample(unique(settings$data$study), nrow(settings$data), replace = TRUE)
  settings$data <- settings$data[select_these, ]
  settings$study <- "study"
  mf_tmp <- do.call(MetaForest, settings)
  c(r2 = mf_tmp$forest$r.squared, mf_tmp$forest$variable.importance)
}

#' @method plot mf_preselect
#' @export
plot.mf_preselect <- function(x, y, ...){
  order_vars <- imp <- x$selected
  if(attributes(x)$algorithm == "recursive"){
    excluded <- colMeans(is.na(imp))
    order_vars[is.na(order_vars)] <- 0
    names(imp) <- paste0(names(imp), ", ", formatC(colMeans(!is.na(imp))*100, digits = 0, format = "f"), "%")
  }
  ylabel <- paste(
    c("Bootstrapped", "Replicated", "Recursive")[
      match(attributes(x)$algorithm, c("bootstrap", "replicate", "recursive"))],
    "variable Importance (Permutation importance)")
  order_vars <- names(imp)[order(colMeans(order_vars), decreasing = FALSE)]
  plotdat <- data.frame(Variable = ordered(rep(names(imp), each = nrow(imp)), levels = order_vars), Importance = c(as.matrix(imp)))
  plotdat <- plotdat[complete.cases(plotdat), ]

  p <- ggplot(plotdat, aes_string(x = "Variable", y = "Importance")) +
    geom_boxplot(color="black", size=0.2, outlier.shape = NA) +
    geom_jitter(width = .2, alpha = .2) +
    theme_bw() +
    geom_hline(yintercept = 0, colour = "grey50", linetype = 1) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.title.y = element_blank()) +
    ylab(ylabel) +
    coord_flip()

  if(hasArg("label_elements")){
    label_elements <- eval(match.call()[["label_elements"]])
    levels(p$data$Variable) <- rename_fun(levels(p$data$Variable), names(label_elements), label_elements)
  }
  p
}


#' @method print mf_preselect
#' @export
print.mf_preselect <- function(x, digits = 3, ...){
  imp <- x$selected
  if(inherits(x, "mf_preselect_recursive")){
    imp <- !is.na(imp)
    cat("Percentage of replications in which each variable was retained in final model:\n")
    imp <- colMeans(imp)
    print(sort(imp, decreasing = TRUE), digits = digits)
  } else {
    cat("Mean variable importance across bootstrap replications:\n")
    imp_m <- colMeans(imp)
    print(sort(imp_m, decreasing = TRUE), digits = digits)
    cat("\nSD of variable importance across bootstrap replications:\n")
    print(apply(imp, 2, sd)[order(imp_m, decreasing = TRUE)], digits = digits)
  }
  cat("\n\nR2 median: ", median(x$rsquared), "\nR2 sd:", sd(x$rsquared))
}


#' @title Extract variable names from mf_preselect object
#' @description Returns a vector of variable names from an mf_preselect object,
#' based on a cutoff criterion provided.
#' @param x Object of class mf_preselect.
#' @param cutoff Numeric. Must be a value between 0 and 1. By default, uses .95
#' for bootstrapped preselection, and .1 for recursive preselection.
#' @param criterion Character. Which criterion to use. See \code{Details} for
#' more information. By default, uses 'ci' (confidence interval) for
#' bootstrapped preselection, and 'p' (proportion) for recursive preselection.
#' @return Character vector.
#' @details For \code{criterion = 'p'}, the function evaluates the proportion of
#' replications in which a variable achieved a positive (>0) variable
#' importance. For \code{criterion = 'ci'}, the function evaluates whether the
#' lower bound of a confidence interval of a variable's importance across
#' replications exceeds zero. The width of the confidence interval is determined
#' by \code{cutoff}.
#'
#' For recursive preselection, any variable not included in a final
#' model is assigned zero importance.
#' @examples
#' \dontrun{
#' data <- get(data(dat.bourassa1996))
#' data <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
#'                data = data, add = 1/2, to = "all")
#' data$mage[is.na(data$mage)] <- median(data$mage, na.rm = TRUE)
#' data[c(5:8)] <- lapply(data[c(5:8)], factor)
#' data$yi <- as.numeric(data$yi)
#' preselected <- preselect(formula = yi~ selection + investigator + hand_assess + eye_assess +
#'                         mage +sex,
#'           data, study = "sample",
#'           whichweights = "unif", num.trees = 300,
#'           replications = 10,
#'           algorithm = "bootstrap")
#' preselect_vars(preselected)
#' }
#' @rdname preselect_vars
#' @export
#'
preselect_vars <- function(x,
                        cutoff = NULL,
                        criterion = NULL) {
  if (!inherits(x, "mf_preselect"))
    stop("Function preselect_vars() requires an object of class 'mf_preselect'.")
  if (!is.null(cutoff)){
    if(cutoff < 0 | cutoff > 1) stop("Argument 'cutoff' must be a number between 0 and 1.")
  }
  if (attributes(x)$algorithm == "recursive") {
    if (is.null(cutoff))
      cutoff <- .1
    if (is.null(criterion))
      criterion <- "p"
  } else {
    if (is.null(cutoff))
      cutoff <- .95
    if (is.null(criterion))
      criterion <- "ci"
  }
  imp <- x$selected
  imp[is.na(imp)] <- 0
  names(imp)[switch(criterion,
                    p = colMeans(imp > 0) > cutoff,
                    ci = sapply(imp, quantile, (1 - cutoff) / 2) > 0)]
}

