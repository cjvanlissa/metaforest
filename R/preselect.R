#' @title Preselect variables for MetaForest analysis
#' @description Applies different methods to preselect variables before
#' conducting a \code{\link{MetaForest}} analysis.
#' @inheritParams MetaForest
#' @param replications Integer. Number of replications to run for variable
#' preselection. Default: 100.
#' @param algorithm Character. Preselection method to apply. Currently, either
#' 'recursive' or 'bootstrap' are available.
#' @return An object of class 'mf_preselect'
#' @examples
#' \dontrun{
#' data <- get(data(dat.bourassa1996))
#' data <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
#'                data = data, add = 1/2, to = "all")
#' data$mage[is.na(data$mage)] <- median(data$mage, na.rm = TRUE)
#' data[c(5:8)] <- lapply(data[c(5:8)], factor)
#' data$yi <- as.numeric(data$yi)
#' preselect(formula = yi~ selection + investigator + hand_assess + eye_assess +
#'                         mage +sex,
#'           data, study = "sample",
#'           whichweights = "unif", num.trees = 300,
#'           replications = 10,
#'           algorithm = "recursive")
#' }
#' @rdname preselect
#' @export
#'
preselect <- function(formula, data, vi = "vi", study = NULL,
                                   whichweights = "random",
                                   num.trees = 500, mtry = NULL, method = "REML",
                                   tau2 = NULL, ...,
                                   replications = 100L,
                      algorithm = "bootstrap"){

  settings <- as.list(match.call())[-1]
  settings$replications <- NULL
  settings$algorithm <- NULL
  mods <- names(get_all_vars(formula, data = data))
  if(as.character(formula[2]) %in% mods) mods <- mods[-match(as.character(formula[2]), mods)]
  if(vi %in% mods) mods <- mods[-match(vi, mods)]
  if(!is.null(study)){
    if(study %in% mods) mods <- mods[-match(study, mods)]
  }

  master_list <- switch(algorithm,
                        recursive = replicate(replications, recursive_mf(mods, settings)),
                        bootstrap = replicate(replications, bootstrap_mf(mods, settings), simplify = FALSE))

  r2s <- sapply(master_list, `[`, "r2")

  var_selected <- data.frame(t(sapply(master_list, function(x){
    out <- rep(NA, length(mods))
    out[match(names(x)[-1], mods)] <- x[-1]
    out
  })))
  names(var_selected) <- mods
  outlist <- list(rsquared = r2s, selected = var_selected)
  class(outlist) <- "mf_preselect"
  outlist
}

select_vars <- function(x, threshold){

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

bootstrap_mf <- function(modvars, settings){
  settings$formula <- update(as.formula(settings$formula), paste0("~ ", paste(modvars, collapse = " + ")))
  mf_tmp <- do.call(MetaForest, settings)
  c(r2 = mf_tmp$forest$r.squared, mf_tmp$forest$variable.importance)
}

#' @method plot mf_preselect
#' @export
plot.mf_preselect <- function(x, y, ..., threshold = .01){
  if(threshold > 1) stop("Argument 'threshold' must be between 0 and 1.")
  imp <- x$selected
  recursive <- FALSE
  if(anyNA(imp)){
    recursive <- TRUE
    imp <- !is.na(imp)
    imp <- colMeans(imp)
  } else {
    imp <- colMeans(imp)
    threshold <- threshold*max(imp)
  }

  xlabel <- ifelse(recursive, "Selected in % of iterations", "Mean bootstrapped variable importance")
  data_var_selected <- data.frame(Variable = factor(names(imp)[order(imp, decreasing = FALSE)],
                                  levels = names(imp)[order(imp, decreasing = FALSE)]),
                                  Selected = ifelse(recursive, 100, 1)*imp[order(imp, decreasing = FALSE)])

  p <- ggplot(data_var_selected, aes_string(y="Variable", x="Selected"))+
    geom_segment(aes_string(x=0, xend="Selected", y="Variable", yend="Variable"), colour = "grey50", linetype = 2)+
    geom_vline(xintercept = 0, colour = "grey50", linetype = 1)+
    geom_point(shape=1, size=2) +
    geom_vline(xintercept = threshold, linetype = 3)+
    xlab(xlabel)+
    theme_bw()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y=element_blank())
  p
}


#' @method print mf_preselect
#' @export
print.mf_preselect <- function(x, digits = 3, ...){
  imp <- x$selected
  if(anyNA(imp)){
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
