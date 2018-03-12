#' Plots partial dependence plots for a MetaForest object, using the function
#' \code{partial_dependence()} from the package \code{edarf}.
#'
#' @param mf MetaForest object.
#' @param vars Character vector containing the moderator names for which to plot
#' partial dependence plots. If empty, all moderators are plotted.
#' @param interaction Logical, indicating whether a bivariate interaction should
#' be plotted, using a heatmap. Only valid when the number of \code{vars} is 2.
#' @param pi Numeric (0-1). What percentile interval should be plotted for the
#' partial dependence predictions? Defaults to NULL. To obtain a 95\% confidence
#' interval, set to \code{.95}.
#' @param rawdata Logical, indicating whether to plot weighted raw data.
#' Defaults to FALSE. Uses the same weights as the MetaForest object passed.
#' @param resolution Integer vector of length two, giving the resolution of the
#' partial predictions. The first element indicates the resolution of the
#' partial predictions; the second element gives the number of rows of the data
#' to be sampled without replacement when averaging over values of the other
#' predictors.
#' @param ... Additional arguments to be passed to \code{marginalPrediction}.
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom mmpf marginalPrediction
#' @export
#' @examples
#' set.seed(42)
#' data <- SimulateSMD(k_train = 100, model = es * x[, 1] + es * x[, 2] + es *
#'                                            x[, 1] * x[, 2])
#' mf.random <- MetaForest(formula = yi ~ ., data = data$training,
#'                         whichweights = "random", method = "DL",
#'                         tau2 = 0.2450)
#' #Examine univariate partial dependence plot for all variables in the model:
#' PartialDependence(mf.random)
#' #Examine bivariate partial dependence plot the interaction between X1 and X2:
#' PartialDependence(mf.random, vars = c("X1", "X2"), interaction = TRUE)
PartialDependence <- function(mf, vars = NULL, interaction = FALSE, pi = NULL, rawdata = FALSE, resolution = NULL, ...){
  if(is.null(vars)){
    vars <- names(mf$forest$variable.importance)
  } else {
    vars <- vars[which(vars %in% names(mf$forest$variable.importance))]
  }
  if(interaction & (length(vars) > 2 | length(vars) == 1)) {
    stop("Only bivariate interactions can be plotted. Argument \'vars\' must be of length 2 when \'interaction = TRUE\'.")
  }
  classes <- sapply(mf$data[vars], class)
  if(!interaction & (any(classes %in% c("numeric", "integer")) & any(classes %in% c("factor", "character")))){
    stop("Argument \'vars\' contains both numeric and factor variables. Please request separate partial dependence plots for numeric and factor variables.")
  }

  data <- get_all_vars(as.formula(mf$call[2]), mf$data)
  target <- as.character(as.formula(mf$call[2])[2])

  if(is.null(resolution)){
    resolution <- c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data))
  }
  args <- list(
    "data" = data,
    "n" = resolution,
    "model" = mf$forest,
    "predict.fun" = function(object, newdata) {
      predict(object, data = newdata)$predictions
    },
    ...
  )
  if (!is.null(pi) & !interaction){
    args[["aggregate.fun"]] <- function(x){ c(sum(x)/length(x), quantile(x, c((.5 * (1 - pi)), 1 - (.5 * (1 - pi))))) }
  }
  if(interaction) {
    args[["vars"]] <- vars
    pd = do.call("marginalPrediction", args)
    names(pd)[3] <- target
  } else {
    pd <- lapply(vars, function(x) {
      do.call(
        "marginalPrediction",
        c(args, "vars" = x)
      )
    })
    var_labels <- unlist(sapply(1:length(vars), function(x){rep(vars[x], nrow(pd[[x]]))}, simplify = FALSE))
    pd <- lapply(pd, function(x){
      names(x)[c(1,2)] <- c("Value", target)
      x
    })
    pd <- do.call("rbind", pd)
    pd$Variable <- var_labels
  }
  if(!is.null(pi)) names(pd)[c(3,4)] <- c("lower", "upper")

  if(!interaction) { #If no interaction is requested, plot univariate plots
    p <- ggplot(pd, aes_string("Value", target))
    if(all(classes %in% c("numeric", "integer"))){
      p <- p + geom_line(aes(group=1)) + scale_x_continuous(expand = c(0,0))
      if(!is.null(pi)){
        p <- p + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = .2)
      }
      if(rawdata){
        raw.data <- data.frame(wi = mf$weights, mf$data[c(1, match(vars, names(mf$data)))])
        names(raw.data)[-c(1,2)] <- paste0("Value.", names(raw.data)[-c(1,2)])
        raw.data <- reshape(raw.data, varying = 3:ncol(raw.data), direction = "long", timevar = "Variable")[, c(1:4)]
        p <- p + geom_point(data = raw.data, alpha=.1, aes_string(size = "wi"))
      }
    } else {
      p <- p + geom_point(size = 5, shape = 18)
      if(!is.null(pi)){
        p <- p + geom_errorbar(aes_string(
                      ymin = "lower",
                      ymax = "upper"),
                      width = .4)
      }
      if(rawdata){
        raw.data <- data.frame(wi = mf$weights, mf$data[c(1, match(vars, names(mf$data)))])
        names(raw.data)[-c(1,2)] <- paste0("Value.", names(raw.data)[-c(1,2)])
        raw.data <- reshape(raw.data, varying = 3:ncol(raw.data), direction = "long", timevar = "Variable")[, c(1:4)]
        p <- p + geom_jitter(data = raw.data, width = .2, alpha=.1, aes_string(size = "wi"))
      }
    }

    if (length(vars) == 1) {
      p <- p + labs(x = vars)
    } else {
      p <- p + facet_wrap(~Variable, scales = "free_x")
    }
  } else { #If an interaction plot is requested
      p <- ggplot(pd, aes_string(vars[1], vars[2], fill = target)) +
      geom_raster() +
      scale_fill_gradient(low = "white", high = "black")
      if(class(vars[1]) %in% c("factor", "character")){
        p <- p + scale_x_discrete(expand=c(0,0))
      } else {
        p <- p + scale_x_continuous(expand=c(0,0))
      }
      if(class(vars[2]) %in% c("factor", "character")){
        p <- p + scale_y_discrete(expand=c(0,0))
      } else {
        p <- p + scale_y_continuous(expand=c(0,0))
      }
  }

  p + theme_bw() + scale_size(guide = 'none')
}
