#' Plots partial dependence plots for a MetaForest object, using the function
#' \code{partial_dependence()} from the package \code{edarf}.
#'
#' @param mf MetaForest object.
#' @param vars Character vector containing the moderator names for which to plot
#' partial dependence plots. If empty, all moderators are plotted.
#' @param interaction Logical, indicating whether a bivariate interaction should
#' be plotted, using a heatmap. Only valid when the number of \code{vars} is 2.
#' @param ... Additional arguments to be passed to \code{partial_dependence}.
#' @return A ggplot object.
#' @import edarf
#' @import ggplot2
#' @import reshape2
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
PartialDependence <- function(mf, vars = NULL, interaction = FALSE, ...){
  if(is.null(vars)){
    vars <- names(mf$forest$variable.importance)
  } else {
    vars <- vars[which(vars %in% names(mf$forest$variable.importance))]
  }
  if(interaction & (length(vars) > 2 | length(vars) == 1)) {
    stop("Only bivariate interactions can be plotted. Argument \'vars\' must be of length 2 when \'interaction = TRUE\'.")
  }
  data <- mf$data

  data <- get_all_vars(as.formula(mf$call[2]), data)
  target <- as.character(as.formula(mf$call[2])[2])

  pd <- partial_dependence(fit = mf$forest, vars = vars, data = data, interaction = interaction, ...)

  if(!interaction) { #If no interaction is requested, plot univariate plots
    classes <- sapply(pd[-ncol(pd)], class)
    if(any(classes %in% c("numeric", "integer")) & any(classes %in% c("factor", "character"))){
      warning("Argument \'vars\' contains both numeric and categorical variables. Numeric variables will be treated as factors.")
      pd[names(classes)[which(sapply(classes, function(x) any(c("numeric", "integer") %in% x)))]] <- lapply(pd[names(classes)[which(sapply(classes, function(x) any(c("numeric", "integer") %in% x)))]], factor, ordered = TRUE)
      levs <- unique(unlist(sapply(pd[-match(target, names(pd))], function(x){levels(factor(x))})))
    }

    dat <- melt(pd, id.vars = target, na.rm = TRUE)

    if(exists("levs")){
      dat$value <- ordered(dat$value, levels = levs)
    }

    p <- ggplot(dat, aes_string("value", target)) +
                geom_line(aes(group=1)) +
                geom_point()
    if (length(vars) == 1) {
      p <- p + labs(x = vars)
    } else {
      p <- p + facet_wrap(~variable, scales = "free_x")
    }
  } else { #If an interaction plot is requested
    dat <- pd
    classes <- unique(sapply(dat[-3], class)) #Check if all variables are the same class
    if(length(classes) == 1){
      p <- ggplot(dat, aes_string(vars[1], vars[2], fill = target)) +
      geom_raster() +
      scale_fill_gradient(low = "white", high = "black")
      if(classes %in% c("factor", "character")){
        p <- p + scale_x_discrete(expand=c(0,0)) +
                 scale_y_discrete(expand=c(0,0))
      } else {
        p <- p + scale_x_continuous(expand=c(0,0)) +
                 scale_y_continuous(expand=c(0,0))
      }
    } else {
        warning("Provided one continuous and one numeric variable. Will try to interpolate the numeric variable with twice as many categories as the original number of unique values.")
        the_factor <- names(dat)[which(classes %in% c("factor", "character"))]
        the_num <- names(dat)[which(!(classes %in% c("factor", "character")))]
        dat <- do.call(rbind, lapply(levels(dat[[the_factor]]), function(x){
          tmp_dat <- dat[with(dat, get(the_factor) == x), ]
          data.frame(x, approx(tmp_dat[[the_num]], tmp_dat[[3]], n = length(unique(dat[[the_num]]))*2))
        }))
        names(dat) <- c(the_factor, the_num, target)

      p <- ggplot(dat, aes_string(the_num, the_factor, fill = target)) +
                  geom_raster() +
                  scale_x_continuous(expand=c(0,0)) +
                  scale_y_discrete(expand=c(0,0)) +
                  scale_fill_gradient(low = "white", high = "black")
    }
  }

  p <- p + theme_bw()
  p
}
