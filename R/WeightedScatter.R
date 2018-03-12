#' Plots weighted scatterplots for meta-analytic data. Can plot effect size as
#' a function of either continuous (numeric, integer) or categorical (factor,
#' character) predictors.
#'
#' @param data A data.frame.
#' @param yi Character. The name of the column in \code{data} that contains the
#' meta-analysis effect sizes. Defaults to \code{"yi"}.
#' @param vi Character. The name of the column in the \code{data} that contains
#' the variances of the effect sizes. Defaults to \code{"vi"}. By default,
#' \code{vi} is used to calculate fixed-effects weights, because fixed effects
#' weights summarize the data set at hand, rather than generalizing to the
#' population.
#' @param vars Character vector containing either 1) the names of specific
#' moderator variables to plot, or 2)  scatterplots. If empty, all moderators
#' are plotted. Note, however, that continuous and categorical variables cannot
#' be plotted at the same time.
#' @param tau2 Numeric. Provide an optional value for tau2. If this value is
#' provided, random-effects weights will be used instead of fixed-effects
#' weights.
#' @param summarize Logical. Should summary stats be displayed? Defaults to
#' FALSE. If TRUE, a smooth trend line is displayed for continuous variables,
#' using [stats::loess()] for less than 1000 observations, and [mgcv::gam()] for
#' larger datasets. For categorical variables, box-and-whiskers plots are
#' displayed. Outliers are omitted, because the raw data fulfill this function.
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @examples
#' set.seed(42)
#' data <- SimulateSMD(k_train = 100, model = es * x[, 1] + es * x[, 2] + es *
#'                                            x[, 1] * x[, 2])$training
#' WeightedScatter(data, summarize = TRUE)
#' WeightedScatter(data, tau2 = .04)
#' data$X2 <- factor(ifelse(data$X2 > mean(data$X2), "Low", "High"))
#' data$X3 <- factor(ifelse(data$X3 > mean(data$X3), "Small", "Big"))
#' WeightedScatter(data, vars = "all.factors", summarize = TRUE)
WeightedScatter <-
  function(data,
           yi = "yi",
           vi = "vi",
           vars = "all.continuous",
           tau2 = NULL,
           summarize = FALSE) {
    select_vars <- names(data)[-which(names(data) %in% c(yi, vi))]
    if (all(vars %in% c("all.continuous", "all.factors"))) {
      if (vars == "all.continuous") {
        select_vars <-
          select_vars[which(sapply(data[select_vars], class) %in% c("numeric", "integer"))]
      } else {
        select_vars <-
          select_vars[which(sapply(data[select_vars], class) %in% c("factor", "character"))]
      }
    } else {
      select_vars <- select_vars[which(select_vars %in% vars)]
      classes <- sapply(data[select_vars], class)
      if (any(classes %in% c("numeric", "integer")) &
          any(classes %in% c("factor", "character"))) {
        stop(
          "Argument \'vars\' contains both numeric and factor variables. Please request separate WeightedScatter plots for numeric and factor variables."
        )
      } else {
        if(classes[1] %in% c("numeric", "integer")){
          vars <- "all.continuous"
        } else {
          vars <- "all.factors"
        }
      }
    }

    if (is.null(tau2)) {
      weights <- 1 / data[[vi]]
    } else {
      weights <- 1 / (data[[vi]] + tau2)
    }

    plotdat <-
      data.frame(weights = weights / sum(weights), data[c(names(data)[which(names(data) %in% c(yi))], select_vars)])
    names(plotdat)[-c(1, 2)] <-
      paste0("Value.", names(plotdat)[-c(1, 2)])
    plotdat <-
      reshape(
        plotdat,
        varying = 3:ncol(plotdat),
        timevar = "Variable",
        direction = "long"
      )[-5]
    p <-
      ggplot(plotdat,
             aes_string(
               x = "Value",
               y = yi,
               size = "weights",
               weight = "weights"
             ))
    if (vars == "all.continuous") {
      p <- p + geom_point(alpha = .2)
      if (summarize) {
        p <- p + stat_smooth(color = "darkblue", linetype = 2)
      }
    } else {
      if (summarize) {
        p <- p + geom_boxplot(outlier.shape = NA)
      }
      p <- p + geom_jitter(width = .2, alpha = .2)
    }

    p + theme_bw() +
      theme(legend.position = "none") +
      facet_wrap(~ Variable, scales = "free_x") +
      theme(axis.title.x = element_blank())
  }
