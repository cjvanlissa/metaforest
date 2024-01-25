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
#' @param vars Character vector containing the names of specific moderator
#' variables to plot. When set to \code{NULL}, the default, all moderators
#' are plotted.
#' @param tau2 Numeric. Provide an optional value for tau2. If this value is
#' provided, random-effects weights will be used instead of fixed-effects
#' weights.
#' @param summarize Logical. Should summary stats be displayed? Defaults to
#' FALSE. If TRUE, a smooth trend line is displayed for continuous variables,
#' using [stats::loess()] for less than 1000 observations, and [mgcv::gam()] for
#' larger datasets. For categorical variables, box-and-whiskers plots are
#' displayed. Outliers are omitted, because the raw data fulfill this function.
#' @return A gtable object.
#' @import ggplot2
#' @importFrom grid nullGrob textGrob grobWidth grid.newpage grid.draw
#' @importFrom gtable gtable_matrix gtable_add_cols gtable_add_grob
#' @export
#' @examples
#' \dontrun{
#' set.seed(42)
#' data <- SimulateSMD(k_train = 100, model = es * x[, 1] + es * x[, 2] + es *
#'                       x[, 1] * x[, 2])$training
#' data$X2 <- cut(data$X2, breaks = 2, labels = c("Low", "High"))
#' data$X3 <- cut(data$X3, breaks = 2, labels = c("Small", "Big"))
#' WeightedScatter(data, summarize = FALSE)
#' WeightedScatter(data, vars = c("X3"))
#' WeightedScatter(data, vars = c("X1", "X3"))
#' }
WeightedScatter <-
  function(data,
           yi = "yi",
           vi = "vi",
           vars = NULL,
           tau2 = NULL,
           summarize = TRUE) {
    select_vars <- names(data)[-which(names(data) %in% c(yi, vi))]
    if(!is.null(vars)){
      select_vars <- vars[which(vars %in% select_vars)]
    }

    numeric_vars <-
          which(sapply(data[select_vars], class) %in% c("numeric", "integer"))

    if (is.null(tau2)) {
      weights <- 1 / data[[vi]]
    } else {
      weights <- 1 / (data[[vi]] + tau2)
    }

    n_grobs <- length(select_vars)
    grob_rows <- floor(sqrt(n_grobs))
    grob_cols <- ceiling(sqrt(n_grobs))
    if((grob_rows*grob_cols) < n_grobs){
      grob_rows <- grob_rows + 1
    }

    plotdat <-
      data.frame(weights = weights / sum(weights), data[, c(yi, select_vars)])

    plots <- lapply(1:n_grobs, function(x){
      current_variable <- select_vars[x]
      p <-
        ggplot(data.frame(plotdat[, c(yi, "weights", current_variable)], Variable = current_variable),
               aes_string(
                 x = current_variable,
                 y = yi,
                 size = "weights",
                 weight = "weights"
               )) +
        facet_wrap("Variable") +
        theme_bw() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(rep(.2, 4), "line"))

      if (summarize) {
        if(current_variable %in% select_vars[numeric_vars]){
          p <- p + geom_smooth(color = "darkblue", linetype = 2)
        } else {
          p <- p + geom_boxplot(outlier.shape = NA)
        }
      }
      if(current_variable %in% select_vars[numeric_vars]){
        p <- p + geom_point(alpha = .2) + scale_x_continuous(expand = c(0,0))
      } else {
        p <- p + geom_jitter(width = .2, alpha = .2)
      }
      if(!(x %in% seq.int(1, n_grobs, by = grob_cols))){
        p <- p + theme(
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()
                  )
      }
      suppressMessages(
        ggplotGrob(p)
      )
    })

    if(n_grobs > 1){
      plots[2:length(plots)] <- lapply(plots[2:length(plots)], function(x){
        x$widths <- plots[[1]]$widths
        x
      })
    }


    if(n_grobs < (grob_cols * grob_rows)){
      plots[(length(plots)+1):(grob_cols * grob_rows)] <- lapply((length(plots)+1):(grob_cols * grob_rows), function(x){nullGrob()})
    }

    gt <- gtable_matrix("weighted.scatterplot",
                        matrix(plots, nrow = grob_rows, byrow = TRUE),
                        widths = unit(rep(1, grob_cols), "null"),
                        heights = unit(rep(1, grob_rows), "null"))


    left <- textGrob(yi, rot = 90, just = c(.5, .5))
    gt <- gtable_add_cols(gt, widths = grobWidth(left)+ unit(0.5, "line"), 0)
    gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt),
                          l = 1, r = 1, z = Inf)
    gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))
    grid.newpage()
    grid.draw(gt)
    invisible(gt)
  }

