#' Partial dependence plots
#'
#' Plots partial dependence plots (predicted effect size as a function of the
#' value of each predictor variable) for a MetaForest- or rma model object. For
#' rma models, it is advisable to mean-center numeric predictors, and to not
#' include plot_int effects, except when the rma model is bivariate, and the
#' \code{plot_int} argument is set to \code{TRUE}.
#'
#' @title PartialDependence: Partial dependence plots
#' @param x Model object.
#' @param vars Character vector containing the moderator names for which to plot
#' partial dependence plots. If empty, all moderators are plotted.
#' @param pi Numeric (0-1). What percentile interval should be plotted for the
#' partial dependence predictions? Defaults to NULL. To obtain a 95\% interval,
#' set to \code{.95}.
#' @param rawdata Logical, indicating whether to plot weighted raw data.
#' Defaults to FALSE. Uses the same weights as the model object passed to the
#' \code{x} argument.
#' @param bw Logical, indicating whether the plot should be black and white, or
#' color.
#' @param resolution Integer vector of length two, giving the resolution of the
#' partial predictions. The first element indicates the resolution of the
#' partial predictions; for Monte-Carlo integration, the second element gives
#' the number of rows of the data to be sampled without replacement when
#' averaging over values of the other predictors.
#' @param moderator Atomic character vector, referencing the name of one
#' variable in the model. Results in partial prediction plots, conditional on
#' the moderator. If \code{moderator} references a factor variable, separate
#' lines/boxplots are plotted for each factor level. If \code{moderator}
#' references a numeric variable, heatmaps are plotted - unless the moderator is
#' categorized using the \code{mod_levels} argument.
#' @param mod_levels Vector. If \code{moderator} is continuous, specify
#' thresholds for the \code{\link[base]{cut}} function. The continuous moderator
#' is categorized, and predictions are based on the median moderator value
#' within each category. You can call \code{\link[stats]{quantile}} to cut the
#' moderator at specific quantiles. If \code{moderator} is a factor variable,
#' you can use \code{mod_levels} to specify a character vector with the factor
#' levels to retain in the plot (i.e., dropping the other factor levels).
#' @param output Character. What type of output should be returned? Defaults to
#' \code{"plot"}, which returns and plots a gtable object. To obtain a list of
#' \code{ggplot} objects instead, provide the argument \code{"list"}.
#' @param ... Additional arguments to be passed to \code{marginalPrediction}.
#' @return A gtable object.
#' @import ggplot2
#' @importFrom methods hasArg
#' @examples
#' # Partial dependence plot for MetaForest() model:
#' set.seed(42)
#' data <- SimulateSMD(k_train = 200, model = es * x[, 1] + es * x[, 2] + es *
#'                                            x[, 1] * x[, 2])$training
#' data$X2 <- cut(data$X2, breaks = 2, labels = c("Low", "High"))
#' mf.random <- MetaForest(formula = yi ~ ., data = data,
#'                         whichweights = "random", method = "DL",
#'                         tau2 = 0.2450)
#' # Examine univariate partial dependence plot for all variables in the model:
#' PartialDependence(mf.random, pi = .8)
#' \dontrun{
#' # Examine bivariate partial dependence plot the plot_int between X1 and X2:
#' pd.plot <- PartialDependence(mf.random, vars = c("X1", "X2"), plot_int = TRUE)
#' # Save to pdf file
#' pdf("pd_plot.pdf")
#' grid.draw(pd.plot)
#' dev.off()
#' # Partial dependence plot for metafor rma() model:
#' dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
#' dat$yi <- as.numeric(dat$yi)
#' dat$alloc <- factor(dat$alloc)
#' dat$ablat_d <- cut(dat$ablat, breaks = 2, labels = c("low", "high"))
#' # Demonstrate partial dependence plot for a bivariate plot_int
#' rma.model.int <- rma(yi, vi, mods=cbind(ablat, tpos),
#'                      data=dat, method="REML")
#' PartialDependence(rma.model.int, rawdata = TRUE, pi = .95,
#'                   plot_int = TRUE)
#'
#' # Compare partial dependence for metaforest and rma
#' dat2 <- dat
#' dat2[3:7] <- lapply(dat2[3:7],
#'                     function(x){as.numeric(scale(x, scale = FALSE))})
#' mf.model.all <- MetaForest(yi ~ ., dat2[, c(3:11)])
#' rma.model.all <- rma(dat$yi, dat2$vi,
#'                   mods = model.matrix(yi~., dat2[, c(3:10)])[, -1],
#'                   method="REML")
#' PartialDependence(mf.model.all, rawdata = TRUE, pi = .95)
#' PartialDependence(rma.model.all, rawdata = TRUE, pi = .95)
#' }
#' @export
PartialDependence <-
  function(x,
           vars = NULL,
           pi = NULL,
           rawdata = FALSE,
           bw = FALSE,
           resolution = NULL,
           moderator = NULL,
           mod_levels = NULL,
           output = "plot",
           ...) {
    UseMethod("PartialDependence", x)
  }

#' @method PartialDependence ranger
#' @export
PartialDependence.ranger <-
  function(x,
           vars = NULL,
           pi = NULL,
           rawdata = FALSE,
           bw = FALSE,
           resolution = NULL,
           moderator = NULL,
           mod_levels = NULL,
           output = "plot",
           ...,
           data = NULL) {

    if(is.null(data)) stop("Plotting Partial Dependences for a ranger model requires specifying a 'data' argument, with the data used to build the model.")
    # Functions
    args <- match.call()
    out <- list(forest = x,
                data = data,
                call = as.character(x$call),
                weights = rep(1, nrow(data)))
      class(out) <- "MetaForest"
      args$x <- out
      args[[1]] <- as.name("PartialDependence")
      eval(args, sys.frame(sys.parent()))
    }

#' @method PartialDependence MetaForest
#' @export
PartialDependence.MetaForest <-
  function(x,
           vars = NULL,
           pi = NULL,
           rawdata = FALSE,
           bw = FALSE,
           resolution = NULL,
           moderator = NULL,
           mod_levels = NULL,
           output = "plot",
           ...) {
    # Check input arguments ---------------------------------------------------
    if(hasArg("interaction")){
      stop("The argument 'interaction' has been deprecated, and is replaced by the argument 'moderator'. See ?PartialDependence for help on how to use the 'moderator' argument." )
    }
    if(hasArg("label_elements")){
      label_elements <- eval(match.call()[["label_elements"]])
    } else {
      label_elements <- NULL
    }
    if (is.null(vars)) {
      select_vars <- names(x$forest$variable.importance)
    } else {
      if (!class(vars) == "character") {
        stop("Argument 'vars' must be a character string.", call. = FALSE)
      }
      select_vars <-
        vars[which(vars %in% names(x$forest$variable.importance))]
      if (length(select_vars) == 0)
        stop("Argument 'vars' must be either NULL, or a vector of names of variables in the model.")
    }
    if (anyNA(match(
      c(select_vars, moderator),
      names(x$forest$variable.importance)
    ))) {
      stop(
        "Arguments 'vars' and 'moderator' must correspond to names of variables in the model.",
        call. = FALSE
      )
    }
    if (!is.null(moderator)) {
      select_vars <- select_vars[!select_vars == moderator]
      if (!class(moderator) == "character") {
        stop(
          "Moderator must be a character string, corresponding to the name of a variable in the MetaForest analysis.",
          call. = FALSE
        )
      }
      if (length(moderator) > 1) {
        stop("Only a single moderator variable can be used.", call. = FALSE)
      }
      if (!is.null(mod_levels)) {
        if (inherits(x$data[[moderator]], c("factor", "character"))) {
          if ((!inherits(mod_levels, "character")) |
              anyNA(match(mod_levels, unique(x$data[[moderator]])))) {
            stop(
              "Argument 'mod_levels' does not correspond to the levels of the column of data indicated by 'moderator'."
            )
          }
        } else {
          if ((!inherits(mod_levels, c("numeric", "integer"))) |
              any(!c(mod_levels > min(x$data[[moderator]]), mod_levels < max(x$data[[moderator]])))) {
            stop(
              "Argument 'mod_levels' must be a numeric vector with values within the range of 'moderator'."
            )
          }
        }
      }
    }

    plot_pi <- !is.null(pi)
    if (plot_pi) {
      if (!is.numeric(pi) |
          length(pi) > 1)
        stop("Argument 'pi' must be a numeric constant between 0 and 1.")
      if (!(pi < 1 &
            pi > 0))
        stop("Argument 'pi' must have a value between 0 and 1.")
    }

    cases <- nrow(x$data)

    numeric_vars <-
      which(sapply(x$data[select_vars], class) %in% c("numeric", "integer"))

    if (is.null(resolution)) {
      resolution <-
        c(min(nrow(unique(x$data[, select_vars, drop = FALSE])), 25L), cases)
    } else {
      if (resolution[2] > cases) {
        warning(
          "Second element of resolution cannot exceed the number of rows of the data. Set to ",
          cases,
          call. = FALSE
        )
        resolution[2] <- cases
      }
    }



    # Process raw data --------------------------------------------------------

    cont_mod <- FALSE
    raw.data <- data.table(x$data, wi = x$weights)
    if (!is.null(moderator)) {
      if (inherits(x$data[[moderator]], c("numeric", "integer"))) {
        if (is.null(mod_levels)) {
          cont_mod <- TRUE
        } else {
          cont_mod <- FALSE
          tmp <- cut(raw.data[[moderator]],
                     c((min(raw.data[[moderator]]) - .00001), mod_levels, (max(raw.data[[moderator]]) +
                                                                             .00001)))
          raw.data[, (moderator) := tmp]
          mod_levels <-
            tapply(x$data[[moderator]], raw.data[[moderator]], median)
        }
      }
    }

    # Prepare plot data -------------------------------------------------------

    args <- list(
      "data" = x$data,
      "n" = resolution,
      "model" = x$forest,
      "percentile_interval" = pi,
      "moderator" = moderator,
      "mod_levels" = mod_levels
    )

    pd <- lapply(select_vars, function(.pred) {
      args[["vars"]] <- c(.pred, moderator)
      do.call("create_marginal_preds",
              args)
    })

    # Generate list of plots
    plots <-
      create_plotlist(
        pd,
        raw.data,
        rawdata,
        plot_pi,
        plot_int = !is.null(moderator),
        cont_mod = cont_mod,
        bw = bw,
        label_elements = label_elements
      )
    if (output == "list")
      return(plots)
    merge_plots(plots)
  }



create_plotlist <-
  function(pd,
           raw.data,
           rawdata,
           plot_pi,
           plot_int,
           cont_mod,
           mod_levels,
           bw,
           label_elements) {
    rename_labels <- ifelse(is.null(label_elements), FALSE, TRUE)
    if (!(plot_int & cont_mod)) {
      if (rawdata) {
        y_limits <- range(raw.data[[1]])
      } else {
        if (plot_pi) {
          y_limits <-
            range(unlist(lapply(pd, function(x) {
              x[, range(unlist(.SD)), .SDcols = c("lower", "upper")]
            })))
        } else {
          y_limits <-
            range(unlist(lapply(pd, function(x) {
              x[, range(unlist(.SD)), .SDcols = "preds"]
            })))
        }
      }
    }

    lapply(1:length(pd), function(.thisgrob) {
      .plot <- pd[[.thisgrob]]
      if(!rename_labels){
        .plot[, ("Variable") := names(pd[[.thisgrob]])[1]]
      } else {
        .plot[, ("Variable") := rename_fun(names(pd[[.thisgrob]])[1], names(label_elements), label_elements)]
      }
      if (plot_int) {
        if (cont_mod) {
          # Heatmap
          p <- ggplot(.plot, aes_string(
            x = names(.plot)[1],
            y = names(.plot)[2],
            fill = "preds"
          )) +
            geom_raster() +
            scale_y_continuous(expand = c(0, 0)) +
            ylab(names(.plot)[2])
          if (bw) {
            p <-
              p + scale_fill_gradient(name = names(raw.data)[1],
                                      low = "white",
                                      high = "black")

          } else {
            p <-
              p + #scale_fill_gradient(name = names(raw.data)[1])
              scale_fill_gradient2(name = names(raw.data)[1],
                                   low = "blue",
                                   mid = "white",
                                   high = "red")
          }
        } else {
          # Grouped line plot
          if (bw) {
            p <- ggplot(
              .plot,
              aes_string(
                x = names(.plot)[1],
                y = names(.plot)[3],
                group = names(.plot)[2],
                linetype = names(.plot)[2],
                shape = names(.plot)[2]
              )
            ) +
              scale_y_continuous(limits = y_limits) +
              ylab(names(raw.data)[1])
          } else {
            p <- ggplot(
              .plot,
              aes_string(
                x = names(.plot)[1],
                y = names(.plot)[3],
                group = names(.plot)[2],
                colour = names(.plot)[2],
                fill = names(.plot)[2]
              )
            ) +
              scale_y_continuous(limits = y_limits) +
              ylab(names(raw.data)[1])
          }
        }

      } else {
        # Single line plot
        if (bw) {
          p <- ggplot(.plot, aes_string(x = names(.plot)[1],
                                        y = names(.plot)[2])) +
            scale_y_continuous(limits = y_limits) +
            ylab(names(raw.data)[1])
        } else {
          p <- ggplot(.plot,
                      aes_string(
                        x = names(.plot)[1],
                        y = names(.plot)[2],
                        group = 1,
                        colour = 1,
                        fill = 1
                      )) +
            scale_color_continuous(guide = "none") +
            scale_fill_continuous(guide = "none") +
            scale_y_continuous(limits = y_limits) +
            ylab(names(raw.data)[1])
        }

      }
      p <- p +
        facet_wrap("Variable") +
        theme_bw() +
        theme(
          legend.direction = "vertical",
          legend.box = "horizontal",
          legend.position = c(1, .997),
          legend.justification = c(1, 1),
          axis.title.x = element_blank(),
          plot.margin = unit(rep(.2, 4), "line")
        )

      if (inherits(.plot[[1]], c("numeric", "integer"))) {
        if (!cont_mod) {
          # Add shaded confidence ribbon
          if (plot_pi) {
            p <- p + geom_ribbon(aes_string(ymin = names(.plot)[ncol(.plot) - 2],
                                            ymax = names(.plot)[ncol(.plot) -
                                                                  1]),
                                 alpha = .2)
          }
          # Add points
          if (rawdata) {
            p <- p + geom_point(
              data = raw.data,
              alpha = .1,
              aes_string(
                x = names(.plot)[1],
                y = names(raw.data)[1],
                size = "wi"
              )
            ) +
              scale_size_continuous(guide = "none")
          }
          # Add line
          p <-
            p + geom_line(size = 1) + scale_x_continuous(expand = c(0, 0))
        } else {
          p <- p + scale_x_continuous(expand = c(0, 0))
        }
      } else {
        if (!cont_mod) {
          # Add errorbars
          if (plot_pi) {
            p <- p + geom_errorbar(
              aes_string(ymin = names(.plot)[ncol(.plot) - 2],
                         ymax =  names(.plot)[ncol(.plot) - 1]),
              width = .4,
              position = position_dodge(width = .5)
            )
          }
          # Add jittered points
          if (rawdata) {
            p <- p + geom_jitter(
              data = raw.data,
              position = position_jitterdodge(
                jitter.width = .2,
                jitter.height = 0,
                dodge.width = .5
              ),
              alpha = .1,
              aes_string(
                x = names(.plot)[1],
                y = names(raw.data)[1],
                size = "wi"
              )
            )
          }
          # Add clustered centroids
          p <- p + geom_point(
            size = 5,
            shape = 18,
            position = position_dodge(width = .5)
          ) +
            scale_size_continuous(guide = "none")
        } else {
          # If cont_mod, just fix X scale limits
          p <- p + scale_x_discrete(expand = c(0, 0))
        }
      }
      p
    })
  }



#' @import grid gtable
merge_plots <- function(plots, ...){
  args <- match.call()
  if(!("ylab" %in% names(args))){
    ylab <- plots[[1]]$labels$y
  }
  n_grobs <- length(plots)
  grob_rows <- floor(sqrt(n_grobs))
  grob_cols <- ceiling(sqrt(n_grobs))
  if((grob_rows*grob_cols) < n_grobs){
    grob_rows <- grob_rows + 1
  }
  for(x in 1:length(plots)){
    if(!(x %in% seq.int(1, n_grobs, by = grob_cols))){
      plots[[x]] <- plots[[x]] + theme(axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank())
    }
    if(!x == grob_cols){
      plots[[x]] <- plots[[x]] + theme(legend.position = "none")
    }
    plots[[x]] <- suppressMessages(ggplotGrob(plots[[x]]+theme(axis.title.y = element_blank())))
    if(x > 1) plots[[x]]$widths <- plots[[1]]$widths
  }

  if(n_grobs < (grob_cols * grob_rows)){
    plots[(length(plots)+1):(grob_cols * grob_rows)] <- lapply((length(plots)+1):(grob_cols * grob_rows), function(x){nullGrob()})
  }

  gt <- gtable_matrix("partial.dependence",
                      matrix(plots, nrow = grob_rows, byrow = TRUE),
                      widths = unit(rep(1, grob_cols), "null"),
                      heights = unit(rep(1, grob_rows), "null"))

  left <- textGrob(ylab, rot = 90, just = c(.5, .5))
  gt <- gtable_add_cols(gt, widths = grobWidth(left)+ unit(0.5, "line"), 0)
  gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt),
                        l = 1, r = 1, z = Inf)
  gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))

  grid.newpage()
  grid.draw(gt)
  invisible(gt)
}
