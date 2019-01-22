#' Partial dependence plots split by factor moderator
#'
#' Plots partial dependence plots for a MetaForest model object, split by a
#' factor moderating variable.
#'
#' @param x Model object.
#' @param vars Character vector containing the moderator names for which to plot
#' partial dependence plots. If empty, all moderators are plotted.
#' @param moderator Character. The name of a factor moderator variable in the
#' model object provided to \code{x}.
#' @param pi Numeric (0-1). What percentile interval should be plotted for the
#' partial dependence predictions? Defaults to NULL. To obtain a 95\% interval,
#' set to \code{.95}.
#' @param rawdata Logical, indicating whether to plot weighted raw data.
#' Defaults to FALSE. Uses the same weights as the model object passed to the
#' \code{x} argument.
#' @param resolution Integer vector of length two, giving the resolution of the
#' partial predictions. The first element indicates the resolution of the
#' partial predictions; for Monte-Carlo integration, the second element gives
#' the number of rows of the data to be sampled without replacement when
#' averaging over values of the other predictors.
#' @param output Character. What type of output should be returned? Defaults to
#' \code{"plot"}, which returns and plots a gtable object. To obtain a list of
#' \code{ggplot} objects instead, provide the argument \code{"list"}.
#' @param ... Additional arguments to be passed to \code{marginalPrediction}.
#' @return A gtable object or list of ggplot objects, see the \code{output}
#' parameter.
#' @import ggplot2
#' @importFrom mmpf marginalPrediction
#' @export
#' @examples
#' # Partial dependence plot for MetaForest() model:
#' set.seed(42)
#' data <- SimulateSMD(k_train = 100, model = es * x[, 1] + es * x[, 2] + es *
#'                                            x[, 1] * x[, 2])$training
#' data$X2 <- cut(data$X2, breaks = 2, labels = c("Low", "High"))
#' mf.random <- MetaForest(formula = yi ~ ., data = data,
#'                         whichweights = "random", method = "DL",
#'                         tau2 = 0.2450)
#' # Examine univariate partial dependence plot for all variables in the model:
#' PDP_interaction(mf.random, moderator = "X2")
PDP_interaction <- function(x, vars = NULL, moderator = NULL, pi = NULL, rawdata = FALSE, resolution = NULL, output = "plot", ...) {
  if(!class(moderator) == "character") {
    stop("Moderator must be a character string, corresponding to the name of a variable in the MetaForest analysis.", call. = FALSE)
  }
  if(length(moderator) > 1) {
    stop("Only a single moderator variable can be used.", call. = FALSE)
  }
  if(!moderator %in% names(x$forest$variable.importance)) {
    stop("Moderator must be the name of a variable in the MetaForest analysis.", call. = FALSE)
  }

  if(is.null(vars)){
    select_vars <- names(x$forest$variable.importance)[-match(moderator, names(x$forest$variable.importance))]
  } else {
    select_vars <- vars[which(vars %in% names(x$forest$variable.importance)[-match(moderator, names(x$forest$variable.importance))])]
  }
  cases <- nrow(x$data)

  numeric_vars <-
    which(sapply(x$data[select_vars], class) %in% c("numeric", "integer"))

  if(is.null(resolution)){
    resolution <- c(min(nrow(unique(x$data[, select_vars, drop = FALSE])), 25L), cases)
  } else {
    if(resolution[2] > cases){
      warning("Second element of resolution cannot exceed the number of rows of the data. Set to ", cases, call. = FALSE)
      resolution[2] <- cases
    }
  }

  pd <- create_partial_preds(x$data, resolution, x$forest, pi, select_vars, moderator)
  raw.data <- data.frame(x$data, wi = x$weights)
  # Generate list of plots
  plots <- create_plotlist_mod(pd, raw.data, rawdata)

  if(output == "list") return(plots)
  merge_plots(plots)
}

merge_plots <- function(plots){
  target <- plots[[1]]$labels$y
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

  left <- textGrob(target, rot = 90, just = c(.5, .5))
  gt <- gtable_add_cols(gt, widths = grobWidth(left)+ unit(0.5, "line"), 0)
  gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt),
                        l = 1, r = 1, z = Inf)
  gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))

  grid.newpage()
  grid.draw(gt)
  invisible(gt)
}


create_plotlist_mod <- function(pd, raw.data, rawdata){
  # Calculate y limits of the plot
  plot_pi <- sum(grepl("^preds\\.\\d+$", names(pd[[1]]))) == 2
  if(!plot_pi & !rawdata){
    y_limits <- range(unlist(lapply(pd, `[`, j = "preds")))
  } else {
    y_limits <- range(raw.data[[1]])
  }

  lapply(1:length(pd), function(.thisgrob){
    .plot <- data.frame(pd[[.thisgrob]], Variable = names(pd[[.thisgrob]])[1])

    p <- ggplot(.plot, aes_string(x = names(.plot)[1],
                                    y = names(.plot)[3],
                                    group = names(.plot)[2],
                                    colour = names(.plot)[2],
                                    fill = names(.plot)[2])) +
      facet_wrap("Variable") +
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            plot.margin = unit(rep(.2, 4), "line"))+
      scale_y_continuous(limits = y_limits)+
      ylab(names(raw.data)[1])

    if(class(.plot[[1]]) %in% c("numeric", "integer")){
      p <- p + geom_line() + scale_x_continuous(expand = c(0,0))
      if(plot_pi){
        p <- p + geom_ribbon(aes_string(ymin = names(.plot)[ncol(.plot)-2],
                                        ymax = names(.plot)[ncol(.plot)-1]),
                             alpha = .2)
      }
      if(rawdata){
        p <- p + geom_point(data = raw.data,
                            alpha=.1,
                            aes_string(x = names(.plot)[1],
                                       y = names(raw.data)[1],
                                       size = "wi"))
      }
    } else {
      p <- p + geom_point(size = 5,
                          shape = 18,
                          position = position_dodge(width = .5))
      if(plot_pi){
        p <- p + geom_errorbar(aes_string(
          ymin = names(.plot)[ncol(.plot)-2],
          ymax =  names(.plot)[ncol(.plot)-1]),
          width = .4,
          position = position_dodge(width = .5))
      }
      if(rawdata){
        p <- p + geom_jitter(data = raw.data,
                             position = position_jitterdodge(jitter.width = .2, jitter.height = 0,
                                                             dodge.width = .5),
                             alpha= .1,
                             aes_string(x = names(.plot)[1],
                                        y = names(raw.data)[1],
                                        size = "wi"))
      }
    }
    p
  })
}


create_partial_preds <- function(data, resolution, forest, pi, select_vars, moderator = NULL){
  args <- list(
    "data" = data,
    "n" = resolution,
    "model" = forest,
    "predict.fun" = function(object, newdata) {
      predict(object, data = newdata)$predictions
    }
  )
  if (!is.null(pi)){
    args[["aggregate.fun"]] <- function(x){ c(sum(x)/length(x), quantile(x, c((.5 * (1 - pi)), 1 - (.5 * (1 - pi))))) }
  }
  lapply(select_vars, function(.pred) {
    args[["vars"]] <- c(.pred, moderator)
    dat <- do.call(
      "marginalPrediction",
      args)
    names(dat) <- gsub("(%|(?<=^preds)1$)", "", names(dat), perl = TRUE)
    dat
  })
}
