#' Partial dependence plots
#'
#' Plots partial dependence plots (predicted effect size as a function of the
#' value of each predictor variable) for a MetaForest- or rma model object. For
#' rma models, it is advisable to mean-center numeric predictors, and to not
#' include interaction effects, except when the rma model is bivariate, and the
#' \code{interaction} argument is set to \code{TRUE}.
#'
#' @title PartialDependence: Partial dependence plots
#' @param x Model object.
#' @param vars Character vector containing the moderator names for which to plot
#' partial dependence plots. If empty, all moderators are plotted.
#' @param interaction Logical, indicating whether a bivariate interaction should
#' be plotted, using a heatmap. Only valid when the number of \code{vars} is 2.
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
#' @param ... Additional arguments to be passed to \code{marginalPrediction}.
#' @return A ggplot object.
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
#' PartialDependence(mf.random)
#' \dontrun{
#' # Examine bivariate partial dependence plot the interaction between X1 and X2:
#' PartialDependence(mf.random, vars = c("X1", "X2"), interaction = TRUE)
#'
#' # Partial dependence plot for metafor rma() model:
#' dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
#' dat$yi <- as.numeric(dat$yi)
#' dat$alloc <- factor(dat$alloc)
#' dat$ablat_d <- cut(dat$ablat, breaks = 2, labels = c("low", "high"))
#' # Demonstrate partial dependence plot for a bivariate interaction
#' rma.model.int <- rma(yi, vi, mods=cbind(ablat, tpos),
#'                      data=dat, method="REML")
#' PartialDependence(rma.model.int, rawdata = TRUE, pi = .95,
#'                   interaction = TRUE)
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
PartialDependence <- function(x, vars = NULL, interaction = FALSE, pi = NULL, rawdata = FALSE, resolution = NULL, ...){
  UseMethod("PartialDependence")
}

#' @export
PartialDependence.MetaForest = function(x, vars = NULL, interaction = FALSE, pi = NULL, rawdata = FALSE, resolution = NULL, ...) {
  if(is.null(vars)){
    select_vars <- names(x$forest$variable.importance)
  } else {
    select_vars <- vars[which(vars %in% names(x$forest$variable.importance))]
  }
  if(interaction & (length(select_vars) > 2 | length(select_vars) == 1)) {
    stop("Only bivariate interactions can be plotted. Argument \'vars\' must be of length 2 when \'interaction = TRUE\'.", call. = FALSE)
  }
  data <- get_all_vars(as.formula(x$call[2]), x$data)
  target <- as.character(as.formula(x$call[2])[2])

  numeric_vars <-
    which(sapply(data[select_vars], class) %in% c("numeric", "integer"))

  if(is.null(resolution)){
    resolution <- c(min(nrow(unique(data[, select_vars, drop = FALSE])), 25L), nrow(data))
  } else {
    if(resolution[2] > nrow(data)){
      warning("Second element of resolution cannot exceed the number of rows of the data. Set to ", nrow(data), call. = FALSE)
      resolution[2] <- nrow(data)
    }
  }
  args <- list(
    "data" = data,
    "n" = resolution,
    "model" = x$forest,
    "predict.fun" = function(object, newdata) {
      predict(object, data = newdata)$predictions
    },
    ...
  )
  if (!is.null(pi) & !interaction){
    args[["aggregate.fun"]] <- function(x){ c(sum(x)/length(x), quantile(x, c((.5 * (1 - pi)), 1 - (.5 * (1 - pi))))) }
  }
  if(interaction) {
    #apply(pd[, c(1, 2)], 2, function(x){ table(diff(x))})
    args[["vars"]] <- select_vars
    pd = do.call("marginalPrediction", args)
  } else {
    pd <- lapply(select_vars, function(.pred) {
      if(!is.null(pi)){
        pred_dat <-
          do.call(
            "marginalPrediction",
            c(args, "vars" = .pred)
          )
        names(pred_dat)[c(3,4)] <- c("lower", "upper")
        pred_dat
      } else {
        do.call(
          "marginalPrediction",
          c(args, "vars" = .pred)
        )
      }
    })

    if(is.null(pi)){
      if(rawdata){
        y_limits <- c(data[[target]], unlist(lapply(pd, '[', j = 2)))
      } else {
        y_limits <- unlist(lapply(pd, '[', j = 2))
      }
    } else {
      if(rawdata){
        y_limits <- c(data[[target]], unlist(lapply(pd, '[', j = c(2, 3, 4))))
      } else {
        y_limits <- unlist(lapply(pd, '[', j = c(2, 3, 4)))
      }
    }

    y_limits <- range(y_limits)
  }

  n_grobs <- length(pd)
  grob_rows <- floor(sqrt(n_grobs))
  grob_cols <- ceiling(sqrt(n_grobs))
  if((grob_rows*grob_cols) < n_grobs){
    grob_rows <- grob_rows + 1
  }

  if(!interaction) { #If no interaction is requested, plot univariate plots
    plots <- lapply(1:n_grobs, function(.thisgrob){
      .plot <- pd[[.thisgrob]]
      p <-
        ggplot(data.frame(.plot, Variable = names(.plot)[1]),
               aes_string(
                 x = names(.plot)[1],
                 y = names(.plot)[2])) +
        facet_wrap("Variable") +
        theme_bw() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(rep(.2, 4), "line"))+
        scale_y_continuous(limits = y_limits)

      if(class(.plot[[1]]) %in% c("numeric", "integer")){
        p <- p + geom_line(aes(group=1)) + scale_x_continuous(expand = c(0,0))
        if(!is.null(pi)){
          p <- p + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = .2)
        }
        if(rawdata){
          raw.data <- data.frame(wi = x$weights, x$data[, c(target, names(.plot)[1])])
          p <- p + geom_point(data = raw.data, alpha=.1, aes_string(x = names(.plot)[1],
                                                                    y = target,
                                                                    size = "wi"))
        }
      } else {
        p <- p + geom_point(size = 5, shape = 18)
        if(!is.null(pi)){
          p <- p + geom_errorbar(aes_string(
            ymin = "lower",
            ymax =  "upper"),
            width = .4)
        }
        if(rawdata){
          raw.data <- data.frame(wi = x$weights, x$data[, c(target, names(.plot)[1])])
          #raw.data[[names(.plot)[1]]] <-
          p <- p + geom_jitter(data = raw.data, width = .2, height = 0, alpha=.1, aes_string(x = names(.plot)[1],
                                                                                 y = target,
                                                                                 size = "wi"))
        }
      }

      if(!(.thisgrob %in% seq.int(1, n_grobs, by = grob_cols))){
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

  } else { #If an interaction plot is requested
    p <- ggplot(pd, aes_string(x = names(pd)[1], y = names(pd)[2], fill = "preds")) +
      geom_raster() +
      scale_fill_gradient(name = target, low = "white", high = "black")
    if(class(pd[[1]]) %in% c("factor", "character")){
      p <- p + scale_x_discrete(expand=c(0,0))
    } else {
      p <- p + scale_x_continuous(expand=c(0,0))
    }
    if(class(pd[[2]]) %in% c("factor", "character")){
      p <- p + scale_y_discrete(expand=c(0,0))
    } else {
      p <- p + scale_y_continuous(expand=c(0,0))
    }
    p + theme_bw()
  }

}

#' @export
PartialDependence.rma = function(x, vars = NULL, interaction = FALSE, pi = NULL, rawdata = FALSE, resolution = NULL, ...) {
  data <- data.frame(yi = as.numeric(x$yi), x$X[, !(colnames(x$X) == "intrcpt")])
  if (is.null(x$tau2)) {
    weights <- 1 / x$vi
  } else {
    weights <- 1 / (x$vi + x$tau2)
  }

  if(is.null(vars)){
    select_vars <- names(data)[-1]
  } else {
    not_exist <- vars[which(!(vars %in% names(data)))]
    if(length(not_exist) > 0){
      warning("The following elements of vars did not occur in rma()$X, and are probably not correct variable names: ", not_exist)
    }
    select_vars <- vars[which(vars %in% names(data))]
  }
  if(length(select_vars) == 0){
    stop("PartialDependence() found no moderators in the rma() model provided.", call. = FALSE)
  }
  if(interaction & (length(select_vars) > 2 | length(select_vars) == 1)) {
    stop("Only bivariate interactions can be plotted. Argument \'vars\' must be of length 2 when \'interaction = TRUE\'.", call. = FALSE)
  }

  factor_vars <- sapply(data[select_vars], function(v){
      tmp <- unique(v)
      length(tmp) - sum(is.na(tmp)) == 2L && all(tmp[1:2] %in% 0:1)
    })
  numeric_vars <- which(!factor_vars)
  factor_vars <- which(factor_vars)

  data[select_vars[factor_vars]] <- lapply(data[select_vars[factor_vars]], factor)
  if(is.null(resolution)){
    resolution <- 100
  } else {
    resolution <- resolution[1]
  }
  if(interaction) {
    mods_list <- as.list(rep(0, ncol(data[-1])))
    names(mods_list) <- names(data)[-1]
    mods_list[select_vars[numeric_vars]] <- lapply(select_vars[numeric_vars], function(.var){
      seq(min(data[[.var]]), max(data[[.var]]), length.out = resolution)
    })
    mods_list[select_vars[factor_vars]] <- lapply(select_vars[factor_vars], function(.var){
      as.numeric(levels(data[[.var]]))
    })
    mods <- expand.grid(mods_list)
    pd <- data.frame(predict.rma(x, newmods = unname(as.matrix(mods))), mods[ , match(select_vars, colnames(mods))])
    # Hier verder
    if(any(select_vars %in% select_vars[-numeric_vars])){
      pd[select_vars[select_vars %in% select_vars[-numeric_vars]]] <- lapply(pd[select_vars[select_vars %in% select_vars[-numeric_vars]]], factor)
    }
  } else {
    if(is.null(pi)){
      .pival <- 95
    } else {
      .pival <- pi*100
    }
    pd <- lapply(select_vars, function(.pred) {
      mods_list <- as.list(rep(0, ncol(data[-1])))
      names(mods_list) <- names(data)[-1]
      if(.pred %in% select_vars[numeric_vars]){
        mods_list[[.pred]] <- seq(min(data[[.pred]]), max(data[[.pred]]), length.out = resolution)
      } else {
        mods_list[[.pred]] <- as.numeric(levels(data[[.pred]]))
      }
      mods <- expand.grid(mods_list)
      data.frame(predict.rma(x, newmods = unname(as.matrix(mods))), mods[ , match(select_vars, colnames(mods))])

      pred_dat <- data.frame(predict.rma(x, newmods = unname(as.matrix(mods)),
                             level = .pival),
                             mods[.pred])
      if(.pred %in% select_vars[factor_vars]){
        pred_dat[[7]] <- factor(pred_dat[[7]])
      }
      pred_dat
    })

    if(is.null(pi)){
      if(rawdata){
        y_limits <- c(data$yi, unlist(lapply(pd, '[', i = 1)))
      } else {
        y_limits <- unlist(lapply(pd, '[', i = 1))
      }
    } else {
      if(rawdata){
        y_limits <- c(data$yi, unlist(lapply(pd, '[', i = c(1, 3, 4))))
      } else {
        y_limits <- unlist(lapply(pd, '[', i = c(1, 3, 4)))
      }
    }

    y_limits <- range(y_limits)
  }

  n_grobs <- length(pd)
  grob_rows <- floor(sqrt(n_grobs))
  grob_cols <- ceiling(sqrt(n_grobs))
  if((grob_rows*grob_cols) < n_grobs){
    grob_rows <- grob_rows + 1
  }

  if(!interaction) { #If no interaction is requested, plot univariate plots
    plots <- lapply(1:n_grobs, function(.thisgrob){

      .plot <- pd[[.thisgrob]]
      p <-
        ggplot(data.frame(.plot, Variable = names(.plot)[7]),
               aes_string(
                 x = names(.plot)[7],
                 y = "pred")) +
        facet_wrap("Variable") +
        theme_bw() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(rep(.2, 4), "line"))+
        scale_y_continuous(limits = y_limits)

      if(.thisgrob %in% numeric_vars){
        p <- p + geom_line(aes(group=1)) + scale_x_continuous(expand = c(0,0))
        if(!is.null(pi)){
          p <- p + geom_ribbon(aes_string(ymin = "ci.lb", ymax = "ci.ub"), alpha = .2)
        }
        if(rawdata){


          raw.data <- data.frame(wi = weights, data[, c("yi", names(.plot)[7])])
          p <- p + geom_point(data = raw.data, alpha=.1, aes_string(x = names(.plot)[7],
                                                                    y = "yi",
                                                                    size = "wi"))
        }
      } else {
        p <- p + geom_point(size = 5, shape = 18)
        if(!is.null(pi)){
          p <- p + geom_errorbar(aes_string(
            ymin = "ci.lb",
            ymax =  "ci.ub"),
            width = .4)
        }
        if(rawdata){
          raw.data <- data.frame(wi = weights, data[, c("yi", names(.plot)[7])])
          raw.data[[3]] <- factor(raw.data[[3]])
          p <- p + geom_jitter(data = raw.data, width = .2, height = 0,
                               alpha=.1, aes_string(x = names(.plot)[7],
                               y = "yi",
                               size = "wi"))
        }
      }

      if(!(.thisgrob %in% seq.int(1, n_grobs, by = grob_cols))){
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

    gt <- gtable_matrix("partial.dependence",
                        matrix(plots, nrow = grob_rows, byrow = TRUE),
                        widths = unit(rep(1, grob_cols), "null"),
                        heights = unit(rep(1, grob_rows), "null"))


    left <- textGrob("yi", rot = 90, just = c(.5, .5))
    gt <- gtable_add_cols(gt, widths = grobWidth(left)+ unit(0.5, "line"), 0)
    gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt),
                          l = 1, r = 1, z = Inf)
    gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))
    grid.newpage()
    grid.draw(gt)

  } else { #If an interaction plot is requested
    p <- ggplot(pd, aes_string(select_vars[1], select_vars[2], fill = "pred")) +
      geom_raster() +
      scale_fill_gradient(name = "yi", low = "white", high = "black")
    if(class(pd[[7]]) %in% c("factor", "character")){
      p <- p + scale_x_discrete(expand=c(0,0))
    } else {
      p <- p + scale_x_continuous(expand=c(0,0))
    }
    if(class(pd[[8]]) %in% c("factor", "character")){
      p <- p + scale_y_discrete(expand=c(0,0))
    } else {
      p <- p + scale_y_continuous(expand=c(0,0))
    }
    p + theme_bw()
  }
}


