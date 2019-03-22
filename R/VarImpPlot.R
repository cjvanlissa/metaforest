#' Plots variable importance for a MetaForest object.
#'
#' @param mf MetaForest object.
#' @param n.var Number of moderators to plot.
#' @param sort Should the moderators be sorted from most to least important?
#' @param ... Parameters passed to and from other functions.
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @examples
#' set.seed(42)
#' data <- SimulateSMD()
#' mf.random <- MetaForest(formula = yi ~ ., data = data$training,
#'                         whichweights = "random", method = "DL",
#'                         tau2 = 0.0116)
#' VarImpPlot(mf.random)
#' VarImpPlot(mf.random, n.var = 2)
#' VarImpPlot(mf.random, sort = FALSE)
VarImpPlot <- function(mf, n.var = 30, sort = TRUE, ...) {
    if (!inherits(mf, c("MetaForest", "ranger")))
      stop("Argument 'mf' must be an object of class \"MetaForest\" or \"ranger\".")
    if(inherits(mf, "MetaForest")){
      ranger_object <- mf$forest
    } else {
      ranger_object <- mf
    }

    var_importance <- mf$forest$variable.importance
    var_importance <- data.frame(Variable=names(var_importance), importance=unname(var_importance))
    if(sort){
      var_importance <- var_importance[order(-var_importance$importance),]
    }
    n.var <- min(n.var, nrow(var_importance))
    var_importance <- var_importance[1:n.var, ]

    var_importance <- var_importance[rev(rownames(var_importance)), ]
    var_importance$Variable <- factor(var_importance$Variable, levels=var_importance$Variable)
    p <- ggplot(var_importance, aes_string(y="Variable", x="importance"))+
      geom_segment(aes_string(x=0, xend="importance", y="Variable", yend="Variable"), colour = "grey50", linetype = 2)+
      geom_vline(xintercept = 0, colour = "grey50", linetype = 1)+
      geom_point(shape=1, size=2) +
      xlab("Variable Importance (Permutation importance)")+
      theme_bw()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.y=element_blank())
    if(hasArg("label_elements")){
      label_elements <- eval(match.call()[["label_elements"]])
      levels(p$data$Variable) <- rename_fun(levels(p$data$Variable), names(label_elements), label_elements)
    }
    p
}
