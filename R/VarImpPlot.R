#' Plots variable importance for a MetaForest object.
#'
#' @param mf MetaForest object.
#' @param n.var Number of moderators to plot.
#' @param sort Should the moderators be sorted from most to least important?
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
VarImpPlot <- function(mf, n.var = 30, sort = TRUE) {
    if (!inherits(mf, c("MetaForest", "ranger")))
      stop("Argument 'mf' must be an object of class \"MetaForest\" or \"ranger\".")
    if(class(mf) == "MetaForest"){
      ranger_object <- mf$forest
    } else {
      ranger_object <- mf
    }

    var_importance <- mf$forest$variable.importance
    var_importance <- data.frame(variable=names(var_importance), importance=unname(var_importance))
    if(sort){
      var_importance <- var_importance[order(-var_importance$importance),]
    }
    n.var <- min(n.var, nrow(var_importance))
    var_importance <- var_importance[1:n.var, ]

    var_importance <- var_importance[rev(rownames(var_importance)), ]
    var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
    p <- ggplot(var_importance, aes_string(y="variable", x="importance"))+
      geom_segment(aes_string(x=0, xend="importance", y="variable", yend="variable"), colour = "grey50", linetype = 2)+
      geom_vline(xintercept = 0, colour = "grey50", linetype = 1)+
      geom_point(shape=1, size=2) +
      xlab("Variable Importance (Permutation importance)")+
      theme_bw()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.y=element_blank())
    p
}
