#' Returns an rma ModelInfo list for use with caret
#'
#' This function allows users to rely on the powerful \code{caret} package for
#' cross-validating and tuning a rma analysis. Methods for rma are not included
#' in the caret package, because the interface of caret is not entirely
#' compatible with rma's model call. Specifically, rma is not compatible with
#' the \code{train} methods for classes 'formula' or 'recipe'. The  variance of
#' the effect sizes can be passed to the 'weights' parameter of \code{train}.
#'
#' When using clustered data (effect sizes within studies), make sure to use
#' 'index = groupKFold(your_study_id_variable, k = 10))' in traincontrol, to
#' sample by study ID when creating cross-validation partitions; otherwise the
#' testing error will be positively biased.
#' @return ModelInfo list of length 13.
#' @importFrom utils installed.packages
#' @export
#' @examples
#' \dontrun{
#' # Prepare data
#' dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
#' dat$yi <- as.numeric(dat$yi)
#' dat$alloc <- factor(dat$alloc)
#' # Run rma
#' rma.model <- rma(y = dat$yi, mods = dat[, c("ablat", "year")], vi = dat$vi)
#' # R^2 is estimated to be .64
#' rma.model$R2
#' # Now, use cross-validation to see how well this model generalizes
#' # Leave-one-out cross-validation is more appropriate than 10-fold cv because
#' # the sample size is very small
#' fit_control <- trainControl(method = "LOOCV")
#' # Train the model without tuning, because rma has no tuning parameters
#' cv.mf.cluster <- train(y = dat$yi, x = dat[, c("ablat", "year")],
#'                        weights = dat$vi,
#'                        method = ModelInfo_rma(),
#'                        trControl = fit_control)
#' # Cross-validated R^2 is .08, suggesting substantial overfitting of the
#' # original rma model
#' cv.mf.cluster$results$Rsquared
#' }
ModelInfo_rma <- function(){
  if(!"caret" %in% utils::installed.packages()[,"Package"]){
    stop("This function requires the R-package 'caret' to be installed.")
  }
  return(eval(parse(text = "list(label = 'Meta-Analysis via Mixed-Effects Models',
                  library = 'metafor',
                    loop = NULL,
                    type = 'Regression',
                    parameters = data.frame(parameter = 'method',
                    class = 'character',
                    label = 'Which method to use'),
                    grid = function(x, y, len = NULL, search = 'grid')
                    data.frame(method = 'REML'),
                    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(is.null(wts))
                    {
                    stop(\"Tuning an rma() model requires passing a vector of sampling variances to the 'weights' parameter when calling train().\")
                    }
                    dat <- if(is.matrix(x)) x else as.matrix(x)
                    out <- rma(yi = y, vi = wts, mods = x, method = param$method, ...)
                    out
                    },
                    predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    predict.rma(modelFit, newdata)$pred
                    },
                    prob = NULL,
                    predictors = function(x, ...) rownames(x$b)[-match('intrcpt', rownames(x$b))],
                    tags = c('Meta-analysis', 'Linear Regression', 'Accepts Case Weights'),
                    varImp = function(object, ...) {
                    values <- object$zval
                    varImps <-  abs(values[-1])
                    out <- data.frame(varImps)
                    colnames(out) <- 'Overall'
                    rnames <- rownames(object$b)[-match('intrcpt', rownames(object$b))]
                    if(!is.null(rnames)) rownames(out) <- rnames
                    out
                    },
                    sort = function(x) x)")))
}
