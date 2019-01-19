#' Returns a MetaForest ModelInfo list for use with caret
#'
#' This function allows users to rely on the powerful \code{caret} package for
#' cross-validating and tuning a MetaForest analysis. Methods for MetaForest
#' are not included in the caret package, because the interface of caret is not
#' entirely compatible with MetaForest's model call. Specifically, MetaForest is
#' not compatible with the \code{train} methods for classes 'formula' or
#' 'recipe', because the variance of the effect size must be a column of the
#' training data x. The name of this column is specified using the argument
#' 'vi'.
#'
#' To train a clustered MetaForest (\code{clusterMF}), simply provide the
#' optional argument 'study' to the train function, to specify the study ID.
#' This should again refer to a column of x.
#'
#' When training a clustered MetaForest, make sure to use
#' 'index = groupKFold(your_study_id_variable, k = 10))' in traincontrol, to
#' sample by study ID when creating cross-validation partitions; otherwise the
#' testing error will be positively biased.
#' @return ModelInfo list of length 17.
#' @importFrom utils installed.packages
#' @export
#' @examples
#' \dontrun{
#' # Prepare data
#' data <- dat.bangertdrowns2004
#' data[, c(4:12)] <- apply(data[ , c(4:12)], 2, function(x){
#'   x[is.na(x)] <- median(x, na.rm = TRUE)
#'   x})
#' data$subject <- factor(data$subject)
#' data$yi <- as.numeric(data$yi)
#' # Load caret
#' library(caret)
#' set.seed(999)
#' # Specify the resampling method as 10-fold CV
#' fit_control <- trainControl(method = "cv", number = 10)
#' cv_mf_fit <- train(y = data$yi, x = data[,c(3:13, 16)],
#'                    method = ModelInfo_mf(), trControl = fit_control)
#'
#'
#' # Cross-validated clustered MetaForest
#' data <- get(data(dat.bourassa1996))
#' data <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
#'                data = data, add = 1/2, to = "all")
#' data$mage[is.na(data$mage)] <- median(data$mage, na.rm = TRUE)
#' data[c(5:8)] <- lapply(data[c(5:8)], factor)
#' data$yi <- as.numeric(data$yi)
#' # Set up 10-fold grouped CV
#' fit_control <- trainControl(method = "cv", index = groupKFold(data$sample,
#'                             k = 10))
#' # Set up a custom tuning grid for the three tuning parameters of MetaForest
#' rf_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
#'                        mtry = c(2, 4, 6),
#'                        min.node.size = c(2, 4, 6))
#' # Train the model
#' cv.mf.cluster <- train(y = data$yi, x = data[, c("selection", "investigator",
#'                                                  "hand_assess", "eye_assess",
#'                                                  "mage", "sex", "vi",
#'                                                  "sample")],
#'                        study = "sample", method = ModelInfo_mf(),
#'                        trControl = fit_control,
#'                        tuneGrid = rf_grid)
#' }
ModelInfo_mf <- function(){
  if(!"caret" %in% utils::installed.packages()[,"Package"]){
    stop("This function requires the R-package 'caret' to be installed.")
  }
  return(eval(parse(text = "list(label = 'MetaForest',
              library = c('metaforest'),

                    check = function(pkg) {
                    requireNamespace('metaforest')
                    current <- packageDescription('metaforest')$Version
                    expected <- '0.1.3'
                    if(compareVersion(current, expected) < 0)
                    stop('This modeling workflow requires metaforest version ',
                    expected, 'or greater.', call. = FALSE)
                    },
                    loop = NULL,
                    type = 'Regression',
                    parameters = data.frame(parameter = c('whichweights', 'mtry', 'min.node.size'),
                    class = c('character', 'numeric', 'numeric'),
                    label = c('Type of weights to use', '#Randomly Selected Predictors',
                    'Minimal Node Size')),

                    grid = function(x, y, len = NULL, search = 'grid') {
                    if(search == 'grid') {
                      expand.grid(whichweights = 'random',
                      mtry = caret::var_seq(p = (ncol(x)-2),
                      classification = FALSE,
                      len = len),
                      min.node.size = 5)
                    } else {
                      data.frame(
                      whichweights = 'random',
                      mtry = sample(1:ncol(x), size = len, replace = TRUE),
                      min.node.size = sample(1:(min(20,nrow(x))),
                      size = len, replace = TRUE))
                    }
                    },

                    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    args <- as.list(match.call())[-1]
                    x <- data.frame(.outcome = y, x)
                    args[['data']] <- x
                    args[['whichweights']] <- levels(param$whichweights)[as.numeric(param$whichweights)]
                    args[['mtry']] <- param$mtry
                    args[['min.node.size']] <- param$min.node.size
                    args[['formula']] <- as.formula('.outcome ~ .')
                    args[['x']] <- NULL
                    args[['y']] <- NULL
                    args[['wts']] <- NULL
                    args[['param']] <- NULL
                    args[['lev']] <- NULL
                    args[['last']] <- NULL
                    args[['classProbs']] <- NULL
                    do.call(metaforest::MetaForest, args)
                    },

                    predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)$predictions
                    },

                    prob = NULL,
                    predictors = function(x, ...) {
                    var_index <- sort(unique(unlist(lapply(x$forest$forest$split.varIDs, function(x) x))))
                    var_index <-var_index[var_index > 0]
                    x$forest$forest$independent.variable.names[var_index]
                    },

                    varImp = function(object, ...){
                      if(length(object$forest$variable.importance) == 0)
                      stop('No importance values available')
                      imps <- object$forest$variable.importance
                      out <- data.frame(Overall = as.vector(imps))
                      rownames(out) <- names(imps)
                      out
                    },

                    levels = function(x) {
                      NULL
                    },

                    oob = function(x) {
                      postResample(x$predictions, x$y)
                    },
                    tags = c('Random Forest', 'Meta-Analysis', 'Ensemble Model', 'Bagging', 'Implicit Feature Selection', 'Accepts Case Weights'),
                    sort = function(x){ x[order(x[,1]),] },
                    notes = \"MetaForest does not use the train methods for classes 'formula' or 'recipe', because the variance of the effect size must be a column of the training data x. The name of this column is specified by providing the argument 'vi' when calling 'train'. To train a clustered MetaForest, simply provide the optional argument 'study' to specify the study ID. This should again refer to a column of x. When training a clustered MetaForest, make sure to use 'index = groupKFold(your_study_id_variable, k = 10))' in traincontrol, to sample by study ID when creating cross-validation partitions; otherwise the testing error will be positively biased.\")")))
}
