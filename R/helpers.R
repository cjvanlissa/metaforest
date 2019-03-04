rename_fun <- function(x, old, new){
  if(length(x) == 1){
    if(x %in% old){
      new[match(x, old)]
    } else {
      x
    }
  } else {
    x[na.omit(match(old, x))] <- new[old %in% x]
    x
  }

}

best <- function(x){
  if(!inherits(x, "train")) stop("x must be an object of class 'train'.")
  if(!x$modelInfo$label == "MetaForest") stop("The method used to train the model must be MetaForest.")
  x$bestTune$whichweights <- c("uniform", "fixed-effect", "random-effects")[match(x$bestTune$whichweights, c("unif", "fixed", "random"))]
  x$bestTune
}
