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


#' Report formatted number
#'
#' Report a number, rounded to a specific number of decimals (defaults to two),
#' using C-style formats. Intended for RMarkdown reports.
#' @param x Numeric. Value to be reported
#' @param digits Integer. Number of digits to round to.
#' @param equals Logical. Whether to report an equals (or: smaller than) sign.
#' @return An atomic character vector.
#' @author Caspar J. van Lissa
#' @keywords internal
report <- function(x, digits = 2, equals = TRUE){
  equal_sign <- "= "
  if(x%%1==0){
    outstring <- formatC(x, digits = 0, format = "f")
  } else {
    if(abs(x) <  10^-digits){
      equal_sign <- "< "
      outstring <- 10^-digits
    } else {
      outstring <- formatC(x, digits = digits, format = "f")
    }
  }
  ifelse(equals, paste0(equal_sign, outstring), outstring)
}
