#' @title Test coefficients of a model
#' @description Conduct a t-test or z-test for coefficients of a model.
#' @param x A model.
#' @param par1 Numeric or character. Name or position of the first parameter.
#' @param par2 Numeric or character. Name or position of the second parameter.
#' @param distribution Character. Which distribution to use. Currently, can be
#' one of \code{c("pt", "pnorm")}, for a t-test or z-test, respectively.
#' Defaults to "pt".
#' @return Named vector.
#' @examples
#' dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
#' res <- rma(yi, vi, mods = ~alloc-1, data=dat, method="REML")
#' coef_test(res, 1, 2)
#' @rdname coef_test
#' @export

coef_test <- function(x, par1, par2, distribution = "pt"){
  UseMethod("coef_test", x)
}

#' @method coef_test rma
#' @export
coef_test.rma <- function(x, par1, par2, distribution = "pt"){
  estimate <- coef(x)
  Sigma <- vcov(x)

  Beta1 <- estimate[par1]
  Beta2 <- estimate[par2]
  Var_Beta1 <- Sigma[par1, par1]
  Var_Beta2 <- Sigma[par2, par2]
  Cov_Beta1_Beta2 <- Sigma[par1, par2]
  t_stat <- (Beta1 - Beta2) / sqrt(Var_Beta1 + Var_Beta2 - 2 * Cov_Beta1_Beta2)
  df <- x$k - 2 # Check if this is correct
  switch(distribution,
         pt = {
           p <- 2*pt(abs(t_stat), df, lower.tail = FALSE)
           out <- c("t" = as.vector(t_stat), "df" = df, "p" = as.vector(p))
           },
         {
           p <- 2*pnorm(abs(t_stat), lower.tail = FALSE)
           out <- c("z" = as.vector(t_stat), "df" = NA, "p" = as.vector(p))
           }
         )
  class(out) <- c("coef_test_results", class(out))
  out
}

#' @method print coef_test_results
#' @export
print.coef_test_results <- function(x, digits = 2, ...){
  if(names(x)[1] == "t"){
    cat("t(", x[2], ") ", report(x[1]), ", p ", report(x[3]), sep = "")
  } else {
    cat("z ", report(x[1]), ", p ", report(x[3]), sep = "")
  }
}
