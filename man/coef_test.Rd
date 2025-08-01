% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef_test.R
\name{coef_test}
\alias{coef_test}
\title{Test coefficients of a model}
\usage{
coef_test(x, par1, par2, distribution = "pt")
}
\arguments{
\item{x}{A model.}

\item{par1}{Numeric or character. Name or position of the first parameter.}

\item{par2}{Numeric or character. Name or position of the second parameter.}

\item{distribution}{Character. Which distribution to use. Currently, can be
one of \code{c("pt", "pnorm")}, for a t-test or z-test, respectively.
Defaults to "pt".}
}
\value{
Named vector.
}
\description{
Conduct a t-test or z-test for coefficients of a model.
}
\examples{
data("dat.bcg", package = "metadat")
dat <- metafor::escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
res <- metafor::rma(yi, vi, mods = ~alloc-1, data=dat, method="REML")
coef_test(res, 1, 2)
}
