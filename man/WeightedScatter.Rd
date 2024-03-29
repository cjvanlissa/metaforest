% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WeightedScatter.R
\name{WeightedScatter}
\alias{WeightedScatter}
\title{Plots weighted scatterplots for meta-analytic data. Can plot effect size as
a function of either continuous (numeric, integer) or categorical (factor,
character) predictors.}
\usage{
WeightedScatter(
  data,
  yi = "yi",
  vi = "vi",
  vars = NULL,
  tau2 = NULL,
  summarize = TRUE
)
}
\arguments{
\item{data}{A data.frame.}

\item{yi}{Character. The name of the column in \code{data} that contains the
meta-analysis effect sizes. Defaults to \code{"yi"}.}

\item{vi}{Character. The name of the column in the \code{data} that contains
the variances of the effect sizes. Defaults to \code{"vi"}. By default,
\code{vi} is used to calculate fixed-effects weights, because fixed effects
weights summarize the data set at hand, rather than generalizing to the
population.}

\item{vars}{Character vector containing the names of specific moderator
variables to plot. When set to \code{NULL}, the default, all moderators
are plotted.}

\item{tau2}{Numeric. Provide an optional value for tau2. If this value is
provided, random-effects weights will be used instead of fixed-effects
weights.}

\item{summarize}{Logical. Should summary stats be displayed? Defaults to
FALSE. If TRUE, a smooth trend line is displayed for continuous variables,
using [stats::loess()] for less than 1000 observations, and [mgcv::gam()] for
larger datasets. For categorical variables, box-and-whiskers plots are
displayed. Outliers are omitted, because the raw data fulfill this function.}
}
\value{
A gtable object.
}
\description{
Plots weighted scatterplots for meta-analytic data. Can plot effect size as
a function of either continuous (numeric, integer) or categorical (factor,
character) predictors.
}
\examples{
\dontrun{
set.seed(42)
data <- SimulateSMD(k_train = 100, model = es * x[, 1] + es * x[, 2] + es *
                      x[, 1] * x[, 2])$training
data$X2 <- cut(data$X2, breaks = 2, labels = c("Low", "High"))
data$X3 <- cut(data$X3, breaks = 2, labels = c("Small", "Big"))
WeightedScatter(data, summarize = FALSE)
WeightedScatter(data, vars = c("X3"))
WeightedScatter(data, vars = c("X1", "X3"))
}
}
