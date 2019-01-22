<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build
Status](https://travis-ci.org/cjvanlissa/metaforest.svg?branch=master)](https://travis-ci.org/cjvanlissa/metaforest)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

metaforest
==========

The goal of MetaForest is to explore heterogeneity in meta-analytic
data, identify important moderators, and explore the functional form of
the relationship between moderators and effect size. To do so,
MetaForest conducts a weighted random forest analysis, using
random-effects or fixed-effects weights, as in classic meta-analysis, or
uniform weights (unweighted random forest). Simulation studies have
demonstrated that this technique has substantial power to detect
relevant moderators, even in datasets as small as 20 cases (based on
cross-validated *R*<sup>2</sup>). Using a variable importance plot,
important moderators can be identified, and using partial prediction
plots, the shape of the marginal relationship between moderators and
effect size can be visualized. MetaForest can be readily integrated in
classical meta-analytic approaches: If MetaForest is conducted as a
primary analysis, classic meta-analysis can be used to quantify
heterogeneity (in fact, MetaForest by default reports a random-effects
meta-analysis on the raw data, and the residuals of the random forests
analysis), or to provide a simplified representation of the linear
effects of important predictors. Conversely, a theory-driven classical
meta-analysis could be complemented by an exploratory MetaForest
analysis, as a final check to ensure that important moderators have not
been overlooked. We hope that this approach will be of use to
researchers, and that the availability of user-friendly R functions will
facilitate its adoption.

Example
-------

This example demonstrates how one might go about conducting a
meta-analysis using MetaForest:

``` r
#Load metaforest package
library(metaforest)
#> Loading required package: ggplot2
#> Loading required package: metafor
#> Loading required package: Matrix
#> Loading 'metafor' package (version 2.0-0). For an overview 
#> and introduction to the package please type: help(metafor).
#> Loading required package: ranger
#> Loading required package: mmpf

#Simulate a meta-analysis dataset with 20 studies, 1 relevant moderator, and 4 irrelevant moderators
set.seed(42)
data <- SimulateSMD()$training

#Conduct an unweighted MetaForest analysis, to estimate the residual tau2
mf.unif <- MetaForest(formula = yi ~ ., data = data,
                      whichweights = "unif", method = "DL", num.trees = 2000)

#Extract the result of this analysis and print them
results <- summary(mf.unif)
results
#> MetaForest results
#>                                          
#> Type of analysis:              MetaForest
#> Number of studies:             20        
#> Number of moderators:          5         
#> Number of trees in forest:     2000      
#> Candidate variables per split: 2         
#> Minimum terminal node size:    5         
#> OOB prediction error (MSE):    0.1010    
#> R squared (OOB):               0.2987    
#> 
#> Tests for Heterogeneity: 
#>                                tau2   tau2_SE I^2     H^2    Q-test  df
#> Raw effect sizes:              0.0553 0.0486  37.2642 1.5940 30.2857 19
#> Residuals (after MetaForest):  0.0098 0.0334  9.5005  1.1050 20.9946 19
#>                                Q_p   
#> Raw effect sizes:              0.0483
#> Residuals (after MetaForest):  0.3371
#> 
#> 
#> Random intercept meta-analyses:
#>                                Intercept se     ci.lb   ci.ub   p     
#> Raw effect sizes:              -0.2136   0.0875 -0.3851 -0.0421 0.0147
#> Residuals (after MetaForest):  0.0362    0.0719 -0.1048 0.1771  0.6148

#Conduct a weighted MetaForest analysis, using the residual tau2 from the
#unweighted analysis above
mf.random <- MetaForest(formula = yi ~ ., data = data,
                      whichweights = "random", method = "DL", 
                      tau2 = results$rma[2,1],
                      num.trees = 2000)

#Print the result of this analysis
summary(mf.random)
#> MetaForest results
#>                                          
#> Type of analysis:              MetaForest
#> Number of studies:             20        
#> Number of moderators:          5         
#> Number of trees in forest:     2000      
#> Candidate variables per split: 2         
#> Minimum terminal node size:    5         
#> OOB prediction error (MSE):    0.0945    
#> R squared (OOB):               0.3439    
#> 
#> Tests for Heterogeneity: 
#>                                tau2   tau2_SE I^2     H^2    Q-test  df
#> Raw effect sizes:              0.0553 0.0486  37.2642 1.5940 30.2857 19
#> Residuals (after MetaForest):  0.0031 0.0312  3.2046  1.0331 19.6290 19
#>                                Q_p   
#> Raw effect sizes:              0.0483
#> Residuals (after MetaForest):  0.4172
#> 
#> 
#> Random intercept meta-analyses:
#>                                Intercept se     ci.lb   ci.ub   p     
#> Raw effect sizes:              -0.2136   0.0875 -0.3851 -0.0421 0.0147
#> Residuals (after MetaForest):  0.0303    0.0692 -0.1054 0.1660  0.6618
```
