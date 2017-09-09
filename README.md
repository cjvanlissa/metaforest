<!-- README.md is generated from README.Rmd. Please edit that file -->
metaforest
==========

The goal of MetaForest is to explore heterogeneity in meta-analytic data, identify important moderators, and explore the functional form of the relationship between moderators and effect size. To do so, MetaForest conducts a weighted random forest analysis, using random-effects or fixed-effects weights, as in classic meta-analysis, or uniform weights (unweighted random forest). Simulation studies have demonstrated that this technique has substantial power to detect relevant moderators, even in datasets as small as 20 cases (based on cross-validated *R*<sup>2</sup>). Using a variable importance plot, important moderators can be identified, and using partial prediction plots, the shape of the marginal relationship between moderators and effect size can be visualized. MetaForest can be readily integrated in classical meta-analytic approaches: If MetaForest is conducted as a primary analysis, classic meta-analysis can be used to quantify heterogeneity (in fact, MetaForest by default reports a random-effects meta-analysis on the raw data, and the residuals of the random forests analysis), or to provide a simplified representation of the linear effects of important predictors. Conversely, a theory-driven classical meta-analysis could be complemented by an exploratory MetaForest analysis, as a final check to ensure that important moderators have not been overlooked. We hope that this approach will be of use to researchers, and that the availability of user-friendly R functions will facilitate its adoption.

Example
-------

This example demonstrates how one might go about conducting a meta-analysis using MetaForest:

``` r
#Load metaforest package
library(metaforest)
#> Loading required package: edarf
#> Loading required package: ggplot2
#> Loading required package: metafor
#> Loading required package: Matrix
#> Loading 'metafor' package (version 2.0-0). For an overview 
#> and introduction to the package please type: help(metafor).
#> Loading required package: ranger

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
#> OOB prediction error (MSE):    0.10      
#> R squared (OOB):               0.30      
#> 
#> Tests for Heterogeneity: 
#>                                tau2 tau2_SE I^2   H^2  Q-test df Q_p 
#> Raw effect sizes:              0.06 0.05    37.26 1.59 30.29  19 0.05
#> Residuals (after MetaForest):  0.01 0.03    9.50  1.10 20.99  19 0.34
#> 
#> 
#> Random intercept meta-analyses:
#>                                Intercept se   ci.lb ci.ub p   
#> Raw effect sizes:              -0.21     0.09 -0.39 -0.04 0.01
#> Residuals (after MetaForest):  0.04      0.07 -0.10 0.18  0.61

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
#> OOB prediction error (MSE):    0.09      
#> R squared (OOB):               0.34      
#> 
#> Tests for Heterogeneity: 
#>                                tau2 tau2_SE I^2   H^2  Q-test df Q_p 
#> Raw effect sizes:              0.06 0.05    37.26 1.59 30.29  19 0.05
#> Residuals (after MetaForest):  0.00 0.03    3.20  1.03 19.63  19 0.42
#> 
#> 
#> Random intercept meta-analyses:
#>                                Intercept se   ci.lb ci.ub p   
#> Raw effect sizes:              -0.21     0.09 -0.39 -0.04 0.01
#> Residuals (after MetaForest):  0.03      0.07 -0.11 0.17  0.66
```
