#Load and clean data from metafor
data <- dat.bourassa1996
data <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
               data = data, add = 1/2, to = "all")
data$mage[is.na(data$mage)] <- median(data$mage, na.rm = TRUE)
data[c(5:8)] <- lapply(data[c(5:8)], factor)
data$yi <- as.numeric(data$yi)
set.seed(33)

# The problem is that num_tree is a variable.
mf.cluster.b1996 <- MetaForest(formula = yi~ selection + investigator + hand_assess + eye_assess + mage +sex, data = data, study = "sample", whichweights = "unif", num.trees = 300)

test_that("clustermf has the correct number of trees", {expect_equal(300, mf.cluster.b1996$forest$num.trees)})
test_that("Metaforest calls clustermf", {expect_true(inherits(mf.cluster.b1996, "cluster_mf"))})

