#Load and clean data from metafor
data("dat.bourassa1996", package = "metadat")
df <- dat.bourassa1996
df <- escalc(measure = "OR", ai = lh.le, bi = lh.re, ci = rh.le, di= rh.re,
               data = df, add = 1/2, to = "all")
df$mage[is.na(df$mage)] <- median(df$mage, na.rm = TRUE)
df[c(5:8)] <- lapply(df[c(5:8)], factor)
df$yi <- as.numeric(df$yi)
set.seed(33)

# The problem is that num_tree is a variable.
mf.cluster.b1996 <- MetaForest(formula = yi~ selection + investigator + hand_assess + eye_assess + mage +sex, data = df, study = "sample", whichweights = "unif", num.trees = 300)

test_that("clustermf has the correct number of trees", {expect_equal(300, mf.cluster.b1996$forest$num.trees)})
test_that("Metaforest calls clustermf", {expect_true(inherits(mf.cluster.b1996, "cluster_mf"))})

