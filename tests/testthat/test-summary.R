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

sum <- summary(mf.cluster.b1996)

test_that("Summary contains forest and rma", {expect_s3_class(sum, "summary.MetaForest")})
