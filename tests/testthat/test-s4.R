data <- fukkink_lont
set.seed(53)

check_conv <- MetaForest(yi~.,
                         data = data,
                         study = "id_exp",
                         whichweights = "random",
                         num.trees = 100)


test_that("clustermf has the correct number of trees", {expect_equal(100, check_conv$forest$num.trees)})

test_that("Metaforest calls clustermf", {expect_true(inherits(check_conv, "cluster_mf"))})

test_that("Metaforest has forest", {expect_true(inherits(check_conv$forest, "ranger"))})

#test_that("Metaforest r2oob", {expect_equivalent(-.3327, check_conv$forest$r.squared, tolerance = .0001)})


p <- plot(check_conv)
test_that("Plot is ggplot", {expect_s3_class(p, "ggplot")})

set.seed(347)
mf_rep <- MetaForest(yi~.,
                     data = data,
                     study = "id_exp",
                     whichweights = "random",
                     num.trees = 100)
# Run recursive preselection, store results in object 'preselect'
preselected <- preselect(mf_rep,
                         replications = 10,
                         algorithm = "recursive")

test_that("preselected is mf_preselect", {expect_s3_class(preselected, "mf_preselect")})

p <- plot(preselected)
test_that("Plot is ggplot", {expect_s3_class(p, "ggplot")})


retain_mods <- preselect_vars(preselected, cutoff = .7)
test_that("Which vars are preselected", {expect_true(all(retain_mods %in% names(data)))})

if(requireNamespace("caret", quietly = TRUE)){
library(caret)

grouped_cv <- trainControl(method = "cv",
                           index = groupKFold(data$id_exp, k = 3))

tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 1:2,
                           min.node.size = 1:4)

X <- data[, c("id_exp", "vi", retain_mods)]

# Train the model
set.seed(9648)
mf_cv <- train(y = data$yi,
               x = X,
               study = "id_exp", # Name of the clustering variable
               method = ModelInfo_mf(),
               trControl = grouped_cv,
               tuneGrid = tuning_grid,
               num.trees = 100)

test_that("Tuning works", {expect_equivalent(nrow(mf_cv$results), nrow(tuning_grid))})

test_that("outcome is called .outcome", {expect_equivalent(names(mf_cv$finalModel$data)[1], ".outcome")})

best <- metaforest:::best(mf_cv)
print(best)
test_that("best is unchanged", {expect_true(best[[1]] %in% c("uniform", "fixed-effect", "random-effects"))})

final <- mf_cv$finalModel
# test_that("r2_oob_best unchanged", {expect_equivalent(final$forest$r.squared, 0.0294, tolerance = .0001)})

p <- plot(final)
test_that("Plot is ggplot", {expect_s3_class(p, "ggplot")})

p <- VarImpPlot(final)
test_that("varimpplot correct", {expect_true(all(p$data$Variable %in% retain_mods))})

p <- PartialDependence(final)
test_that("varimpplot correct", {expect_s3_class(p, "gtable")})
}
