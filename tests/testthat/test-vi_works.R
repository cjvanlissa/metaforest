library(metaforest)

test_that("vi works", {
  expect_error({
    MetaForest(yi~.,
               data = fukkink_lont,
               vi = "vi",
               study = "id_exp",
               whichweights = "random",
               num.trees = 20000)
  }, NA)
})
