# Handling rma non-convergence --------------------------------------------

df <- data.frame(
  yi = c(1.30, 1.94, 0.70, 0.36, 1.31, 0.46, 1.24, 0.71, 0.35, 0.77),
  vi = c(0.640, 0.421, 0.992, 0.058, 0.756, 0.634, 0.79, 0.596, 0.457, 0.935),
  x = rep(c("a", "b"), each = 5)
)

test_that("Metaforest warns about non-convergence", {
  expect_warning(res <- MetaForest(yi ~. ,df))
})

suppressWarnings(res <- MetaForest(yi ~. ,df))

test_that("Metaforest warns about non-convergence", {
  expect_equivalent(res$rma_before$tau2, 0, tolerance = .0001)
})

