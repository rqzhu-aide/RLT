# test-miscellaneous.R

context("Miscellaneous Features")

# --- Reproducibility ---
test_that("same seed produces identical results across models", {
  d_reg <- generate_simple_regression(n = 80, p = 10)
  fit1 <- RLT(d_reg$X, d_reg$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  fit2 <- RLT(d_reg$X, d_reg$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  expect_identical(fit1$Prediction, fit2$Prediction)
})

test_that("different seeds produce different results", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit1 <- RLT(d$X, d$y, ntrees = 30, seed = 1, ncores = 1, verbose = FALSE)
  fit2 <- RLT(d$X, d$y, ntrees = 30, seed = 2, ncores = 1, verbose = FALSE)
  expect_false(identical(fit1$Prediction, fit2$Prediction))
})

test_that("reproducibility works with linear combinations", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit1 <- RLT(d$X, d$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE,
              linear.comb = 3, linear.comb.method = "naive")
  fit2 <- RLT(d$X, d$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE,
              linear.comb = 3, linear.comb.method = "naive")
  expect_identical(fit1$Prediction, fit2$Prediction)
})

# --- Core counts ---
test_that("single core (ncore = 1) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, ncores = 1, verbose = FALSE),
    NA
  )
})

test_that("all cores (ncore = 0) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, ncores = 0, verbose = FALSE),
    NA
  )
})

test_that("specific core count (ncore = 2) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("multi-core run completes and returns valid predictions", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, seed = 42, ncores = 2, verbose = FALSE)
  expect_length(fit$Prediction, d$n)
  expect_true(is.finite(fit$Error))
})

# --- Kernel ---
test_that("self-kernel matrix computation works", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  K <- forest.kernel(fit, d$X, ncores = 2)
  expect_s3_class(K, "RLT")
  expect_equal(dim(K$Kernel), c(d$n, d$n))
})

test_that("cross-kernel matrix computation works", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X[1:60, ], d$y[1:60], ntrees = 30, ncores = 2, verbose = FALSE)
  K <- forest.kernel(fit, d$X[1:60, ], d$X[61:80, ], ncores = 2)
  expect_equal(dim(K$Kernel), c(60, 20))
})

test_that("kernel properties are correct", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  K <- forest.kernel(fit, d$X, ncores = 2)$Kernel
  # Self-kernel should be symmetric
  expect_equal(K[lower.tri(K)], t(K)[lower.tri(K)])
  # Diagonal should equal ntrees (each obs in exactly 1 node per tree)
  expect_equal(diag(K), rep(30, d$n))
})

test_that("kernel-based prediction", {
  d <- generate_simple_regression(n = 80, p = 10)
  train_idx <- 1:60
  fit <- RLT(d$X[train_idx, ], d$y[train_idx], ntrees = 30, ncores = 2, verbose = FALSE)
  K <- forest.kernel(fit, d$X[train_idx, ], d$X[-train_idx, ], ncores = 2)$Kernel
  kernel_pred <- t(K) %*% d$y[train_idx] / rowSums(t(K))
  direct_pred <- predict(fit, d$X[-train_idx, ])
  # Kernel-weighted prediction should be correlated with direct prediction
  expect_gt(cor(as.vector(kernel_pred), direct_pred$Prediction), 0.5)
})

test_that("classification forest kernel works", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  K <- forest.kernel(fit, d$X, ncores = 2)
  expect_equal(dim(K$Kernel), c(d$n, d$n))
})

test_that("classification cross-kernel works", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X[1:60, ], d$y[1:60], ntrees = 30, ncores = 2, verbose = FALSE)
  K <- forest.kernel(fit, d$X[1:60, ], d$X[61:80, ], ncores = 2)
  expect_equal(dim(K$Kernel), c(60, 20))
})

# --- Small data ---
test_that("reproducibility with very small data", {
  set.seed(42)
  X <- matrix(rnorm(20), 10, 2)
  y <- X[, 1] + rnorm(10)
  fit1 <- suppressWarnings(RLT(X, y, ntrees = 10, seed = 1, ncores = 1, verbose = FALSE))
  fit2 <- suppressWarnings(RLT(X, y, ntrees = 10, seed = 1, ncores = 1, verbose = FALSE))
  expect_identical(fit1$Prediction, fit2$Prediction)
})

test_that("kernel with single tree", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 1, ncores = 2, verbose = FALSE)
  K <- forest.kernel(fit, d$X, ncores = 2)
  expect_equal(dim(K$Kernel), c(d$n, d$n))
})
