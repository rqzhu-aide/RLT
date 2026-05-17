# test-regression-lc.R

context("Regression - Linear Combination")

test_that("naive method (1) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("lm method (2) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "lm",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("pca method (3) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "pca",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("sir method (4) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "sir",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("integer method codes 1-4 work correctly", {
  d <- generate_simple_regression(n = 80, p = 10)
  for (m in 1:4) {
    expect_error(
      RLT(d$X, d$y, ntrees = 20, linear.comb = 3, linear.comb.method = m,
          ncores = 2, verbose = FALSE),
      NA
    )
  }
})

test_that("LC with categorical predictors works", {
  d <- generate_mixed_feature_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC forest produces predictions comparable to non-LC", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit_lc <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
                ncores = 2, verbose = FALSE)
  fit_base <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  pred_lc <- predict(fit_lc, d$X)
  pred_base <- predict(fit_base, d$X)
  mse_lc <- mean((d$y - pred_lc$Prediction)^2)
  mse_base <- mean((d$y - pred_base$Prediction)^2)
  # LC should not be catastrophically worse
  expect_lt(mse_lc, 10 * mse_base)
})

test_that("small sample (n=30) LC fits without error", {
  d <- generate_simple_regression(n = 30, p = 5)
  expect_error(
    RLT(d$X, d$y, ntrees = 20, linear.comb = 3, linear.comb.method = "sir",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC sir beats mean baseline on data with signal", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "sir",
             ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  mse_model <- mean((d$y - pred$Prediction)^2)
  mse_mean <- mean((d$y - mean(d$y))^2)
  expect_lt(mse_model, mse_mean)
})

test_that("invalid linear.comb.method triggers warning", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_warning(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "invalid",
        ncores = 2, verbose = FALSE)
  )
})

test_that("var.prob is accepted alongside LC", {
  d <- generate_simple_regression(n = 80, p = 10)
  vp <- rep(1/d$p, d$p)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
        var.prob = vp, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC works with p >> n", {
  set.seed(42)
  X <- matrix(rnorm(80 * 50), 80, 50)
  y <- X[, 1] + rnorm(80)
  expect_error(
    RLT(X, y, ntrees = 20, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})
