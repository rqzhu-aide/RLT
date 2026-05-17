# test-regression-basic.R

context("Regression - Basic")

test_that("regression fits and returns correct class", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_s3_class(fit, "RLT")
})

test_that("predict returns correct-length numeric predictions", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  expect_type(pred$Prediction, "double")
  expect_length(pred$Prediction, d$n)
})

test_that("OOB predictions returned with correct length", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_length(fit$Prediction, d$n)
})

test_that("training predictions correlate with true values", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  cor_val <- cor(fit$Prediction, d$y)
  expect_gt(cor_val, 0)
})

test_that("importance = TRUE returns correct-length VarImp", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  expect_length(fit$VarImp, d$p)
})

test_that("importance = FALSE returns NULL VarImp", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = FALSE, ncores = 2, verbose = FALSE)
  expect_null(fit$VarImp)
})

test_that("importance identifies signal variables", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  top2 <- order(fit$VarImp, decreasing = TRUE)[1:2]
  # X1 and X2 are signal; at least 1 should be in top 2
  expect_gte(sum(c(1, 2) %in% top2), 1)
})

test_that("var.prob is accepted without error", {
  d <- generate_simple_regression(n = 80, p = 10)
  vp <- rep(1 / d$p, d$p)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, var.prob = vp, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("obs.w is accepted without error", {
  d <- generate_simple_regression(n = 80, p = 10)
  w <- runif(d$n)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, obs.w = w, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("same seed gives identical results", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit1 <- RLT(d$X, d$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  fit2 <- RLT(d$X, d$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  expect_identical(fit1$Prediction, fit2$Prediction)
})

test_that("different seeds give different results", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit1 <- RLT(d$X, d$y, ntrees = 30, seed = 1, ncores = 1, verbose = FALSE)
  fit2 <- RLT(d$X, d$y, ntrees = 30, seed = 2, ncores = 1, verbose = FALSE)
  expect_false(identical(fit1$Prediction, fit2$Prediction))
})

test_that("factor columns in X are handled", {
  d <- generate_mixed_feature_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("nsplit = 0 (best split) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, nsplit = 0, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("nsplit > 0 (random splits) works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, nsplit = 3, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("alpha = 0 works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, alpha = 0, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("alpha = 0.25 works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, alpha = 0.25, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("OOB error is finite", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_true(is.finite(fit$Error))
})

test_that("train-test split: test predictions correlate with truth", {
  d <- generate_simple_regression(n = 100, p = 10)
  train_idx <- 1:70
  fit <- RLT(d$X[train_idx, ], d$y[train_idx], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X[-train_idx, ])
  expect_gt(cor(pred$Prediction, d$y[-train_idx]), 0.3)
})

test_that("importance = 'distribute' returns correct-length VarImp", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = "distribute", ncores = 2, verbose = FALSE)
  expect_length(fit$VarImp, d$p)
})

test_that("resample parameters can be set manually", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.replace = FALSE, resample.prob = 0.632,
        ncores = 2, verbose = FALSE),
    NA
  )
})
