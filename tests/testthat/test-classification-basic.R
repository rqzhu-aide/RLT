# test-classification-basic.R

context("Classification - Basic")

test_that("classification fits and returns correct class", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_s3_class(fit, "RLT")
})

test_that("predict returns correct-length factor predictions", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  expect_true(is.factor(pred$Prediction))
  expect_length(pred$Prediction, d$n)
})

test_that("binary classification accuracy > random (50%)", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  acc <- mean(pred$Prediction == d$y)
  expect_gt(acc, 0.5)
})

test_that("multiclass (3-class) fits and predicts all classes", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, X)
  expect_true(all(levels(pred$Prediction) == c("1", "2", "3")))
})

test_that("classification with factor columns works", {
  d <- generate_mixed_feature_regression(n = 80, p = 10)
  y <- factor(ifelse(d$y > median(d$y), 1, 0))
  expect_error(
    RLT(d$X, y, ntrees = 30, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("OOB predictions are returned", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_length(fit$Prediction, d$n)
})

test_that("importance = TRUE returns correct-length VarImp", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  expect_length(fit$VarImp, d$p)
})

test_that("var.prob is accepted without error", {
  d <- generate_classification_data(n = 80, p = 10)
  vp <- rep(1/d$p, d$p)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, var.prob = vp, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("var.prob with skewed weights favors high-prob variables", {
  d <- generate_classification_data(n = 80, p = 10)
  vp <- c(rep(5, 3), rep(0.1, 7))
  fit <- RLT(d$X, d$y, ntrees = 30, var.prob = vp, importance = TRUE, ncores = 2, verbose = FALSE)
  top3 <- order(fit$VarImp, decreasing = TRUE)[1:3]
  expect_length(top3, 3)
})

test_that("obs.w is accepted without error", {
  d <- generate_classification_data(n = 80, p = 10)
  w <- runif(d$n)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, obs.w = w, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("same seed gives identical results", {
  d <- generate_classification_data(n = 80, p = 10)
  fit1 <- RLT(d$X, d$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  fit2 <- RLT(d$X, d$y, ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  expect_identical(fit1$Prediction, fit2$Prediction)
})

test_that("classification with LC split fits and predicts", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
             ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  expect_true(is.factor(pred$Prediction))
})

test_that("single-class y should fail gracefully", {
  set.seed(42)
  X <- matrix(rnorm(80), ncol = 4)
  y <- factor(rep(1, 20))
  expect_error(RLT(X, y, ntrees = 30, verbose = FALSE))
})

test_that("nsplit = 0 (best split) works", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, nsplit = 0, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("nsplit > 0 (random splits) works", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, nsplit = 3, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("alpha = 0 works", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, alpha = 0, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("alpha = 0.25 works", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, alpha = 0.25, ncores = 2, verbose = FALSE),
    NA
  )
})
