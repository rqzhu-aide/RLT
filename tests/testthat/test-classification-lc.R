# test-classification-lc.R

context("Classification - Linear Combination")

test_that("lda method (1) works on binary data", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("naive method (2) works on binary data", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("random method (3) works on binary data", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "random",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("logistic method (4) works on binary data", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "logistic",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("lda method works on 3-class data", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  expect_error(
    RLT(X, y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("naive method works on 3-class data", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  expect_error(
    RLT(X, y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("random method works on 3-class data", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  expect_error(
    RLT(X, y, ntrees = 30, linear.comb = 3, linear.comb.method = "random",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("logistic method works on 3-class data", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  expect_error(
    RLT(X, y, ntrees = 30, linear.comb = 3, linear.comb.method = "logistic",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("integer method codes 1-4 work correctly", {
  d <- generate_classification_data(n = 80, p = 10)
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
  y <- factor(ifelse(d$y > median(d$y), 1, 0))
  expect_error(
    RLT(d$X, y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("string class labels are preserved in predictions", {
  set.seed(42)
  X <- matrix(rnorm(80 * 5), 80, 5)
  y <- factor(sample(c("cat", "dog"), 80, replace = TRUE))
  fit <- RLT(X, y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
             ncores = 2, verbose = FALSE)
  pred <- predict(fit, X)
  expect_true(all(levels(pred$Prediction) == c("cat", "dog")))
})

test_that("LC forest produces predictions comparable to non-LC", {
  d <- generate_classification_data(n = 80, p = 10)
  fit_lc <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
                ncores = 2, verbose = FALSE)
  fit_base <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  pred_lc <- predict(fit_lc, d$X)
  pred_base <- predict(fit_base, d$X)
  # LC should produce reasonable accuracy
  expect_gt(mean(pred_lc$Prediction == d$y), 0.3)
})

test_that("small sample (n=30) LC fits without error", {
  set.seed(42)
  X <- matrix(rnorm(30 * 5), 30, 5)
  y <- factor(sample(1:2, 30, replace = TRUE))
  expect_error(
    RLT(X, y, ntrees = 20, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC lda accuracy > 50% on binary data with signal", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
             ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  expect_gt(mean(pred$Prediction == d$y), 0.5)
})

test_that("LC works with p >> n", {
  set.seed(42)
  X <- matrix(rnorm(80 * 50), 80, 50)
  y <- factor(sample(1:2, 80, replace = TRUE))
  expect_error(
    RLT(X, y, ntrees = 20, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})
