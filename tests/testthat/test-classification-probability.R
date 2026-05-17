# test-classification-probability.R

context("Classification - Probability Output")

test_that("binary OOB Prob has correct dimensions (n x 2)", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_is(fit$Prob, "matrix")
  expect_equal(dim(fit$Prob), c(d$n, 2))
})

test_that("binary OOB Prob rows sum to 1", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_equal(rowSums(fit$Prob), rep(1, d$n))
})

test_that("binary OOB Prob values are in [0, 1]", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_true(all(fit$Prob >= 0 & fit$Prob <= 1))
})

test_that("binary OOB Prediction agrees with max probability class", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  max_class <- apply(fit$Prob, 1, which.max)
  pred_levels <- levels(d$y)
  expected <- factor(pred_levels[max_class], levels = pred_levels)
  expect_equal(as.character(fit$Prediction), as.character(expected))
})

test_that("binary predict()$Prob has correct dimensions", {
  d <- generate_classification_data(n = 80, p = 10)
  train_idx <- 1:60
  fit <- RLT(d$X[train_idx, ], d$y[train_idx], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X[-train_idx, ])
  expect_equal(dim(pred$Prob), c(20, 2))
})

test_that("binary predict()$Prob rows sum to 1", {
  d <- generate_classification_data(n = 80, p = 10)
  train_idx <- 1:60
  fit <- RLT(d$X[train_idx, ], d$y[train_idx], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X[-train_idx, ])
  expect_equal(rowSums(pred$Prob), rep(1, 20))
})

test_that("binary predict()$Prob values are in [0, 1]", {
  d <- generate_classification_data(n = 80, p = 10)
  train_idx <- 1:60
  fit <- RLT(d$X[train_idx, ], d$y[train_idx], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X[-train_idx, ])
  expect_true(all(pred$Prob >= 0 & pred$Prob <= 1))
})

test_that("binary Prediction matches argmax of Prob", {
  d <- generate_classification_data(n = 80, p = 10)
  train_idx <- 1:60
  fit <- RLT(d$X[train_idx, ], d$y[train_idx], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X[-train_idx, ])
  max_class <- apply(pred$Prob, 1, which.max)
  pred_levels <- levels(d$y)
  expected <- factor(pred_levels[max_class], levels = pred_levels)
  expect_equal(as.character(pred$Prediction), as.character(expected))
})

test_that("3-class OOB Prob has correct dimensions (n x 3)", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_equal(dim(fit$Prob), c(90, 3))
})

test_that("3-class OOB Prob rows sum to 1", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_equal(rowSums(fit$Prob), rep(1, 90))
})

test_that("3-class OOB Prob values are in [0, 1]", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_true(all(fit$Prob >= 0 & fit$Prob <= 1))
})

test_that("3-class OOB Prob is informative (max prob > uniform 1/3)", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  eta <- X[, 1] - X[, 2]
  y <- factor(apply(cbind(eta > 0.5, eta > -0.5), 1, function(r) {
    if (r[1]) 1 else if (r[2]) 2 else 3
  }))
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_gt(mean(apply(fit$Prob, 1, max)), 1/3)
})

test_that("3-class predict()$Prob has correct dimensions", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  fit <- RLT(X[1:70, ], y[1:70], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, X[71:90, ])
  expect_equal(dim(pred$Prob), c(20, 3))
})

test_that("3-class predict()$Prob rows sum to 1", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  fit <- RLT(X[1:70, ], y[1:70], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, X[71:90, ])
  expect_equal(rowSums(pred$Prob), rep(1, 20))
})

test_that("3-class Prediction matches argmax of Prob", {
  set.seed(42)
  X <- matrix(rnorm(90 * 5), 90, 5)
  y <- factor(rep(1:3, each = 30))
  fit <- RLT(X[1:70, ], y[1:70], ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, X[71:90, ])
  max_class <- apply(pred$Prob, 1, which.max)
  expect_equal(as.numeric(pred$Prediction), max_class)
})

test_that("4-class OOB Prob has 4 columns", {
  set.seed(42)
  X <- matrix(rnorm(80 * 5), 80, 5)
  y <- factor(rep(1:4, each = 20))
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_equal(ncol(fit$Prob), 4)
})

test_that("LC classification still returns valid probabilities", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
             ncores = 2, verbose = FALSE)
  expect_equal(dim(fit$Prob), c(d$n, 2))
  expect_equal(rowSums(fit$Prob), rep(1, d$n))
})

test_that("Prob is still returned when importance = TRUE", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  expect_equal(dim(fit$Prob), c(d$n, 2))
})

test_that("well-separated classes produce high-confidence predictions", {
  set.seed(42)
  X <- rbind(matrix(rnorm(40 * 5, mean = 3), 40, 5),
             matrix(rnorm(40 * 5, mean = -3), 40, 5))
  y <- factor(rep(c("A", "B"), each = 40))
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_gt(mean(apply(fit$Prob, 1, max)), 0.9)
})
