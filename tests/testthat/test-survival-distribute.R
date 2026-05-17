# test-survival-distribute.R
# Test survival distribute variable importance

context("Survival - distribute importance")

test_that("survival distribute importance produces non-zero VI for important variables", {
  set.seed(42)
  n <- 300
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  true_effect <- X[, 1] + 0.5 * X[, 2]
  hazard <- exp(true_effect)
  surv_time <- rexp(n, rate = hazard)
  censor_time <- rexp(n, rate = median(hazard) * 0.3 / 0.7)
  Y <- pmin(surv_time, censor_time)
  Censor <- as.numeric(surv_time <= censor_time)

  fit <- RLT(X, Y, censor = Censor, model = "survival",
             ntrees = 100, importance = "distribute",
             resample.prob = 0.7, resample.replace = FALSE,
             ncores = 2, verbose = FALSE, seed = 123)

  # X1 and X2 are important
  expect_gt(fit$VarImp[1], 0.01)
  expect_gt(fit$VarImp[2], 0.005)
  # Important vars should rank above noise vars
  expect_gt(fit$VarImp[1], mean(fit$VarImp[3:5]))
  expect_gt(fit$VarImp[2], mean(fit$VarImp[3:5]))
  # Length should match number of predictors
  expect_length(fit$VarImp, p)
})

test_that("survival distribute importance works with replacement sampling", {
  set.seed(42)
  n <- 200
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  true_effect <- X[, 1] + 0.5 * X[, 2]
  hazard <- exp(true_effect)
  surv_time <- rexp(n, rate = hazard)
  censor_time <- rexp(n, rate = median(hazard) * 0.3 / 0.7)
  Y <- pmin(surv_time, censor_time)
  Censor <- as.numeric(surv_time <= censor_time)

  fit <- RLT(X, Y, censor = Censor, model = "survival",
             ntrees = 100, importance = "distribute",
             resample.replace = TRUE,
             ncores = 2, verbose = FALSE, seed = 456)

  expect_gt(fit$VarImp[1], 0.01)
  expect_length(fit$VarImp, p)
})

test_that("survival distribute ranking agrees with permute", {
  set.seed(42)
  n <- 300
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  true_effect <- X[, 1] + 0.5 * X[, 2]
  hazard <- exp(true_effect)
  surv_time <- rexp(n, rate = hazard)
  censor_time <- rexp(n, rate = median(hazard) * 0.3 / 0.7)
  Y <- pmin(surv_time, censor_time)
  Censor <- as.numeric(surv_time <= censor_time)

  fit_perm <- RLT(X, Y, censor = Censor, model = "survival",
                  ntrees = 100, importance = "permute",
                  resample.prob = 0.7, resample.replace = FALSE,
                  ncores = 2, verbose = FALSE, seed = 123)

  fit_dist <- RLT(X, Y, censor = Censor, model = "survival",
                  ntrees = 100, importance = "distribute",
                  resample.prob = 0.7, resample.replace = FALSE,
                  ncores = 2, verbose = FALSE, seed = 123)

  # Both methods should rank X1 and X2 as top 2 variables
  rank_perm <- order(fit_perm$VarImp, decreasing = TRUE)
  rank_dist <- order(fit_dist$VarImp, decreasing = TRUE)

  expect_true(all(rank_perm[1:2] %in% c(1, 2)))
  expect_true(all(rank_dist[1:2] %in% c(1, 2)))
})

test_that("survival distribute with noise data gives near-zero VI", {
  set.seed(99)
  X <- matrix(rnorm(200 * 5), 200, 5)
  Y <- rexp(200, rate = 1)
  Censor <- sample(c(0, 1), 200, replace = TRUE, prob = c(0.3, 0.7))

  fit <- RLT(X, Y, censor = Censor, model = "survival",
             ntrees = 100, importance = "distribute",
             resample.prob = 0.7, resample.replace = FALSE,
             ncores = 2, verbose = FALSE, seed = 789)

  expect_lt(max(abs(fit$VarImp)), 0.05)
})

test_that("survival distribute works with var.mode = 'matched'", {
  set.seed(42)
  X <- matrix(rnorm(200 * 5), 200, 5)
  true_effect <- X[, 1] + 0.5 * X[, 2]
  hazard <- exp(true_effect)
  surv_time <- rexp(200, rate = hazard)
  censor_time <- rexp(200, rate = median(hazard) * 0.3 / 0.7)
  Y <- pmin(surv_time, censor_time)
  Censor <- as.numeric(surv_time <= censor_time)

  fit <- RLT(X, Y, censor = Censor, model = "survival",
             ntrees = 100, importance = "distribute",
             param.control = list(var.mode = "matched"),
             ncores = 2, verbose = FALSE, seed = 111)

  expect_length(fit$VarImp, 5)
  expect_true("VarVI" %in% names(fit))
  expect_length(fit$VarVI, 5)
})

test_that("survival distribute works with linear combination splits", {
  set.seed(42)
  X <- matrix(rnorm(100 * 5), 100, 5)
  true_effect <- X[, 1] + 0.5 * X[, 2]
  hazard <- exp(true_effect)
  surv_time <- rexp(100, rate = hazard)
  censor_time <- rexp(100, rate = median(hazard) * 0.3 / 0.7)
  Y <- pmin(surv_time, censor_time)
  Censor <- as.numeric(surv_time <= censor_time)

  fit <- RLT(X, Y, censor = Censor, model = "survival",
             ntrees = 50, importance = "distribute",
             reinforcement = TRUE, linear.comb = 2,
             ncores = 2, verbose = FALSE, seed = 222)

  expect_length(fit$VarImp, 5)
})
