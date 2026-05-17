# test-survival-basic.R

context("Survival - Basic")

test_that("survival model fits and returns correct class", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  expect_s3_class(fit, "RLT")
})

test_that("survival predict returns Survival matrix", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  expect_is(pred$Survival, "matrix")
  expect_equal(nrow(pred$Survival), d$n)
})

test_that("survival OOB Prediction is a matrix", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  expect_is(fit$Prediction, "matrix")
  expect_equal(nrow(fit$Prediction), d$n)
})

test_that("survival Prediction columns are time grid points", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  expect_true(ncol(fit$Prediction) > 1)
})

test_that("survival predictions are non-negative", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  expect_true(all(pred$Survival >= 0))
})

test_that("survival predict Survival curve is monotonically decreasing", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  for (i in 1:min(5, d$n)) {
    diffs <- diff(pred$Survival[i, ])
    expect_true(all(diffs <= 1e-10))
  }
})

test_that("survival OOB error is finite", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  expect_true(is.finite(fit$Error))
})

test_that("logrank split rule works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, split.rule = "logrank", ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("suplogrank split rule works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, split.rule = "suplogrank", ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("coxgrad split rule works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, split.rule = "coxgrad", ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("nsplit = 0 (best split) works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, nsplit = 0, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("nsplit > 0 (random splits) works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, nsplit = 3, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("same seed gives identical results", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit1 <- RLT(d$X, d$y, censor = d$censor, model = "survival",
              ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  fit2 <- RLT(d$X, d$y, censor = d$censor, model = "survival",
              ntrees = 30, seed = 42, ncores = 1, verbose = FALSE)
  expect_identical(fit1$Prediction, fit2$Prediction)
})

test_that("importance = TRUE returns correct-length VarImp", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  expect_length(fit$VarImp, d$p)
})

test_that("importance = FALSE returns NULL VarImp", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, importance = FALSE, ncores = 2, verbose = FALSE)
  expect_null(fit$VarImp)
})

test_that("var.prob is accepted without error", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  vp <- rep(1 / d$p, d$p)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, var.prob = vp, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("obs.w is accepted without error", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  w <- runif(d$n)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, obs.w = w, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("NA in y produces error", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  d$y[5] <- NA
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, ncores = 2, verbose = FALSE)
  )
})

test_that("NA in censor produces error", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  censor_na <- d$censor
  censor_na[5] <- NA
  expect_error(
    RLT(d$X, d$y, censor = censor_na, model = "survival",
        ntrees = 30, ncores = 2, verbose = FALSE)
  )
})

test_that("Mismatched dimensions produce error", {
  set.seed(42)
  X <- matrix(rnorm(80 * 5), 80, 5)
  y <- rexp(50)
  censor <- rep(1, 50)
  expect_error(
    RLT(X, y, censor = censor, model = "survival",
        ntrees = 30, ncores = 2, verbose = FALSE)
  )
})

test_that("Invalid censor values produce error", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  censor_bad <- sample(c(0, 1, 2), d$n, replace = TRUE)
  expect_error(
    RLT(d$X, d$y, censor = censor_bad, model = "survival",
        ntrees = 30, ncores = 2, verbose = FALSE)
  )
})

test_that("subsample (resample.replace=FALSE) works for survival", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, resample.replace = FALSE, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("survival model has timepoints", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, ncores = 2, verbose = FALSE)
  expect_false(is.null(fit$timepoints))
})
