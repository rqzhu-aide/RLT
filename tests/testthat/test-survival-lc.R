# test-survival-lc.R

context("Survival - Linear Combination Splits")

test_that("LC logrank split works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, split.rule = "logrank",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC suplogrank split works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, split.rule = "suplogrank",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC coxgrad split works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, split.rule = "coxgrad",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC default split rule (logrank) works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC LC.method = 'naive' works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC prediction works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, linear.comb = 3, ncores = 2, verbose = FALSE)
  pred <- predict(fit, d$X)
  expect_is(pred$Survival, "matrix")
  expect_equal(nrow(pred$Survival), d$n)
})

test_that("LC stores SplitLoad in forest", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, linear.comb = 3, ncores = 2, verbose = FALSE)
  expect_false(is.null(fit$FittedForest$SplitLoad))
})

test_that("LC reproducibility with same seed", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit1 <- RLT(d$X, d$y, censor = d$censor, model = "survival",
              ntrees = 30, linear.comb = 3, seed = 42, ncores = 1, verbose = FALSE)
  fit2 <- RLT(d$X, d$y, censor = d$censor, model = "survival",
              ntrees = 30, linear.comb = 3, seed = 42, ncores = 1, verbose = FALSE)
  expect_identical(fit1$Prediction, fit2$Prediction)
})

test_that("LC importance works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, linear.comb = 3, importance = TRUE, ncores = 2, verbose = FALSE)
  expect_length(fit$VarImp, d$p)
})

test_that("LC with subsampling works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, resample.replace = FALSE,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC nsplit = 0 works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, nsplit = 0,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC nsplit > 0 works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, nsplit = 3,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("LC with alpha works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, linear.comb = 3, alpha = 0.2,
        ncores = 2, verbose = FALSE),
    NA
  )
})
