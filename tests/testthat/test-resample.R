# test-resample.R

context("Resampling Parameters")

test_that("replacement=TRUE: regression fits and predicts", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.replace = TRUE, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("replacement=TRUE: classification fits and predicts", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.replace = TRUE, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("replacement=TRUE: survival fits and predicts", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, resample.replace = TRUE, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("replacement=TRUE stored in parameters", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, resample.replace = TRUE, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["resample.replace"]], 1)
})

test_that("replacement=FALSE: regression fits and predicts", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.replace = FALSE, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("replacement=FALSE: classification fits and predicts", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.replace = FALSE, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("replacement=FALSE: survival fits and predicts", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, resample.replace = FALSE, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("replacement=FALSE stored in parameters", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, resample.replace = FALSE, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["resample.replace"]], 0)
})

test_that("resample.prob=0.5: regression subsampling works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.prob = 0.5, resample.replace = FALSE,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("resample.prob=0.8: regression subsampling works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.prob = 0.8, resample.replace = FALSE,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("resample.prob=0.632: regression bootstrap works", {
  d <- generate_simple_regression(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.prob = 0.632, resample.replace = TRUE,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("resample.prob=0.5: classification works", {
  d <- generate_classification_data(n = 80, p = 10)
  expect_error(
    RLT(d$X, d$y, ntrees = 30, resample.prob = 0.5, resample.replace = FALSE,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("resample.prob=0.5: survival works", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, resample.prob = 0.5, resample.replace = FALSE,
        ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("bootstrap produces OOB predictions", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, resample.replace = TRUE, resample.prob = 1.0,
             ncores = 2, verbose = FALSE)
  expect_length(fit$Prediction, d$n)
})

test_that("subsampling produces OOB predictions", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, resample.replace = FALSE, resample.prob = 0.5,
             ncores = 2, verbose = FALSE)
  expect_length(fit$Prediction, d$n)
})

test_that("default resample parameters are stored", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_false(is.null(fit$parameters[["resample.replace"]]))
  expect_false(is.null(fit$parameters[["resample.prob"]]))
})
