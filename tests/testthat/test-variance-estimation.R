# test-variance-estimation.R

context("Variance Estimation")

# --- Regression matched ---
test_that("matched variance: regression predict returns Variance", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_type(pred$Variance, "double")
  expect_length(pred$Variance, d$n)
})

test_that("matched variance: regression Variance is non-negative or NA", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_true(all(pred$Variance >= 0 | is.na(pred$Variance)))
})

test_that("matched variance: regression forces replacement=FALSE and resample.prob=0.5", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  expect_equal(fit$parameters[["resample.replace"]], 0)
  expect_equal(fit$parameters[["resample.prob"]], 0.5)
})

test_that("matched variance: regression VarVI is returned", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  expect_true("VarVI" %in% names(fit))
  expect_length(fit$VarVI, d$p)
})

test_that("matched variance: regression VarVI is numeric", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  expect_type(fit$VarVI, "double")
})

test_that("matched variance: regression predict without var.est has no Variance", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  pred <- predict(fit, d$X)
  expect_null(pred$Variance)
})

# --- Classification matched ---
test_that("matched variance: classification predict returns Variance matrix", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_is(pred$Variance, "matrix")
  expect_equal(nrow(pred$Variance), d$n)
})

test_that("matched variance: classification Variance non-negative or NA", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_true(all(pred$Variance >= 0 | is.na(pred$Variance)))
})

test_that("matched variance: classification VarVI is returned", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  expect_true("VarVI" %in% names(fit))
})

# --- Regression IJ ---
test_that("IJ variance: regression fit stores ObsTrack", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  expect_true("ObsTrack" %in% names(fit))
})

test_that("IJ variance: regression predict returns Variance", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_type(pred$Variance, "double")
  expect_length(pred$Variance, d$n)
})

test_that("IJ variance: regression Variance non-negative or NA", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_true(all(pred$Variance >= 0 | is.na(pred$Variance)))
})

# --- Regression jack ---
test_that("jack variance: regression fit stores ObsTrack", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "jack"))
  expect_true("ObsTrack" %in% names(fit))
})

test_that("jack variance: regression predict returns Variance", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "jack"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_type(pred$Variance, "double")
  expect_length(pred$Variance, d$n)
})

test_that("jack variance: regression Variance non-negative or NA", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "jack"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_true(all(pred$Variance >= 0 | is.na(pred$Variance)))
})

# --- Classification IJ/jack ---
test_that("IJ variance: classification fit stores ObsTrack", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  expect_true("ObsTrack" %in% names(fit))
})

test_that("IJ variance: classification predict returns Variance matrix", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_is(pred$Variance, "matrix")
})

test_that("jack variance: classification predict returns Variance matrix", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "jack"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_is(pred$Variance, "matrix")
})

# --- Error handling ---
test_that("var.est=TRUE without var.mode in fit raises error", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_error(predict(fit, d$X, var.est = TRUE))
})

test_that("IJ forest predict with var.est=TRUE returns Variance (uses stored var.mode)", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  pred <- predict(fit, d$X, var.est = TRUE)
  expect_type(pred$Variance, "double")
  expect_length(pred$Variance, d$n)
})

test_that("IJ forest can predict with jackknife variance", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  pred <- predict(fit, d$X, var.est = TRUE, var.mode = "jack")
  expect_type(pred$Variance, "double")
})

test_that("jack forest can predict with IJ variance", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "jack"))
  pred <- predict(fit, d$X, var.est = TRUE, var.mode = "IJ")
  expect_type(pred$Variance, "double")
})

test_that("clean.variance floors negative variance to NA", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  pred <- predict(fit, d$X, var.est = TRUE)
  # No negative values (cleaned to NA)
  expect_true(all(pred$Variance >= 0 | is.na(pred$Variance)))
})

test_that("IJ mode does not compute VarVI", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "IJ"))
  expect_null(fit$VarVI)
})

test_that("jack mode does not compute VarVI", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "jack"))
  expect_null(fit$VarVI)
})
