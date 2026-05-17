# test-importance.R

context("Importance Method")

test_that("importance() returns data.frame for regression", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_is(imp, "data.frame")
  expect_equal(nrow(imp), d$p)
})

test_that("importance() Variable names come from xnames", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_equal(imp$Variable, colnames(d$X))
})

test_that("importance() VI values are numeric", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_type(imp$VI, "double")
})

test_that("importance() without var.mode has no SD/Z/Sig columns", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_false("SD" %in% colnames(imp))
  expect_false("Z" %in% colnames(imp))
  expect_false("Sig" %in% colnames(imp))
})

test_that("importance() returns data.frame for classification", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_is(imp, "data.frame")
  expect_equal(nrow(imp), d$p)
})

test_that("classification importance identifies signal variables", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  top3 <- imp$Variable[order(imp$VI, decreasing = TRUE)[1:3]]
  # X1, X2, X3 are signal; at least 2 should be in top 3
  expect_gte(sum(c("X1", "X2", "X3") %in% top3), 2)
})

test_that("importance() errors when importance = FALSE", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = FALSE, ncores = 2, verbose = FALSE)
  expect_error(importance(fit))
})

test_that("importance() errors on non-fit object", {
  expect_error(importance(list()))
})

test_that("importance() with var.mode has SD, Z, Sig columns", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  expect_true("SD" %in% colnames(imp))
  expect_true("Z" %in% colnames(imp))
  expect_true("Sig" %in% colnames(imp))
})

test_that("matched var.mode importance SD is non-negative (or NA)", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  expect_true(all(imp$SD >= 0 | is.na(imp$SD)))
})

test_that("matched var.mode Z = VI / SD", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  valid <- !is.na(imp$SD) & imp$SD > 0
  if (any(valid)) {
    expect_equal(imp$Z[valid], imp$VI[valid] / imp$SD[valid])
  }
})

test_that("significance codes are correct", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  valid_codes <- c("***", "**", "*", ".", " ", "")
  expect_true(all(imp$Sig %in% valid_codes))
})

test_that("classification importance with var.mode has variance columns", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  expect_true("SD" %in% colnames(imp))
  expect_true("Z" %in% colnames(imp))
})

test_that("classification matched importance SD is non-negative or NA", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  expect_true(all(imp$SD >= 0 | is.na(imp$SD)))
})

test_that("importance() works with importance = 'distribute'", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = "distribute", ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_is(imp, "data.frame")
})

test_that("importance = 'distribute' with var.mode has variance", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = "distribute", ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  expect_true("SD" %in% colnames(imp))
})

test_that("print.importance.RLT runs without error (no variance)", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_error(capture.output(print(imp)), NA)
})

test_that("print.importance.RLT runs without error (with variance)", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  imp <- importance(fit)
  expect_error(capture.output(print(imp)), NA)
})

test_that("importance() uses V1, V2, ... when X has no colnames", {
  set.seed(42)
  X <- matrix(rnorm(80 * 5), 80, 5)
  y <- X[, 1] + rnorm(80)
  fit <- RLT(X, y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  imp <- importance(fit)
  expect_true(all(grepl("^V[0-9]+$", imp$Variable)))
})

test_that("VarVI is returned for matched var.mode", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE,
             param.control = list(var.mode = "matched"))
  expect_true("VarVI" %in% names(fit))
  expect_length(fit$VarVI, d$p)
})

test_that("VarVI is NULL without var.mode", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, importance = TRUE, ncores = 2, verbose = FALSE)
  expect_null(fit$VarVI)
})
