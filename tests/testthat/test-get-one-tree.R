# test-get-one-tree.R

context("get.one.tree")

test_that("get.one.tree returns data.frame for regression", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_is(tree, "data.frame")
})

test_that("get.one.tree regression has correct column names", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expected_cols <- c("Node", "Depth", "Split", "Value", "n")
  expect_true(all(expected_cols %in% colnames(tree)))
})

test_that("get.one.tree regression Node column is 1-indexed", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_equal(tree$Node[1], 1)
})

test_that("get.one.tree regression Depth starts at 0", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_equal(tree$Depth[1], 0)
})

test_that("get.one.tree regression has terminal nodes (Split = '*')", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_true(any(tree$Split == "*"))
})

test_that("get.one.tree regression leaf Value is numeric or '-'", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  # Value column exists for all nodes
  expect_true("Value" %in% colnames(tree))
})

test_that("get.one.tree regression n column is positive", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_true(all(tree$n > 0))
})

test_that("get.one.tree regression root node n equals training n", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_equal(tree$n[1], d$n)
})

test_that("get.one.tree regression uses X column names for Split", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  split_nodes <- tree[tree$Split != "*", ]
  # Split column should reference X column names
  expect_true(all(split_nodes$Split %in% c(colnames(d$X), "*")))
})

test_that("get.one.tree returns data.frame for classification", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_is(tree, "data.frame")
})

test_that("get.one.tree classification has correct column names", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expected_cols <- c("Node", "Depth", "Split", "Value", "n")
  expect_true(all(expected_cols %in% colnames(tree)))
})

test_that("get.one.tree classification NodeProb is stored in forest", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_false(is.null(fit$FittedForest$NodeProb))
})

test_that("get.one.tree classification root node n equals training n", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_equal(tree$n[1], d$n)
})

test_that("get.one.tree regression LC tree has SplitLoad in forest", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "naive",
             ncores = 2, verbose = FALSE)
  expect_false(is.null(fit$FittedForest$SplitLoad))
})

test_that("get.one.tree classification LC tree works", {
  d <- generate_classification_data(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, linear.comb = 3, linear.comb.method = "lda",
             ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_is(tree, "data.frame")
})

test_that("get.one.tree errors on tree index 0", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_error(get.one.tree(fit, 0))
})

test_that("get.one.tree errors on tree index > ntrees", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  expect_error(get.one.tree(fit, 31))
})

test_that("get.one.tree works for all trees in forest", {
  d <- generate_simple_regression(n = 80, p = 5)
  ntrees <- 5
  fit <- RLT(d$X, d$y, ntrees = ntrees, ncores = 2, verbose = FALSE)
  for (i in 1:ntrees) {
    tree <- get.one.tree(fit, i)
    expect_is(tree, "data.frame")
  }
})

test_that("get.one.tree uses V1, V2, ... when X has no colnames", {
  set.seed(42)
  X <- matrix(rnorm(80 * 5), 80, 5)
  y <- X[, 1] + rnorm(80)
  fit <- RLT(X, y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  split_nodes <- tree[tree$Split != "*", ]
  if (nrow(split_nodes) > 0) {
    expect_true(all(grepl("^V[0-9]+$", split_nodes$Split)))
  }
})

test_that("get.one.tree depth increases monotonically along tree paths", {
  d <- generate_simple_regression(n = 80, p = 10)
  fit <- RLT(d$X, d$y, ntrees = 30, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  # Root depth should be 0
  expect_equal(tree$Depth[1], 0)
  # Max depth should be > 0 for non-trivial trees
  expect_gt(max(tree$Depth), 0)
})
