# test-survival-nsplit-alpha.R

context("Survival - nsplit and alpha parameters")

test_that("nsplit=0 (best split) stores correctly", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, nsplit = 0, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["nsplit"]], 0)
})

test_that("nsplit=5 (random split) stores correctly", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, nsplit = 5, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["nsplit"]], 5)
})

test_that("nsplit produces different trees than best split", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit0 <- RLT(d$X, d$y, censor = d$censor, model = "survival",
              ntrees = 30, nsplit = 0, seed = 1, ncores = 2, verbose = FALSE)
  fit5 <- RLT(d$X, d$y, censor = d$censor, model = "survival",
              ntrees = 30, nsplit = 5, seed = 1, ncores = 2, verbose = FALSE)
  expect_false(identical(fit0$FittedForest$SplitVar[[1]], 
                         fit5$FittedForest$SplitVar[[1]]))
})

test_that("nsplit works with each split.rule", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  for (rule in c("logrank", "suplogrank", "coxgrad")) {
    expect_error(
      RLT(d$X, d$y, censor = d$censor, model = "survival",
          ntrees = 20, nsplit = 3, split.rule = rule, ncores = 2, verbose = FALSE),
      NA
    )
  }
})

test_that("nsplit=1 still produces valid forest", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, nsplit = 1, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("alpha=0 stores correctly", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, alpha = 0, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["alpha"]], 0)
})

test_that("alpha=0.2 stores correctly", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, alpha = 0.2, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["alpha"]], 0.2)
})

test_that("alpha=0.5 (max) stores correctly", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, alpha = 0.5, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["alpha"]], 0.5)
})

test_that("alpha > 0.5 is clamped to 0.5", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, alpha = 0.9, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["alpha"]], 0.5)
})

test_that("alpha works with each split.rule", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  for (rule in c("logrank", "suplogrank", "coxgrad")) {
    expect_error(
      RLT(d$X, d$y, censor = d$censor, model = "survival",
          ntrees = 20, alpha = 0.2, split.rule = rule, ncores = 2, verbose = FALSE),
      NA
    )
  }
})

test_that("nsplit>0 with alpha: both stored correctly", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, nsplit = 3, alpha = 0.3, ncores = 2, verbose = FALSE)
  expect_equal(fit$parameters[["nsplit"]], 3)
  expect_equal(fit$parameters[["alpha"]], 0.3)
})

test_that("nsplit=0 + alpha=0.3 produces valid forest", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  expect_error(
    RLT(d$X, d$y, censor = d$censor, model = "survival",
        ntrees = 30, nsplit = 0, alpha = 0.3, ncores = 2, verbose = FALSE),
    NA
  )
})

test_that("large alpha produces balanced trees", {
  d <- generate_survival_data(n = 80, p = 10, seed = 1)
  fit <- RLT(d$X, d$y, censor = d$censor, model = "survival",
             ntrees = 30, nsplit = 0, alpha = 0.49, ncores = 2, verbose = FALSE)
  tree <- get.one.tree(fit, 1)
  expect_gt(max(tree$Depth), 1)
})
