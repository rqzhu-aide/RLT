# Synthetic Data Generation for RLT Testing
# This file provides data generation functions for testthat tests

#' Generate mixed feature regression data
#'
#' Creates a synthetic dataset with half continuous and half categorical features.
#' The true model depends on first 5 continuous variables and 1 categorical variable.
#'
#' @param n Total number of observations (default: 400)
#' @param p Total number of features (default: 15, half continuous, half categorical)
#' @param seed Random seed for reproducibility (default: 1)
#'
#' @return A list with:
#'   - X: data frame with n rows and p columns (continuous + factors)
#'   - y: numeric response vector of length n
#'   - n: number of observations
#'   - p: number of features
#'   - n_continuous: number of continuous features
#'   - n_categorical: number of categorical features
#'   - true_model: description of the true model
#'
#' @examples
#' data <- generate_mixed_feature_regression(n = 400, p = 15)
#' train_idx <- 1:300
#' trainX <- data$X[train_idx, ]
#' trainY <- data$y[train_idx]
#' testX <- data$X[-train_idx, ]
#' testY <- data$y[-train_idx]
#'
generate_mixed_feature_regression <- function(n = 400, p = 15, seed = 1) {
  
  set.seed(seed)
  
  # Ensure p is even for equal split
  if (p %% 2 != 0) {
    p <- p + 1
    warning(paste("p must be even for equal continuous/categorical split. Adjusted to", p))
  }
  
  n_continuous <- p / 2
  n_categorical <- p / 2
  
  # Generate continuous variables
  X1 <- matrix(rnorm(n * n_continuous), n, n_continuous)
  
  # Generate categorical variables (3 levels: 0, 1, 2)
  X2 <- matrix(as.integer(runif(n * n_categorical) * 3), n, n_categorical)
  
  # Combine into data frame
  X <- data.frame(X1, X2)
  
  # Convert categorical columns to factors
  X[, (n_continuous + 1):p] <- lapply(X[, (n_continuous + 1):p], as.factor)
  
  # Set column names
  colnames(X) <- c(paste0("X", 1:n_continuous), 
                   paste0("Xcat", 1:n_categorical))
  
  # Generate response with known true model
  # True model: first 5 continuous + first categorical (levels 1, 3 have effect)
  y <- 1 + 
       rowSums(X[, 2:min(6, n_continuous)]) +  # continuous variables X2-X6
       2 * (X[, n_continuous + 1] %in% c(1, 2)) +  # categorical Xcat1
       rnorm(n)
  
  return(list(
    X = X,
    y = y,
    n = n,
    p = p,
    n_continuous = n_continuous,
    n_categorical = n_categorical,
    true_model = "y = 1 + X2 + X3 + X4 + X5 + X6 + 2*I(Xcat1 in {1,2}) + noise"
  ))
}

#' Generate simple regression data (all continuous)
#'
#' Creates a simple synthetic dataset with all continuous features.
#' Useful for basic functionality tests.
#'
#' @param n Total number of observations (default: 200)
#' @param p Total number of features (default: 10)
#' @param seed Random seed for reproducibility (default: 1)
#'
#' @return A list with X (matrix), y (vector), n, p
#'
generate_simple_regression <- function(n = 200, p = 10, seed = 1) {
  
  set.seed(seed)
  
  X <- matrix(rnorm(n * p), n, p)
  y <- 1 + X[, 1] + 2 * X[, 2] + rnorm(n)
  
  colnames(X) <- paste0("X", 1:p)
  
  return(list(
    X = X,
    y = y,
    n = n,
    p = p,
    true_model = "y = 1 + X1 + 2*X2 + noise"
  ))
}

#' Generate classification data
#'
#' Creates a synthetic binary classification dataset.
#'
#' @param n Total number of observations (default: 400)
#' @param p Total number of features (default: 15)
#' @param seed Random seed for reproducibility (default: 1)
#'
#' @return A list with X (data frame), y (factor), n, p
#'
generate_classification_data <- function(n = 400, p = 15, seed = 1) {
  
  set.seed(seed)
  
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("X", 1:p)
  
  # Binary outcome based on first 3 variables
  prob <- plogis(X[, 1] + X[, 2] - X[, 3])
  y <- factor(ifelse(runif(n) < prob, 1, 0))
  
  return(list(
    X = X,
    y = y,
    n = n,
    p = p,
    true_model = "logit(P(y=1)) = X1 + X2 - X3"
  ))
}

# ' Generate survival data
#
#' Creates a synthetic right-censored survival dataset.
#'
#' @param n Total number of observations (default: 400)
#' @param p Total number of features (default: 10)
#' @param censoring_rate Proportion of censored observations (default: 0.3)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list with X (matrix), y (survival time), censor (0/1 indicator), n, p
#'
generate_survival_data <- function(n = 400, p = 10, 
                                    censoring_rate = 0.3, 
                                    seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Generate features
  X <- matrix(rnorm(n * p), nrow = n)
  colnames(X) <- paste0("X", 1:p)
  
  # Generate survival times (exponential with covariate effect)
  true_effect <- X[, 1] + 0.5 * X[, 2]
  hazard <- exp(true_effect)
  surv_time <- rexp(n, rate = hazard)
  
  # Generate censoring times to achieve target censoring rate
  if (censoring_rate > 0 && censoring_rate < 1) {
    censor_time <- rexp(n, rate = quantile(hazard, 0.5) * (1 - censoring_rate) / censoring_rate)
  } else if (censoring_rate == 0) {
    censor_time <- rep(Inf, n)
  } else {
    censor_time <- rep(0, n)
  }
  
  # Observed time and censoring indicator
  y <- pmin(surv_time, censor_time)
  censor <- as.numeric(surv_time <= censor_time)
  
  return(list(
    X = X,
    y = y,
    censor = censor,
    n = n,
    p = p,
    censoring_rate = mean(censor),
    true_model = "hazard = exp(X1 + 0.5*X2)"
  ))
}

cat("✅ Synthetic data generators loaded\n")
