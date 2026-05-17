#' @title ClaForest
#' @name ClaForest
#' @description Internal function for fitting classification forest
#' @keywords internal

ClaForest <- function(x, y, 
                      ncat,
                      obs.w, var.prob,
                      resample.preset,
                      param,
                      ...)
{
  # prepare data
  if (any(is.na(x))) stop("NA not permitted in x")
  if (any(is.na(y))) stop("NA not permitted in y")
  if (nrow(x) != length(y)) stop("number of observations does not match: x & y")
  
  if (!is.factor(y)) stop("y must be a factor")
  if (length(unique(y)) != nlevels(y)) warning("y contains empty classes")
  
  nclass = nlevels(y)
  if (nclass < 2) stop("y must have at least 2 distinct classes")
  
  ylabels = levels(y)
  y.integer = as.numeric(y) - 1
  
  storage.mode(y.integer) <- "integer"
  
  # fit model
  
  if (param$linear.comb == 1)
  {
    if (param$verbose > 0)
      cat("Classification Random Forest ... \n")
    
    # check splitting rules
    if (is.null(param$"split.rule"))
      param$"split.rule" <- "gini"
    
    # Handle both string and integer inputs
    if (is.character(param$"split.rule"))
    {
      all.split.rule = c("gini")
      param$"split.rule" <- match(param$"split.rule", all.split.rule, nomatch = 0)
      
      if (param$"split.rule" == 0)
        warning("split.rule is not compatible with classification, switching to default")
      
      param$"split.rule" = 1L
    } else if (is.numeric(param$"split.rule"))
    {
      # Already an integer, just validate
      if (param$"split.rule" != 1)
        warning("split.rule integer must be 1 for classification. Resetting to 1")
      param$"split.rule" = 1L
    } else
    {
      warning("split.rule must be character or integer. Resetting to gini")
      param$"split.rule" = 1L
    }

    # Set linear.comb.method to 1 for consistency
    param$"linear.comb.method" <- 1L

    # fit single variable split model
    fit = ClaUniForestFit(x, y.integer, ncat, nclass,
                          obs.w, var.prob,
                          resample.preset,
                          param)

    fit[["parameters"]] = param
    fit[["ncat"]] = ncat
    fit[["obs.w"]] = obs.w
    fit[["var.prob"]] = var.prob
    fit[["y"]] = y
    fit[["ylabels"]] = ylabels
    fit[["nclass"]] = nclass

    # the returned prediction starts with 0
    fit$Prediction = as.factor( c(1:nclass, fit$Prediction+1) )[-(1:nclass)]
    levels(fit$Prediction) = ylabels

    class(fit) <- c("RLT", "fit", "cla", "uni")
  }else{
    
    if (param$verbose > 0)
      cat("Classification Forest with Linear Combination Splits ... \n")
    
    # check linear combination method
    # 1 = lda (default), 2 = naive, 3 = random, 4 = logistic
    if (is.null(param$"linear.comb.method"))
      param$"linear.comb.method" <- "lda"

    if (is.character(param$"linear.comb.method"))
    {
      all.linear.comb.method = c("lda", "naive", "random", "logistic")
      param$"linear.comb.method" <- match(param$"linear.comb.method", all.linear.comb.method, nomatch = 0)

      if (param$"linear.comb.method" == 0)
      {
        warning("linear.comb.method not recognized. Use 'lda', 'naive', 'random', or 'logistic'. Resetting to lda")
        param$"linear.comb.method" = 1L
      }
    } else if (is.numeric(param$"linear.comb.method"))
    {
      valid_methods <- c(1, 2, 3, 4)
      if (!(param$"linear.comb.method" %in% valid_methods))
      {
        warning("linear.comb.method integer must be 1-4. Resetting to 1 (lda)")
        param$"linear.comb.method" = 1L
      }
      param$"linear.comb.method" = as.integer(param$"linear.comb.method")
    } else
    {
      warning("linear.comb.method must be character or integer. Resetting to lda")
      param$"linear.comb.method" = 1L
    }
    
    # fit linear combination split model
    fit = ClaUniCombForestFit(x, y.integer, ncat, nclass,
                               obs.w, var.prob,
                               resample.preset,
                               param)
    
    fit[["parameters"]] = param
    fit[["ncat"]] = ncat
    fit[["obs.w"]] = obs.w
    fit[["var.prob"]] = var.prob
    fit[["y"]] = y
    fit[["ylabels"]] = ylabels
    fit[["nclass"]] = nclass
    
    # the returned prediction starts with 0
    fit$Prediction = as.factor( c(1:nclass, fit$Prediction+1) )[-(1:nclass)]
    levels(fit$Prediction) = ylabels
    
    class(fit) <- c("RLT", "fit", "cla", "uni", "comb")
  }
  
  return(fit)
}
