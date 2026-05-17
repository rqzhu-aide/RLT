#' @title RegForest
#' @name RegForest
#' @description Internal function for fitting regression forest
#' @keywords internal

RegForest <- function(x, y, 
                      ncat,
                      obs.w, var.prob,
                      resample.preset,
                      param,
                      ...)
{
  # prepare data
  if (!is.vector(y)) stop("y must be a vector")
  if (any(is.na(x))) stop("NA not permitted in x")
  if (any(is.na(y))) stop("NA not permitted in y")
  if (nrow(x) != length(y)) stop("number of observations does not match: x & y")
  if (!is.numeric(y)) stop("y must be numerical for regression")
  
  storage.mode(y) <- "double"
  
  # fit model
  
  if (param$linear.comb == 1)
  {
    if (param$verbose > 0)
      cat("Regression Random Forest ... \n")
    
    # check splitting rules (variance reduction)
    if (is.null(param$"split.rule"))
      param$"split.rule" <- "var"
    
    # Handle both string and integer inputs
    if (is.character(param$"split.rule"))
    {
      all.split.rule = c("var")
      param$"split.rule" <- match(param$"split.rule", all.split.rule, nomatch = 0)
      
      if (param$"split.rule" == 0)
        warning("split.rule is not compatible with regression, switching to default")
      
      param$"split.rule" = 1L
    } else if (is.numeric(param$"split.rule"))
    {
      # Already an integer, just validate
      if (param$"split.rule" != 1)
        warning("split.rule integer must be 1 for regression. Resetting to 1")
      param$"split.rule" = 1L
    } else
    {
      warning("split.rule must be character or integer. Resetting to var")
      param$"split.rule" = 1L
    }
    
    # Set linear.comb.method to 1 for consistency
    param$"linear.comb.method" <- 1L
      
    # fit single variable split model
    fit = RegUniForestFit(x, y, ncat,
                          obs.w, var.prob,
                          resample.preset,
                          param)
  
    fit[["parameters"]] = param
    fit[["ncat"]] = ncat
    fit[["obs.w"]] = obs.w
    fit[["var.prob"]] = var.prob
    fit[["y"]] = y
    
    class(fit) <- c("RLT", "fit", "reg", "uni")
  }else{
    
    if (param$verbose > 0)
      cat("Regression Forest with Linear Combination Splits ... \n")
    
    # check linear combination method
    if (is.null(param$"linear.comb.method"))
      param$"linear.comb.method" <- "sir"
    
    # Handle both string and integer inputs
    if (is.character(param$"linear.comb.method"))
    {
      all.linear.comb.method = c("naive", "lm", "pca", "sir")
      param$"linear.comb.method" <- match(param$"linear.comb.method", all.linear.comb.method, nomatch = 0)
      
      if (param$"linear.comb.method" == 0)
      {
        warning("linear.comb.method not recognized. Use 'naive', 'lm', 'pca', or 'sir'. Resetting to naive")
        param$"linear.comb.method" = 1L
      }
    } else if (is.numeric(param$"linear.comb.method"))
    {
      # Already an integer, just validate it's in range 1-4
      if (param$"linear.comb.method" < 1 || param$"linear.comb.method" > 4)
      {
        warning("linear.comb.method integer must be 1-4. Resetting to 1 (naive)")
        param$"linear.comb.method" = 1L
      }
      param$"linear.comb.method" = as.integer(param$"linear.comb.method")
    } else
    {
      warning("linear.comb.method must be character or integer. Resetting to naive")
      param$"linear.comb.method" = 1L
    }
          
    # fit linear combination split model
    fit = RegUniCombForestFit(x, y, ncat,
                              obs.w, var.prob,
                              resample.preset,
                              param)
    
    fit[["parameters"]] = param
    fit[["ncat"]] = ncat
    fit[["obs.w"]] = obs.w
    fit[["var.prob"]] = var.prob
    fit[["y"]] = y
    
    class(fit) <- c("RLT", "fit", "reg", "uni", "comb")
  }

  return(fit)
}
