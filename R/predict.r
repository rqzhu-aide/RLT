clean.variance <- function(pred) {
  if (!is.null(pred$Variance)) {
    pred$Variance[pred$Variance < 0] <- NA
  }
  pred
}

#' @title prediction using RLT
#' @description  Predict the outcome (regression, classification or survival) 
#'              using a fitted RLT object
#' @param object          A fitted RLT object
#' @param testx           The testing samples, must have the same structure as the training samples
#' @param var.est         Whether to estimate the variance of each testing data. 
#'                        The original forest must be fitted with \code{var.mode != "none"}.
#'                        For survival forests, calculates the covariance matrix over all
#'                        observed time points and calculates critical value for the confidence 
#'                        band.
#' @param keep.all        whether to keep the prediction from all trees. Warning: this can 
#'                        occupy a large storage space, especially in survival model
#' @param ncores          number of cores
#' @param verbose         print additional information
#' @param var.mode        Variance estimation mode for prediction. Can be \code{"none"},
#'                        \code{"matched"}, \code{"IJ"}, or \code{"jack"}. If \code{NULL}
#'                        (default), uses the mode from the fitted object. Only used when
#'                        \code{var.est = TRUE}. \code{"matched"} requires the forest to be
#'                        fitted with \code{var.mode = "matched"}. \code{"IJ"} and \code{"jack"}
#'                        require the forest to have been fitted with resample tracking
#'                        (automatically enabled when using IJ or jack variance).
#' @param band.grid.size  An integer specifying the number of time points for confidence band calculation.
#'                        Default is 0, which uses all unique failure time points. If a positive integer
#'                        is provided, a subset of time points will be selected using quantiles,
#'                        skipping the earliest 5% of time points to improve stability.
#' @param ... ...
#'
#' @return 
#' 
#' A \code{RLT} prediction object, constructed as a list consisting
#' 
#' \item{Prediction}{Prediction}
#' \item{Variance}{if \code{var.est = TRUE} and the fitted object is 
#'                 \code{var.mode != "none"}}
#'                 
#' \strong{For Survival Forests}
#' \item{hazard}{predicted hazard functions}
#' \item{CumHazard}{predicted cumulative hazard function}
#' \item{Survival}{predicted survival function}
#' \item{Allhazard}{if \code{keep.all = TRUE}, the predicted hazard function for each 
#'                 observation and each tree}
#' \item{AllCHF}{if \code{keep.all = TRUE}, the predicted cumulative hazard function for each 
#'                 observation and each tree}
#' \item{Cov}{if \code{var.est = TRUE} and the fitted object is 
#'                 \code{var.mode != "none"}. For each test subject, a matrix of size NFail\eqn{\times}NFail
#'                 where NFail is the number of observed failure times in the training data}
#' \item{Var}{if \code{var.est = TRUE} and the fitted object is 
#'                 \code{var.mode != "none"}. Marginal variance for each subject}
#'  \item{timepoints}{ordered observed failure times from the training data}               
#' \item{MarginalVar}{if \code{var.est = TRUE} and the fitted object is 
#'                 \code{var.mode != "none"}. Marginal variance for each subject
#'                 from the Cov matrix projected to the nearest positive definite
#'                 matrix}
#' \item{MarginalVarSmooth}{if \code{var.est = TRUE} and the fitted object is 
#'                 \code{var.mode != "none"}. Marginal variance for each subject
#'                 from the Cov matrix projected to the nearest positive definite
#'                 matrix and then smoothed using Gaussian kernel smoothing}
#' \item{CVproj}{if \code{var.est = TRUE} and the fitted object is 
#'                 \code{var.mode != "none"}. Critical values to calculate confidence bands around
#'                 cumulative hazard predictions at several confidence levels. Calculated using 
#'                 \code{MarginalVar}}
#' \item{CVprojSmooth}{if \code{var.est = TRUE} and the fitted object is 
#'                 \code{var.mode != "none"}. Critical values to calculate confidence bands around
#'                 cumulative hazard predictions at several confidence levels. Calculated using 
#'                 \code{MarginalVarSmooth}}
#' @export
#' @examples
#' \donttest{
#'   set.seed(42)
#'   x <- matrix(rnorm(300 * 5), ncol = 5)
#'   y <- rowSums(x[, 1:2]) + rnorm(300)
#'   fit <- RLT(x, y, ntrees = 100)
#'   pred <- predict(fit, testx = x[1:5, ])
#'   print(pred$Prediction)
#' }
predict.RLT<- function(object,
                       testx = NULL,
                       var.est = FALSE,
                       var.mode = NULL,
                       keep.all = FALSE, 
                       ncores = 1,
                       verbose = 0,
                       band.grid.size = 0, 
                       ...)
{
  
  # insample prediction
  if (is.null(testx))
    return(object$Prediction)
  
  # check test data
  if (!is.matrix(testx) && !is.data.frame(testx)) stop("testx must be a matrix or a data.frame")
  
  if (ncol(testx) < object$parameters$p) stop("test data dimension does not match training data") 
    
  if (is.null(colnames(testx)))
  {
    if (ncol(testx) != object$parameters$p) stop("test data dimension does not match training data, variable names are not supplied...")
  }else if (any(colnames(testx) != object$xnames)){
    
    warning("test data variables names does not match training data...")
    varmatch = match(object$xnames, colnames(testx))
    if (any(is.na(varmatch))) stop("test data missing some variables...")
    testx = testx[, varmatch]
  }

  testx <- data.matrix(testx)

  if (var.est && is.null(var.mode) && object$parameters$var.mode == 0)
    stop("The original forest is not fitted with `var.mode`. Please check the conditions and build another forest, or specify var.mode explicitly.")
  
  # Validate that requested var.mode is compatible with fitted forest
  if (var.est && !is.null(var.mode)) {
    req_mode <- match.arg(var.mode, c("none", "matched", "IJ", "jack"))
    fitted_mode <- c("none", "matched", "IJ", "jack")[object$parameters$var.mode + 1]
    
    # matched requires the forest to be built with matched sampling (half-and-half tree structure)
    if (req_mode == "matched" && object$parameters$var.mode != 1)
      stop("matched variance requires the forest to be fitted with var.mode = 'matched'")
    
    # IJ/jack require ObsTrack (resample tracking)
    if (req_mode %in% c("IJ", "jack") && is.null(object$ObsTrack))
      stop("IJ/jack variance requires resample tracking. Fit with var.mode = 'IJ' or 'jack', or resample.track = 1")
  }
  
  ncomb = object$parameters$linear.comb
  
  if( class(object)[[2]] == "fit" && class(object)[[3]] == "reg" && ncomb == 1)
  {
    vm <- if (!is.null(var.mode)) {
      match.arg(var.mode, c("none", "matched", "IJ", "jack"))
      c("none"=0L, "matched"=1L, "IJ"=2L, "jack"=3L)[var.mode]
    } else {
      object$parameters$var.mode
    }
    cxx_var_mode <- if (var.est) vm else 0L
    
    # Get ObsTrack, or empty matrix if not available
    ObsTrack <- object$ObsTrack
    if (is.null(ObsTrack)) ObsTrack <- matrix(0L, nrow = 0, ncol = 0)
    
    pred <- RegUniForestPred(object$FittedForest$SplitVar,
                             object$FittedForest$SplitValue,
                             object$FittedForest$LeftNode,
                             object$FittedForest$RightNode,
                             object$FittedForest$NodeWeight,
                             object$FittedForest$NodeAve,
                             testx,
                             object$ncat,
                             ObsTrack,
                             cxx_var_mode,
                             keep.all,
                             ncores,
                             verbose)
    
    class(pred) <- c("RLT", "pred", "reg")
    pred <- clean.variance(pred)
    return(pred)
  }
  
  
  if( class(object)[[2]] == "fit" && class(object)[[3]] == "reg" && ncomb > 1)
  {
    vm <- if (!is.null(var.mode)) {
      match.arg(var.mode, c("none", "matched", "IJ", "jack"))
      c("none"=0L, "matched"=1L, "IJ"=2L, "jack"=3L)[var.mode]
    } else {
      object$parameters$var.mode
    }
    cxx_var_mode <- if (var.est) vm else 0L
    
    # Get ObsTrack, or empty matrix if not available
    ObsTrack <- object$ObsTrack
    if (is.null(ObsTrack)) ObsTrack <- matrix(0L, nrow = 0, ncol = 0)
    
    pred <- RegUniCombForestPred(object$FittedForest$SplitVar,
                                 object$FittedForest$SplitLoad,
                                 object$FittedForest$SplitValue,
                                 object$FittedForest$LeftNode,
                                 object$FittedForest$RightNode,
                                 object$FittedForest$NodeWeight,
                                 object$FittedForest$NodeAve,
                                 testx,
                                 object$ncat,
                                 ObsTrack,
                                 cxx_var_mode,
                                 keep.all,
                                 ncores,
                                 verbose)
    
    class(pred) <- c("RLT", "pred", "reg")
    pred <- clean.variance(pred)
    return(pred)
  }
  
  
  if( class(object)[[2]] == "fit" && class(object)[[3]] == "cla" && ncomb == 1)
  {
    vm <- if (!is.null(var.mode)) {
      match.arg(var.mode, c("none", "matched", "IJ", "jack"))
      c("none"=0L, "matched"=1L, "IJ"=2L, "jack"=3L)[var.mode]
    } else {
      object$parameters$var.mode
    }
    cxx_var_mode <- if (var.est) vm else 0L
    
    # Get ObsTrack, or empty matrix if not available
    ObsTrack <- object$ObsTrack
    if (is.null(ObsTrack)) ObsTrack <- matrix(0L, nrow = 0, ncol = 0)
    
    pred <- ClaUniForestPred(object$FittedForest$SplitVar,
                             object$FittedForest$SplitValue,
                             object$FittedForest$LeftNode,
                             object$FittedForest$RightNode,
                             object$FittedForest$NodeWeight,
                             object$FittedForest$NodeProb,
                             testx,
                             object$ncat,
                             ObsTrack,
                             cxx_var_mode,
                             keep.all,
                             ncores,
                             verbose)
    
    pred$Prediction = as.factor( c(1:object$nclass, pred$Prediction+1) )[-(1:object$nclass)]
    levels(pred$Prediction) = object$ylabels

    class(pred) <- c("RLT", "pred", "cla")
    pred <- clean.variance(pred)
    return(pred)
  }  
  
  if( class(object)[[2]] == "fit" && class(object)[[3]] == "cla" && ncomb > 1)
  {
    vm <- if (!is.null(var.mode)) {
      match.arg(var.mode, c("none", "matched", "IJ", "jack"))
      c("none"=0L, "matched"=1L, "IJ"=2L, "jack"=3L)[var.mode]
    } else {
      object$parameters$var.mode
    }
    cxx_var_mode <- if (var.est) vm else 0L
    
    # Get ObsTrack, or empty matrix if not available
    ObsTrack <- object$ObsTrack
    if (is.null(ObsTrack)) ObsTrack <- matrix(0L, nrow = 0, ncol = 0)
    
    pred <- ClaUniCombForestPred(object$FittedForest$SplitVar,
                                 object$FittedForest$SplitLoad,
                                 object$FittedForest$SplitValue,
                                 object$FittedForest$LeftNode,
                                 object$FittedForest$RightNode,
                                 object$FittedForest$NodeWeight,
                                 object$FittedForest$NodeProb,
                                 testx,
                                 object$ncat,
                                 ObsTrack,
                                 cxx_var_mode,
                                 keep.all,
                                 ncores,
                                 verbose)
    
    pred$Prediction = as.factor( c(1:object$nclass, pred$Prediction+1) )[-(1:object$nclass)]
    levels(pred$Prediction) = object$ylabels

    class(pred) <- c("RLT", "pred", "cla")
    pred <- clean.variance(pred)
    return(pred)
  }  
  
  if( class(object)[[2]] == "fit" && class(object)[[3]] == "surv" )
  {
    # Resolve variance mode
    vm <- if (!is.null(var.mode)) {
      match.arg(var.mode, c("none", "matched", "IJ", "jack"))
      c("none"=0L, "matched"=1L, "IJ"=2L, "jack"=3L)[var.mode]
    } else {
      object$parameters$var.mode
    }
    cxx_var_mode <- if (var.est) vm else 0L
    
    # Get ObsTrack, or empty matrix if not available
    ObsTrack <- object$ObsTrack
    if (is.null(ObsTrack)) ObsTrack <- matrix(0L, nrow = 0, ncol = 0)
    
    # Get original timepoints and NFail
    original_timepoints <- object$timepoints
    NFail <- length(object$timepoints)
    
    # Default: use all points
    new_grid_timepoints <- original_timepoints

    # Only perform reduction when user specifies valid band.grid.size
    if (band.grid.size > 0) {
      effective_grid_size <- min(band.grid.size, NFail)
      
      # Only execute selection when reduction is actually needed
      if (effective_grid_size < NFail){
        probs_to_select <- seq(from = 0.05, to = 1.0, length.out = effective_grid_size)
        new_grid_timepoints <- unique(as.numeric(stats::quantile(original_timepoints, probs = probs_to_select, type = 1)))
      }
    }
    
    new_grid_timepoints <- sort(new_grid_timepoints)
    
    # Create mapping indices for C++ function
    mapping_indices <- match(new_grid_timepoints, original_timepoints) - 1  # Convert to 0-based indexing
    
    if (ncomb > 1)
    {
      # Linear combination (Comb) survival forest prediction
      pred <- SurvUniCombForestPred(object$FittedForest$SplitVar,
                                     object$FittedForest$SplitLoad,
                                     object$FittedForest$SplitValue,
                                     object$FittedForest$LeftNode,
                                     object$FittedForest$RightNode,
                                     object$FittedForest$NodeWeight,
                                     object$FittedForest$NodeHaz,
                                     testx,
                                     object$ncat,
                                     NFail, 
                                     mapping_indices,
                                     ObsTrack,
                                     cxx_var_mode,
                                     keep.all,
                                     ncores,
                                     verbose)
    }else{
      # Single-variable survival forest prediction
      pred <- SurvUniForestPred(object$FittedForest$SplitVar,
                                object$FittedForest$SplitValue,
                                object$FittedForest$LeftNode,
                                object$FittedForest$RightNode,
                                object$FittedForest$NodeWeight,
                                object$FittedForest$NodeHaz,
                                testx,
                                object$ncat,
                                NFail, 
                                mapping_indices,
                                ObsTrack,
                                cxx_var_mode,
                                keep.all,
                                ncores,
                                verbose)
    }

    
    pred$timepoints <- new_grid_timepoints
    
    class(pred) <- c("RLT", "pred", "surv")
    pred <- clean.variance(pred)
    return(pred)
  }
}
