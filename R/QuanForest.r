#' @title Quantile Forest (Not Yet Implemented)
#' @name QuanForest
#' @description Internal function for fitting quantile regression forest.
#'   This feature is not yet implemented. Calling \code{RLT()} with
#'   \code{model = "quantile"} will produce an informative error.
#' @keywords internal

QuanForest <- function(x, y, ncat,
                       obs.w, var.prob,
                       resample.preset,
                       param,
                       ...)
{
  stop("Quantile forest is not yet implemented. Available models: regression, classification, survival.")
}
