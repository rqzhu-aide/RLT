#' @title C-index
#' @name cindex
#' @description Calculate c-index for survival data
#' @param y survival time
#' @param censor The censoring indicator if survival model is used
#' @param pred the predicted value for each subject
#' @export
#' @return c-index
#' @examples
#' \donttest{
#'   set.seed(42)
#'   n <- 100
#'   x <- matrix(rnorm(n * 5), ncol = 5)
#'   y <- rexp(n, rate = exp(rowSums(x[, 1:2])))
#'   censor <- rbinom(n, 1, 0.7)
#'   fit <- RLT(x, y, censor = censor, model = "survival", ntrees = 100)
#'   # Use cumulative hazard at last timepoint as risk score
#'   risk <- fit$Prediction[, ncol(fit$Prediction)]
#'   cindex(y, censor, risk)
#' }

cindex <- function(y, censor, pred)
{
    if (length(y) != length(censor) | length(y) != length(pred))
        stop("arguments length differ")
    
    if ( any( ! (censor %in% c(0, 1)) ) )
        stop("censoring indicator must be 0 or 1")
        
	return( cindex_d(y, censor, pred) )
}
