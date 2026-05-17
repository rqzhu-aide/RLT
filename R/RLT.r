#' @title Reinforcement Learning Trees
#' @description           Fit models for regression, classification and survival
#' analysis using reinforced splitting rules. The model
#' fits regular random forest models by default unless the
#' parameter \code{reinforcement} is set to `"TRUE"`. Using
#' \code{reinforcement = TRUE} activates embedded model for
#' splitting variable selection and allows linear combination
#' split. To specify parameters of embedded models, see
#' definition of \code{param.control} for details.
#'
#' @param x               A `matrix` or `data.frame` of features. If \code{x} is
#'                        a data.frame, then all factors are treated as categorical
#'                        variables, which will go through an exhaustive search of
#'                        splitting criteria.
#'
#' @param y               Response variable. a `numeric`/`factor` vector.
#'
#' @param censor          Censoring indicator if survival model is used.
#'
#' @param model           The model type: `"regression"`, `"classification"`,
#'                        or `"survival"`. Quantile forest is not yet implemented.
#'
#' @param reinforcement   Should reinforcement splitting rule be used. Default
#'                        is `"FALSE"`, i.e., regular random forests with marginal
#'                        search of splitting variable. When it is activated, an
#'                        embedded model is fitted to find the best splitting variable
#'                        or a linear combination of them, if \code{linear.comb} $> 1$.
#'                        They can also be specified in \code{param.control}.
#'
#' @param ntrees          Number of trees, `ntrees = 100` if reinforcement is
#'                        used and `ntrees = 500` otherwise.
#'
#' @param mtry            Number of randomly selected variables used at each
#'                        internal node. Default: max(1, floor(p/2)).
#'
#' @param nmin            Terminal node size. Splitting will stop when the internal
#'                        node size is less equal to \code{nmin}. Default: 5.
#'
#' @param alpha           Minimum proportion of samples (of the parent node)
#'                        enforced in each child node. Default: 0 (no constraint).
#'                        Clamped to the range 0 to 0.5.
#'
#' @param nsplit          Number of random cutting points to compare for each
#'                        variable at an internal node. Default: 0 (use all unique
#'                        values, i.e., best split). When nsplit > 0, random
#'                        cutting points are generated.
#'
#' @param resample.replace Whether the in-bag samples are obtained with
#'                        replacement.
#'
#' @param resample.prob   Proportion of in-bag samples.
#'
#' @param resample.preset A pre-specified matrix for in-bag data indicator/count
#'                        matrix. It must be an \eqn{n \times} \code{ntrees}
#'                        matrix with integer entries. Positive number indicates
#'                        the number of copies of that observation (row) in the
#'                        corresponding tree (column); zero indicates out-of-bag;
#'                        negative values indicates not being used in either.
#'                        Extremely large counts should be avoided. The sum of
#'                        each column should not exceed \eqn{n}.
#'
#' @param obs.w           Observation weights. The weights will be used for calculating
#'                        the splitting scores, such as a weighted variance reduction
#'                        or weighted gini index. But they will not be used for
#'                        sampling observations. In that case, one can pre-specify
#'                        \code{resample.preset} instead for balanced sampling, etc.
#'                        For survival analysis, observation weights are supported
#'                        in the \code{"logrank"}, \code{"suplogrank"}, and
#'                        \code{"coxgrad"} splitting rules. Weighted logrank and
#'                        suplogrank use a variance estimator that accounts for
#'                        the observation weights.
#'
#' @param var.prob        Variable probabilities for split variable selection. A
#'                        numeric vector of length \code{p} (number of predictors)
#'                        with non-negative weights. When supplied, \code{mtry}
#'                        variables are sampled \emph{without replacement} with
#'                        probabilities proportional to these weights at each
#'                        internal node. This effectively up-weights or
#'                        down-weights individual predictors during tree
#'                        construction. Works for all models (regression,
#'                        classification, survival). The vector does
#'                        not need to sum to 1; it is internally normalized.
#'                        If \code{NULL} (default), uniform sampling is used.
#'
#' @param importance      Whether to calculate variable importance measures. When
#'                        set to `"TRUE"` (or `"permute"`), the calculation follows
#'                        Breiman's original permutation strategy. If set to `"distribute"`,
#'                        then it sends the oob data to both child nodes with
#'                        weights proportional to their sample sizes. Hence
#'                        the final prediction is a weighted average of all possible
#'                        terminal nodes that a perturbed observation could fall into.
#'                        This feature is currently only available in regression
#'                        and classification models.
#'
#' @param linear.comb    Number of variables to combine in each linear combination split.
#'                       Default is 1 (standard axis-aligned splits). See also
#'                       \code{linear.comb.method} and \code{param.control}.
#' @param linear.comb.method  Method for constructing linear combinations:
#'                       \code{"default"}, \code{"coxph"} (Cox PH loading, survival only),
#'                       or \code{"naive"} (covariance-based loading). See \code{param.control}.
#' @param split.rule     Splitting criterion. Default \code{"default"} selects the
#'                       standard rule for each model. For survival: \code{"logrank"},
#'                       \code{"suplogrank"}, or \code{"coxgrad"}. See \code{param.control}.
#' @param var.mode        Variance estimation mode. Default is \code{"none"}
#'                        (no variance estimation). Set to \code{"matched"} or
#'                        \code{TRUE} to use matched-sample U-statistic
#'                        decomposition for prediction variance and variable
#'                        importance variance. When active, several resampling
#'                        parameters are automatically adjusted. Equivalent to
#'                        setting \code{param.control = list(var.mode = "matched")}.
#'                        See \code{param.control} for full details.
#'
#' @param param.control   A list of additional parameters. This can be used to
#'                        specify other features in a random forest or set embedded
#'                        model parameters for reinforcement splitting rules.
#'                        Using \code{reinforcement = TRUE} will automatically
#'                        generate some default tuning for the embedded model.
#'                        \strong{Reinforcement is available for regression,
#'                        classification, and survival models.}
#'                        They are not necessarily optimized.
#'                        \itemize{
#'                        \item \code{embed.ntrees}: number of trees in the embedded model. Default: 50.
#'                        \item \code{embed.mtry}: proportion of variables for embedded splits. Default: 0.5.
#'                        \item \code{embed.nmin}: terminal node size for embedded model. Default: 5.
#'                        \item \code{embed.nsplit}: number of random cutting points. Default: 3.
#'                        \item \code{embed.resample.replace}: whether to sample
#'                              with replacement. Default: TRUE.
#'                        \item \code{embed.resample.prob}: proportion of samples
#'                              (of the internal node) in the embedded model. Default: 0.9.
#'                        \item \code{embed.mute}: variables to mute per split.
#'                              If >= 1: exact count; if < 1: proportion. Default: 0 (no muting).
#'                        \item \code{embed.protect}: number of top variables to protect
#'                              from muting. Default: ceiling(log(n)).
#'                        \item \code{embed.threshold}: threshold, as a fraction
#'                              of the best VI, for being included in the protected
#'                              set at an internal node. Default: 0.25.
#'                        \item \code{linear.comb}: number of variables to use in linear
#'                              combination splits. Requires \code{reinforcement = TRUE}.
#'                              Default: 1 (no linear combination).
#'                        \item \code{linear.comb.method}: method for constructing linear
#'                              combination splits. Regression: \code{"naive"} (1),
#'                              \code{"lm"} (2), \code{"pca"} (3), \code{"sir"} (4, default).
#'                        Classification: \code{"lda"} (1, default), \code{"naive"} (2),
#'                              \code{"random"} (3), \code{"logistic"} (4).
#'                        \item \code{time.grid.size}: number of unique time points for
#'                              survival estimation (default 0 = all). See \code{time.grid.size}
#'                              argument for details.
#'                        }
#'
#'                        See \code{linear.comb} and \code{linear.comb.method} under
#'                        \code{param.control} documentation above.
#'
#'                        \code{split.rule} specifies the splitting criterion for each model type.
#'                        \itemize{
#'                        \item \strong{Regression}: \code{"var"} (variance reduction, default and only option)
#'                        \item \strong{Classification}: \code{"gini"} (Gini index, default and only option)
#'                        \item \strong{Survival}: \code{"logrank"} (default), \code{"suplogrank"}, \code{"coxgrad"}
#'                        }
#'                        Internally mapped to integers: var=1, gini=1, logrank=1, suplogrank=2, coxgrad=3.
#'
#'                        \code{resample.track} indicates whether to keep track
#'                        of which observations are used in each tree. This is
#'                        required for variance estimation (via \code{var.mode}).
#'
#'                        \code{var.mode} specifies the variance estimation method
#'                        to prepare during model fitting. Currently available methods:
#'                        \itemize{
#'                        \item \code{"none"} (default): No variance estimation.
#'                        \item \code{"matched"}: Uses matched-sample U-statistic
#'                        decomposition (Xu, Zhu & Shao, 2023) for prediction
#'                        variance and variable importance variance. Also used for
#'                        confidence band in survival models (Formentini, Liang & Zhu, 2023).
#'                        }
#'                        Specifying \code{var.mode = TRUE} is equivalent to
#'                        \code{var.mode = "matched"}.
#'                        When \code{var.mode} is not \code{"none"}, the following
#'                        parameters are automatically adjusted if not already set:
#'                        \itemize{
#'                        \item \code{resample.preset} is constructed automatically
#'                        \item \code{resample.replace} is set to \code{FALSE}
#'                        \item \code{resample.prob} is set to 0.5
#'                        \item \code{resample.track} is set to \code{TRUE}
#'                        \item \code{importance} is set to \code{"distribute"}
#'                        }
#'
#'                        It is recommended to use a very large \code{ntrees},
#'                        e.g, 10000 or larger. For \code{resample.prob} greater
#'                        than 0.5, one should consider the bootstrap
#'                        approach in Xu, Zhu & Shao (2023).
#'
#'                        \\code{time.grid.size} specifies the number of unique
#'                        time points used for survival estimation. By default
#'                        (0), all observed failure times are used. Setting a
#'                        smaller number (e.g., 50) can speed up computation
#'                        for large datasets. The time points are selected at
#'                        evenly spaced quantiles of the observed failure times,
#'                        always including the minimum and maximum failure times.
#'
#' @param ncores          Number of CPU logical cores. Default is 0 (using all
#'                        available cores).
#'
#' @param verbose         Whether info should be printed.
#'
#' @param seed            Random seed number to replicate a previously fitted forest.
#'                        Internally, the `xoshiro256++` generator is used. If not
#'                        specified, a seed will be generated automatically and
#'                        recorded.
#'
#' @param ...             Additional arguments.
#'
#' @return
#'
#' A \code{RLT} fitted object, constructed as a list consisting
#' \item{FittedForest}{Fitted tree structures}
#' \item{VarImp}{Variable importance measures, if \code{importance = TRUE}}
#' \item{Prediction}{Out-of-bag prediction}
#' \item{Error}{Out-of-bag prediction error, adaptive to the model type}
#' \item{ObsTrack}{Provided if \code{resample.track = TRUE}, \code{var.mode != "none"},
#'                 or if \code{resample.preset} was supplied. This is an \code{n} \eqn{\times} \code{ntrees}
#'                 matrix that has the same meaning as \code{resample.preset}.}
#'
#' For classification forests, these items are further provided or will replace
#' the regression version
#' \item{NClass}{The number of classes}
#' \item{Prob}{Out-of-bag predicted probability}
#'
#' For survival forests, these items are further provided or will replace the
#' regression version
#' \item{timepoints}{ordered observed failure times}
#' \item{NFail}{The number of observed failure times}
#' \item{Prediction}{Out-of-bag prediction of hazard function}
#'
#' @references
#' \itemize{
#'  \item Zhu, R., Zeng, D., & Kosorok, M. R. (2015) "Reinforcement Learning Trees." Journal of the American Statistical Association. 110(512), 1770-1784.
#'  \item Xu, T., Zhu, R., & Shao, X. (2023) "On Variance Estimation of Random Forests with Infinite-Order U-statistics." arXiv preprint arXiv:2202.09008.
#'  \item Formentini, S. E., Wei L., & Zhu, R. (2022) "Confidence Band Estimation for Survival Random Forests." arXiv preprint arXiv:2204.12038.
#' }
#'
#' @examples
#' \donttest{
#'   set.seed(42)
#'   x <- matrix(rnorm(300 * 5), ncol = 5)
#'   y <- rowSums(x[, 1:2]) + rnorm(300)
#'   fit <- RLT(x, y, ntrees = 100)
#'   print(fit)
#' }
#' @export
RLT <- function(x, y, censor = NULL, model = NULL,
				ntrees = if (reinforcement) 100 else 500,
				mtry = max(1, as.integer(ncol(x)/2)),
				nmin = 5,
				alpha = 0,
				nsplit = 0,
				resample.replace = TRUE,
				resample.prob = if(resample.replace) 1 else 0.8,
				resample.preset = NULL,
				obs.w = NULL,
				var.prob = NULL,
		importance = FALSE,
			reinforcement = FALSE,
			linear.comb = 1,
			linear.comb.method = "default",
			split.rule = "default",
			var.mode = "none",
				param.control = list(),
				ncores = 0,
				verbose = 0,
				seed = NULL,
				...)
{
  # check model type
  if (is.null(model))
  {
    if (is.factor(y)) model = "classification"

    if (is.numeric(y) && !is.null(censor)) model = "survival"

    if (is.numeric(y) && is.null(censor)) model = "regression"

    if (is.null(model)) stop("Please specify the model type")
  }

  if (!match(model, c("regression", "classification",
                     "quantile", "survival"), nomatch = 0))
    stop("model type not recognized")

  # check input data
  if (missing(x)) stop("x is missing")

  if (missing(y)) stop("y is missing")

  if (model == "survival")
    if (missing(censor)) stop("censor is missing")

  p = ncol(x)
  n = nrow(x)

  # check some parameters
  # we only check common parameters here
  # model specific parameters will be checked inside each model fitting function
  ntrees = check_ntrees(ntrees)
  mtry = check_mtry(mtry, p)
  nmin = check_nmin(nmin)
  alpha = max(0, min(alpha, 0.5))
  storage.mode(alpha) <- "double"
  nsplit = check_nsplit(nsplit)
  resample.replace = check_resamplereplace(resample.replace)
  resample.prob = check_resampleprob(resample.prob)
  importance = check_importance(importance)
  reinforcement = check_reinforcement(reinforcement)
  ncores = check_ncores(ncores)
  verbose = check_verbose(verbose)
  seed = check_seed(seed) # will randomly generate seed if not provided

  # check observation weights
  if (is.null(obs.w))
  {
    obs.w = ARMA_EMPTY_VEC()
    use.obs.w = 0L
  }else{
    obs.w = check_obsw(obs.w, n)
    use.obs.w = 1L
  }

  # check variable probabilities
  if (is.null(var.prob))
  {
    var.prob = ARMA_EMPTY_VEC()
    use.var.prob = 0L
  }else{
    var.prob = check_varprob(var.prob, p)
    use.var.prob = 1L
  }

  #
  param = list("n" = n,
               "p" = p,
               "ntrees" = ntrees,
               "mtry" = mtry,
               "nmin" = nmin,
               "alpha" = alpha,
               "nsplit" = nsplit,
               "resample.replace" = resample.replace,
               "resample.prob" = resample.prob,
               "use.obs.w" = use.obs.w,
               "use.var.prob" = use.var.prob,
               "importance" = importance,
               "reinforcement" = reinforcement,
               "ncores" = ncores,
               "verbose" = verbose,
               "seed" = seed)

  # time.grid.size for survival
  if (is.null(param.control$time.grid.size)) {
    time.grid.size <- 0
  } else time.grid.size = param.control$time.grid.size

  # inject var.mode from top-level argument into param.control
  # (user can also pass via param.control, but top-level takes precedence)
  if (!identical(var.mode, "none"))
    param.control$var.mode <- var.mode

  # inject linear.comb and linear.comb.method from top-level arguments
  if (!identical(linear.comb, 1))
    param.control$linear.comb <- linear.comb
  if (!identical(linear.comb.method, "default"))
    param.control$"linear.comb.method" <- linear.comb.method
  if (!identical(split.rule, "default"))
    param.control$"split.rule" <- split.rule

  # check control parameters
  param.control = check_control(param.control, param)

  # check importance feasibility (OOB size vs importance type)
  check_importance_settings(importance, n, resample.replace, resample.prob)

  # reset some parameters if var.mode is needed
  if (param.control$var.mode != "none")
  {
    var.msg = character()
    vm = param.control$var.mode

    # --- matched U-statistic specific settings ---
    if (vm == "matched")
    {
      # subsampling (no replacement)
      if (param$resample.replace)
      {
        var.msg = c(var.msg, paste0("  resample.replace: TRUE -> FALSE"))
        param$resample.replace = 0L
      }

      # resample.prob = 0.5 (each half gets n/2)
      if (param$resample.prob != 0.5)
      {
        var.msg = c(var.msg, paste0("  resample.prob: ", param$resample.prob, " -> 0.5"))
        param$resample.prob = 0.5
      }

      # even ntrees (first half paired with second half)
      if (param$ntrees %% 2 != 0)
      {
        old.ntrees = param$ntrees
        param$ntrees = 2*floor(param$ntrees/2)
        var.msg = c(var.msg, paste0("  ntrees: ", old.ntrees, " -> ", param$ntrees))
      }

      if (param$ntrees < 2)
        stop("var.mode requires at least 2 trees for matched sampling")

      # resample.preset auto-generated by matched sampling
      if (!is.null(resample.preset))
        var.msg = c(var.msg, "  resample.preset: user-supplied -> auto-generated (matched sampling)")
    }

    # --- IJ / jackknife specific settings ---
    if (vm %in% c("IJ", "jack"))
    {
      # IJ/jack work best with bootstrap sampling; ensure it is enabled
      if (!param$resample.replace)
      {
        var.msg = c(var.msg, paste0("  resample.replace: FALSE -> TRUE"))
        param$resample.replace = 1L
      }

      # recommend large B for stable variance estimates
      if (param$ntrees < 2000)
        message("Note: ", vm, " variance estimation is more stable with ntrees >= 2000 (current: ",
                param$ntrees, ")")

      # resample.preset auto-generated for standard bootstrap tracking
      if (!is.null(resample.preset))
        var.msg = c(var.msg, "  resample.preset: user-supplied -> auto-generated (bootstrap tracking)")
    }

    # --- common for all variance modes ---
    # importance must be set for VI variance estimation
    if (param$importance == 0)
    {
      # user did not request importance; enable it automatically
      var.msg = c(var.msg, paste0("  importance: FALSE -> distribute"))
      param$importance = 2L
    }

    # output summary of changes
    if (length(var.msg) > 0)
      message("var.mode ('", vm, "') adjusted the following settings:\n",
              paste(var.msg, collapse = "\n"))
  }

  # construct resample.preset
  if (is.null(resample.preset) && param.control$var.mode == "none")
  {
    resample.preset = ARMA_EMPTY_UMAT()
  }else if (param.control$var.mode %in% c("IJ", "jack")){
    # IJ/jack: empty ObsTrack, let C++ fill it with standard bootstrap counts
    resample.preset = ARMA_EMPTY_UMAT()
    param.control$resample.track = 1L
  }else{
    resample.preset = check_resamplepreset(resample.preset, param, param.control)
    param.control$resample.track = 1L
  }

  # prepare x, continuous and categorical
  if (is.data.frame(x))
  {
    # data.frame, check for categorical variables
    xlevels <- lapply(x, function(x) if (is.factor(x)) levels(x) else 0)
    ncat <- sapply(xlevels, length)

    ## Treat ordered factors as numeric.
    ncat <- ifelse(sapply(x, is.ordered), 1, ncat)
  }else{
    # numerical matrix for x, all continuous
    ncat <- rep(1, p)
    names(ncat) <- colnames(x)
    xlevels <- as.list(rep(0, p))

    # Check for variables with very few unique values (potential categoricals)
    for (j in 1:p)
    {
      n_unique <- length(unique(x[, j]))
      if (n_unique <= 10)
      {
        col_name <- if (!is.null(colnames(x))) colnames(x)[j] else paste0("V", j)
        warning(sprintf(
          "Variable '%s' (column %d) has only %d unique values. Consider converting to a factor and using a data.frame input to treat it as categorical.",
          col_name, j, n_unique), call. = FALSE)
      }
    }
  }

  storage.mode(ncat) <- "integer"

  if (max(ncat) > 53)
    stop("cannot handle categorical predictors with more than 53 categories")

  xnames = colnames(x)
  x <- data.matrix(x)

  # set all parameters
  # convert var.mode string to integer for C++
  param.control$var.mode = switch(param.control$var.mode,
                                   "none" = 0L,
                                   "matched" = 1L,
                                   "IJ" = 2L,
                                   "jack" = 3L)
  param.all = append(param, param.control)

  # fit model

  if (model == "regression")
  {
    RLT.fit = RegForest(x, y, ncat,
                        obs.w, var.prob,
                        resample.preset,
                        param.all, ...)
  }

  if (model == "classification")
  {
    RLT.fit = ClaForest(x, y, ncat,
                        obs.w, var.prob,
                        resample.preset,
                        param.all, ...)
  }

  if (model == "quantile")
  {
    RLT.fit = QuanForest(x, y, ncat,
                        obs.w, var.prob,
                        resample.preset,
                        param.all, ...)
  }

  if (model == "survival")
  {
    RLT.fit = SurvForest(x, y, censor,
                         ncat, time.grid.size,
                         obs.w, var.prob,
                         resample.preset,
                         param.all, ...)
  }

  RLT.fit$"xnames" = xnames

  if (importance)
    rownames(RLT.fit$"VarImp") = xnames

  return(RLT.fit)
}


