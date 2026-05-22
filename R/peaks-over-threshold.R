#' Identify exceedances above a threshold
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold value.
#'
#' @return Numeric vector of exceedances (values above `threshold`). If no
#'   values exceed `threshold` an empty vector is returned.
#' @seealso [fit_gpd()] for fitting GPD to exceedances,
#'   [threshold_exceedances()] for indices of exceedances,
#'   [block_maxima()] for alternative block maxima approach
#' @examples
#' exceedances(rnorm(100), 1.5)
#' @export
exceedances <- function(x, threshold) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_number(threshold)
  x[x > threshold]
}

#' Fit a Generalized Pareto Distribution (GPD)
#'
#' Attempts to fit a GPD to the exceedances above `threshold`.
#' Uses `evd::fpot` if available, otherwise `evir::gpd` or
#' `ismev::gpd.fit` as fallbacks.
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold defining exceedances.
#'
#' @return Fitted model object with class `chaotic_model` plus the original
#'   backend class. Stops with an error if none of the supporting
#'   GPD-fitting packages (`evd`, `evir`, `ismev`) are installed.
#' @references
#' Pickands, J. (1975). Statistical inference using extreme order statistics.
#' The Annals of Statistics, 3(1), 119-131.
#'
#' Davison, A. C., and Smith, R. L. (1990). Models for exceedances over high thresholds.
#' Journal of the Royal Statistical Society: Series B (Methodological), 52(3), 393-425.
#' @seealso [exceedances()] for extracting exceedances,
#'   [threshold_diagnostics()] for threshold selection,
#'   [fit_gev()] for alternative block maxima approach
#' @examples
#' fit_gpd(rnorm(1000), 1.5)
#' @export
fit_gpd <- function(x, threshold) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_number(threshold)
  if (requireNamespace("evd", quietly = TRUE)) {
    wrap_chaotic_model(
      evd::fpot(x, threshold),
      model = "gpd",
      method = "evd::fpot",
      threshold = threshold
    )
  } else if (requireNamespace("evir", quietly = TRUE)) {
    wrap_chaotic_model(
      evir::gpd(x, threshold),
      model = "gpd",
      method = "evir::gpd",
      threshold = threshold
    )
  } else if (requireNamespace("ismev", quietly = TRUE)) {
    wrap_chaotic_model(
      ismev::gpd.fit(x, threshold, show = FALSE),
      model = "gpd",
      method = "ismev::gpd.fit",
      threshold = threshold
    )
  } else {
    stop("One of 'evd', 'evir' or 'ismev' packages is required")
  }
}

#' Fit a Poisson point-process likelihood (PPL) model
#'
#' @description
#' Fits the GEV-parameterised Poisson point-process likelihood of Coles
#' (2001, §7.4) to threshold exceedances. The PPL unifies block-maxima
#' and peaks-over-threshold inference: both can be derived as
#' marginalisations of the same Poisson point process. Compared with
#' [fit_gpd()], the PPL returns parameters \eqn{(\mu, \sigma, \xi)}
#' directly on the block-maximum GEV scale, removing the need to
#' back-transform GPD scale to a return level.
#'
#' @details
#' For a series of length `n_y` observations and threshold \eqn{u}, with
#' `n_per_block` observations per (notional) block, the log-likelihood is
#' \deqn{\ell(\mu, \sigma, \xi) = -\frac{n_y}{n_{\text{pb}}}
#'   \big(1 + \xi\,(u - \mu)/\sigma\big)^{-1/\xi} - k \log \sigma -
#'   (1 + 1/\xi) \sum_{i=1}^{k} \log\big(1 + \xi (x_i - \mu)/\sigma\big),}
#' where \eqn{x_1, \ldots, x_k} are the observed exceedances and
#' \eqn{n_{\text{pb}}} is the number of observations per block (e.g. 365 for
#' daily series with annual blocks). The \eqn{\xi = 0} (Gumbel) case is
#' handled separately.
#'
#' The implementation wraps `evd::fpot(model = "pp")`, which performs the
#' maximum-likelihood fit by numerical optimisation. The returned
#' parameters are the *annual* (block-size `n_per_block`) GEV parameters,
#' regardless of how many observations went into the fit.
#'
#' @param x Numeric vector. The raw time series, not just the exceedances.
#' @param threshold Numeric scalar. The high threshold \eqn{u}.
#' @param n_per_block Numeric (\eqn{\ge 1}). Observations per notional
#'   block. Defaults to 365 (annual blocks for daily data); set to 1 if
#'   you want each observation to count as its own block.
#'
#' @return A `chaotic_model` with `model = "ppp"`, wrapping
#'   `evd::fpot(model = "pp")` and carrying the standard
#'   `(loc, scale, shape)` parameters on the block-maximum GEV scale.
#'
#' @references
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme
#' Values*. Springer, §7.4.
#'
#' @seealso [fit_gev()] for block-maxima inference, [fit_gpd()] for the
#'   POT marginal, [profile_return_level()] for return-level CIs from a
#'   PPL fit.
#'
#' @examples
#' set.seed(1)
#' x <- evd::rgev(2000, loc = 0, scale = 1, shape = 0.1)
#' u <- quantile(x, 0.9)
#' fit_ppp(x, threshold = u, n_per_block = 50)
#'
#' @export
fit_ppp <- function(x, threshold, n_per_block = 365) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 10L)
  checkmate::assert_number(threshold, finite = TRUE)
  checkmate::assert_number(n_per_block, lower = 1)
  if (!requireNamespace("evd", quietly = TRUE)) {
    stop("Package 'evd' is required for fit_ppp()")
  }
  fit <- evd::fpot(x, threshold, model = "pp", npp = n_per_block)
  wrap_chaotic_model(
    fit,
    model     = "ppp",
    method    = "evd::fpot(model = \"pp\")",
    threshold = threshold
  )
}

#' Mean Residual Life (MRL) values
#'
#' Computes the average excess above a sequence of thresholds.
#'
#' @param x Numeric vector of observations.
#' @param thresholds Numeric vector of thresholds to evaluate.
#'
#' @return Data frame with columns `threshold` and `mean_excess`. Thresholds with
#'   no exceedances produce `NA` in the `mean_excess` column.
#' @examples
#' mrl <- mean_residual_life(rnorm(1000), seq(0, 2, 0.2))
#' @export
mean_residual_life <- function(x, thresholds) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_numeric(thresholds, any.missing = FALSE)
  mean_excess <- sapply(thresholds, function(u) {
    exc <- x[x > u] - u
    if (length(exc) == 0) return(NA_real_)
    mean(exc)
  })
  data.frame(threshold = thresholds, mean_excess = mean_excess)
}

#' Plot Mean Residual Life (MRL)
#'
#' @param mrl_df Data frame as returned by [mean_residual_life()].
#'
#' @return ggplot object visualizing the MRL curve. Requires the **ggplot2**
#'   package.
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal
#' @examples
#' df <- mean_residual_life(rnorm(1000), seq(0, 2, 0.2))
#' mrl_plot(df)
#' @export
mrl_plot <- function(mrl_df) {
  checkmate::assert_data_frame(mrl_df)
  checkmate::assert_subset(c("threshold", "mean_excess"), names(mrl_df))
  ggplot(mrl_df, aes(x = threshold, y = mean_excess)) +
    geom_point() +
    geom_line() +
    labs(x = "Threshold", y = "Mean Excess",
         title = "Mean Residual Life Plot") +
    theme_minimal()
}
