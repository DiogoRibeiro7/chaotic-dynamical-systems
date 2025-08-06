#' Advanced extreme value analysis utilities
#'
#' This file collects higher-level helpers for multivariate and
#' non-stationary extreme value modelling.
#'
#' @name advanced_extremes
NULL

#' Adaptive threshold selection
#'
#' Computes a rolling high quantile over a sliding window to allow
#' for time-varying thresholds.
#'
#' @param x Numeric vector of observations.
#' @param window_size Integer size of the rolling window.
#' @param prob Numeric probability for the quantile, default 0.95.
#'
#' @return Numeric vector of length `length(x) - window_size + 1` with
#'   adaptive thresholds.
#' @examples
#' x <- rnorm(200)
#' thr <- adaptive_threshold_selection(x, 50)
#' @export
adaptive_threshold_selection <- function(x, window_size = 100L, prob = 0.95) {
  checkmate::assert_numeric(x)
  checkmate::assert_count(window_size)
  checkmate::assert_number(prob, lower = 0, upper = 1)
  checkmate::assert_true(length(x) >= window_size,
                         msg = "x must have length >= window_size")
  vapply(seq_len(length(x) - window_size + 1), function(i) {
    stats::quantile(x[i:(i + window_size - 1)], prob, type = 8, na.rm = TRUE)
  }, numeric(1))
}

#' Fit a simple non-stationary GEV model
#'
#' Estimates a GEV distribution allowing the location parameter to
#' vary linearly with time if `trend` is TRUE. This implementation uses
#' maximum likelihood from the \code{ismev} package when available and
#' falls back to optimisation via \code{stats::optim} otherwise.
#'
#' @param x Numeric vector of block maxima.
#' @param trend Logical. If TRUE include a linear trend in the location parameter.
#'
#' @return List containing estimated parameters.
#' @examples
#' x <- block_maxima(rnorm(1000), 50)
#' fit_nonstationary_gev(x)
#' @export
fit_nonstationary_gev <- function(x, trend = TRUE) {
  checkmate::assert_numeric(x, min.len = 2)
  checkmate::assert_flag(trend)
  time <- seq_along(x)
  if (requireNamespace("ismev", quietly = TRUE)) {
    locform <- if (trend) ~ time else ~ 1
    fit <- ismev::gev.fit(x, ydat = data.frame(time = time), mul = locform, show = FALSE)
    list(location = fit$mle[1], scale = fit$mle[2], shape = fit$mle[3])
  } else {
    loglik <- function(par) {
      mu <- if (trend) par[1] + par[4] * time else par[1]
      sigma <- abs(par[2])
      xi <- par[3]
      if (sigma <= 0) return(Inf)
      z <- (x - mu) / sigma
      if (abs(xi) < 1e-6) {
        ll <- -sum(log(sigma)) - sum(z) - sum(exp(-z))
      } else {
        t <- 1 + xi * z
        if (any(t <= 0)) return(Inf)
        ll <- -sum(log(sigma)) - (1/xi + 1) * sum(log(t)) - sum(t^(-1/xi))
      }
      -ll
    }
    p0 <- c(mean(x), sd(x), 0.1, 0)  # initial values
    fit <- stats::optim(p0, loglik)
    list(location = fit$par[1], scale = abs(fit$par[2]), shape = fit$par[3])
  }
}

#' Tail dependence coefficient
#'
#' Computes the upper tail dependence coefficient between two series.
#'
#' @param x Numeric vector.
#' @param y Numeric vector of the same length as `x`.
#' @param quantile_level Numeric in (0,1) indicating the high quantile.
#'
#' @return Numeric value in [0,1].
#' @examples
#' x <- rnorm(1000)
#' y <- x + rnorm(1000, sd = 0.5)
#' tail_dependence_coefficient(x, y)
#' @export
tail_dependence_coefficient <- function(x, y, quantile_level = 0.9) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(y, len = length(x))
  checkmate::assert_number(quantile_level, lower = 0, upper = 1)
  qx <- stats::quantile(x, quantile_level, type = 8, na.rm = TRUE)
  qy <- stats::quantile(y, quantile_level, type = 8, na.rm = TRUE)
  mean(x > qx & y > qy) / (1 - quantile_level)
}

#' Spectral analysis of extremes
#'
#' Performs a simple spectral density estimation on the indicator
#' series of exceedances above a threshold.
#'
#' @param x Numeric vector.
#' @param threshold Numeric threshold for exceedances.
#'
#' @return List with frequency and spectral density components as returned by
#'   [stats::spectrum()].
#' @examples
#' x <- rnorm(1000)
#' spectral_analysis_extremes(x, threshold = 2)
#' @export
spectral_analysis_extremes <- function(x, threshold) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)
  ind <- as.numeric(x > threshold)
  stats::spectrum(ind, plot = FALSE)
}

#' Calculate return levels from GPD fit
#'
#' Given a series and fitted GPD parameters, compute return levels for
#' specified return periods.
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold for exceedances.
#' @param return_periods Numeric vector of return periods.
#'
#' @return Numeric vector of return levels corresponding to `return_periods`.
#' @examples
#' x <- rnorm(1000)
#' rl <- calculate_return_levels(x, 2, c(10, 100))
#' @export
calculate_return_levels <- function(x, threshold, return_periods) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)
  checkmate::assert_numeric(return_periods, any.missing = FALSE)
  exc <- x[x > threshold] - threshold
  checkmate::assert_true(length(exc) >= 1, msg = "No exceedances above threshold")
  # simple GPD fit via method of moments
  scale <- mean(exc)
  shape <- -scale^2 / (var(exc) - scale^2)
  rate <- length(exc) / length(x)
  sapply(return_periods, function(T) {
    threshold + scale / shape * ((T * rate)^shape - 1)
  })
}

#' Validate extreme value model
#'
#' Performs a basic QQ plot comparison between empirical exceedances and
#' the fitted GPD model.
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold for exceedances.
#' @param method Currently only "qq" is implemented.
#'
#' @return List containing QQ plot data.
#' @export
validate_extreme_model <- function(x, threshold, method = "qq") {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)
  checkmate::assert_choice(method, choices = "qq")
  exc <- x[x > threshold] - threshold
  checkmate::assert_true(length(exc) >= 1, msg = "No exceedances above threshold")
  exc <- sort(exc)
  p <- ppoints(length(exc))
  q <- quantile(exc, p, type = 8)
  list(p = p, q = q, data = exc)
}

#' Goodness-of-fit test for GPD exceedances
#'
#' Performs a Kolmogorov-Smirnov test comparing the empirical
#' distribution of exceedances to the fitted GPD model using a
#' method-of-moments estimate.
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold.
#'
#' @return List with KS statistic and p-value.
#' @export
goodness_of_fit_test <- function(x, threshold) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)
  exc <- x[x > threshold] - threshold
  checkmate::assert_true(length(exc) >= 1, msg = "No exceedances above threshold")
  scale <- mean(exc)
  shape <- -scale^2 / (var(exc) - scale^2)
  pgpd <- function(z) 1 - (1 + shape * z / scale)^(-1/shape)
  ks <- stats::ks.test(exc, pgpd)
  list(statistic = ks$statistic, p_value = ks$p.value)
}

#' Moving block bootstrap resampler
#'
#' Resamples a time series using overlapping blocks of fixed length.
#'
#' @param x Numeric vector of observations.
#' @param block_length Integer length of each block.
#'
#' @return Numeric vector of resampled observations of the same length as `x`.
#' @examples
#' block_bootstrap(1:100, block_length = 10)
#' @export
block_bootstrap <- function(x, block_length) {
  checkmate::assert_numeric(x)
  checkmate::assert_count(block_length)
  checkmate::assert_true(length(x) > block_length,
                         msg = "block_length must be < length of x")
  n <- length(x)
  starts <- sample(seq_len(n - block_length + 1), n, replace = TRUE)
  idx <- unlist(lapply(starts, function(s) s:(s + block_length - 1)))
  x[idx[1:n]]
}
