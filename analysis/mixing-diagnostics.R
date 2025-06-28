#' Mixing diagnostics utilities
#'
#' Provides functions to compute auto-correlation decay, estimate simple mixing
#' coefficients, and check Leadbetter's D(un) condition for extreme value
#' theory applications.
#'
#' Args:
#'   x: Numeric vector containing the time series.
#'   lags: Integer vector of lags for correlation or mixing calculations.
#'   threshold: Numeric threshold for exceedance-based tests.
#'
#' Returns:
#'   Depends on the function. See individual descriptions.
#'
#' @export
acf_decay <- function(x, lags) {
  stopifnot(is.numeric(x), is.numeric(lags))
  stats::acf(x, lag.max = max(lags), plot = FALSE)$acf[lags + 1]
}

#' Estimate simple mixing coefficients
#'
#' Computes empirical alpha-mixing coefficients using indicator functions of
#' threshold exceedances separated by specified lags.
#'
#' Args:
#'   x: Numeric vector containing the time series.
#'   threshold: Numeric exceedance threshold.
#'   lags: Integer vector of lags at which to estimate coefficients.
#'
#' Returns:
#'   Numeric vector of estimated coefficients corresponding to each lag.
#'
#' @export
mixing_coefficients <- function(x, threshold, lags) {
  stopifnot(is.numeric(x), is.numeric(threshold), is.numeric(lags))
  exc <- as.integer(x > threshold)
  n <- length(exc)
  sapply(lags, function(k) {
    if (k >= n) return(NA_real_)
    prob_joint <- mean(exc[1:(n - k)] * exc[(k + 1):n])
    prob_prod <- mean(exc[1:(n - k)]) * mean(exc[(k + 1):n])
    abs(prob_joint - prob_prod)
  })
}

#' Check D(un) condition
#'
#' Evaluates a heuristic version of Leadbetter's D(un) mixing condition by
#' comparing joint exceedance probabilities at lag `r` with the product of
#' marginals for a high threshold `u_n`.
#'
#' Args:
#'   x: Numeric vector containing the time series.
#'   threshold: High threshold `u_n` for exceedances.
#'   r: Integer lag between exceedances.
#'
#' Returns:
#'   Logical indicating whether the empirical estimate suggests the D(un)
#'   condition holds at lag `r`.
#'
#' @export
d_check <- function(x, threshold, r) {
  stopifnot(is.numeric(x), is.numeric(threshold), length(threshold) == 1,
            is.numeric(r), r >= 1)
  exc <- as.integer(x > threshold)
  n <- length(exc)
  if (r >= n) return(FALSE)
  p1 <- mean(exc)
  p2 <- mean(exc[1:(n - r)] * exc[(r + 1):n])
  abs(p2 - p1^2) < sqrt(p1 / n)
}

if (identical(environment(), globalenv())) {
  set.seed(123)
  x <- arima.sim(model = list(ar = 0.6), n = 2000)
  lags <- 1:10
  print(acf_decay(x, lags))
  thr <- quantile(x, 0.95)
  print(mixing_coefficients(x, thr, lags))
  print(d_check(x, thr, 3))
}
