#' Mixing diagnostics utilities
#'
#' Provides functions to compute auto-correlation decay, estimate simple mixing
#' coefficients, and check Leadbetter's D(un) condition for extreme value
#' theory applications.
#'
#' @param x Numeric vector containing the time series.
#' @param lags Integer vector of lags for correlation or mixing calculations.
#' @param threshold Numeric threshold for exceedance-based tests.
#'
#' @return Depends on the function. For `acf_decay` this is a numeric vector of
#'   autocorrelation values. Returns `NA` when `lags` contains values larger than
#'   the length of the series.
#' @examples
#' # Simulate logistic map
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#'
#' # Compute ACF decay
#' acf_vals <- acf_decay(x, lags = 1:10)
#' plot(1:10, acf_vals, type = "b", xlab = "Lag", ylab = "ACF")
#' @export
acf_decay <- function(x, lags) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_integerish(lags, any.missing = FALSE)
  stats::acf(x, lag.max = max(lags), plot = FALSE)$acf[lags + 1]
}

#' Estimate simple mixing coefficients
#'
#' Computes empirical alpha-mixing coefficients using indicator functions of
#' threshold exceedances separated by specified lags.
#'
#' @param x Numeric vector containing the time series.
#' @param threshold Numeric exceedance threshold.
#' @param lags Integer vector of lags at which to estimate coefficients.
#'
#' @return Numeric vector of estimated coefficients corresponding to each lag.
#'   For lags greater than the series length the function returns `NA`.
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' mix_coef <- mixing_coefficients(x, threshold = 0.9, lags = 1:10)
#' plot(1:10, mix_coef, type = "b", xlab = "Lag", ylab = "Mixing Coefficient")
#' @export
mixing_coefficients <- function(x, threshold, lags) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_number(threshold)
  checkmate::assert_integerish(lags, any.missing = FALSE)
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
#' @param x Numeric vector containing the time series.
#' @param threshold High threshold `u_n` for exceedances.
#' @param r Integer lag between exceedances.
#'
#' @return Logical indicating whether the empirical estimate suggests the D(un)
#'   condition holds at lag `r`. Returns `FALSE` when `r` exceeds the length of
#'   the series.
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' d_check(x, threshold = 0.9, r = 5)
#' @export
d_check <- function(x, threshold, r) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_number(threshold)
  checkmate::assert_int(r, lower = 1)
  exc <- as.integer(x > threshold)
  n <- length(exc)
  if (r >= n) return(FALSE)
  p1 <- mean(exc)
  p2 <- mean(exc[1:(n - r)] * exc[(r + 1):n])
  abs(p2 - p1^2) < sqrt(p1 / n)
}
