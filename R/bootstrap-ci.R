#' Bootstrap confidence intervals for the extremal index
#'
#' Implements a simple block bootstrap for the extremal index
#' using either the runs or intervals estimator.
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold for exceedances.
#' @param estimator Character string, either "runs" or "intervals".
#' @param run_length Integer run parameter for the runs estimator.
#' @param block_size Integer block length for resampling.
#' @param B Integer number of bootstrap replicates.
#' @param parallel Logical. If TRUE use parallel processing via
#'   [parallel::mclapply()].
#'
#' @return A list with elements `theta_hat`, the point estimate; `replicates`,
#'   the bootstrap sample of extremal index estimates; and `ci`, the 95%
#'   percentile confidence interval. Returns `NA` values in the replicates if
#'   the estimator fails on a resampled series.
#' @examples
#' set.seed(123)
#' x <- arima.sim(model = list(ar = 0.7), n = 2000)
#' thr <- quantile(x, 0.95)
#' boot <- bootstrap_extremal_index(x, thr, estimator = "runs",
#'                                  run_length = 5, block_size = 50, B = 200)
#' boot$ci
#' @export
bootstrap_extremal_index <- function(x, threshold, estimator = c("runs", "intervals"),
                                     run_length = 5L, block_size = 50L, B = 1000L,
                                     parallel = FALSE) {
  estimator <- match.arg(estimator)
  checkmate::assert_numeric(x, len = NULL, any.missing = FALSE, min.len = 2)
  checkmate::assert_number(threshold)
  checkmate::assert_int(block_size, lower = 1)
  checkmate::assert_int(B, lower = 2)
  n <- length(x)
  n_blocks <- ceiling(n / block_size)
  # helper for block bootstrap resampling
  resample_series <- function() {
    start_idx <- sample(seq_len(n - block_size + 1), n_blocks, replace = TRUE)
    idx <- unlist(lapply(start_idx, function(s) s:(s + block_size - 1)))
    x[idx[1:n]]
  }
  # choose estimator function
  est_fun <- if (estimator == "runs") {
    function(z) extremal_index_runs(z, threshold, run_length)
  } else {
    function(z) extremal_index_intervals(z, threshold)
  }
  theta_hat <- est_fun(x)
  if (parallel) {
    cores <- max(1L, parallel::detectCores() - 1L)
    boots <- unlist(parallel::mclapply(seq_len(B), function(i) {
      est_fun(resample_series())
    }, mc.cores = cores))
  } else {
    boots <- replicate(B, est_fun(resample_series()))
  }
  ci <- quantile(boots, c(0.025, 0.975), na.rm = TRUE)
  list(theta_hat = theta_hat,
       replicates = boots,
       ci = ci)
}
