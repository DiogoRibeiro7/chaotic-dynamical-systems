#' Bootstrap confidence intervals for the extremal index
#'
#' Implements a simple block bootstrap for the extremal index
#' using either the runs or intervals estimator.
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold for exceedances.
#' @param estimator Character string, either "runs" or "intervals".
#' @param method Deprecated alias for `estimator` kept for compatibility.
#' @param run_length Integer run parameter for the runs estimator.
#' @param block_size Integer block length for resampling.
#' @param bootstrap_type Character string, either `"moving_block"` (default)
#'   or `"stationary"`.
#' @param B Integer number of bootstrap replicates.
#' @param n_bootstrap Deprecated alias for `B` kept for compatibility.
#' @param parallel Logical. If TRUE use parallel processing via
#'   [parallel::mclapply()] where available, otherwise sequential evaluation.
#' @param n_cores Integer number of cores when `parallel = TRUE`. Defaults to
#'   `max(1, detectCores() - 1)`.
#' @param seed Optional integer seed for reproducible bootstrap replicates.
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
                                     method = NULL, run_length = 5L,
                                     block_size = 50L,
                                     bootstrap_type = c("moving_block", "stationary"),
                                     B = 1000L, n_bootstrap = NULL,
                                     parallel = FALSE, n_cores = NULL,
                                     seed = NULL) {
  if (!is.null(method)) {
    estimator <- method
  }
  estimator <- match.arg(estimator)
  bootstrap_type <- match.arg(bootstrap_type)
  if (!is.null(n_bootstrap)) {
    B <- n_bootstrap
  }

  checkmate::assert_numeric(x, len = NULL, any.missing = FALSE, min.len = 2)
  checkmate::assert_number(threshold)
  checkmate::assert_choice(estimator, c("runs", "intervals"))
  checkmate::assert_int(block_size, lower = 1)
  checkmate::assert_choice(bootstrap_type, c("moving_block", "stationary"))
  checkmate::assert_int(B, lower = 2)
  checkmate::assert_flag(parallel)
  checkmate::assert_int(n_cores, lower = 1, null.ok = TRUE)
  checkmate::assert_int(seed, null.ok = TRUE)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- length(x)
  n_blocks <- ceiling(n / block_size)

  if (n < block_size) {
    stop("block_size must be <= length(x)")
  }

  # pre-generate resampling starts so results are reproducible for both
  # sequential and parallel execution
  start_grid <- matrix(
    sample(seq_len(n - block_size + 1), B * n_blocks, replace = TRUE),
    nrow = B,
    ncol = n_blocks
  )

  stationary_grid <- NULL
  if (bootstrap_type == "stationary") {
    # expected block length approximately equal to block_size
    p_new <- min(1, 1 / block_size)
    stationary_grid <- matrix(
      stats::runif(B * n),
      nrow = B,
      ncol = n
    )
  }

  resample_series <- function(b) {
    if (bootstrap_type == "moving_block") {
      start_idx <- start_grid[b, ]
      idx <- unlist(lapply(start_idx, function(s) s:(s + block_size - 1)))
      return(x[idx[1:n]])
    }

    # stationary bootstrap
    u <- stationary_grid[b, ]
    idx <- integer(n)
    idx[1] <- sample.int(n, 1)
    for (i in 2:n) {
      if (u[i] < p_new) {
        idx[i] <- sample.int(n, 1)
      } else {
        idx[i] <- idx[i - 1] + 1L
        if (idx[i] > n) {
          idx[i] <- 1L
        }
      }
    }
    x[idx]
  }

  sanitize_theta <- function(est) {
    if (!is.numeric(est) || length(est) != 1L || !is.finite(est)) {
      return(NA_real_)
    }
    est <- as.numeric(est)
    min(1, max(0, est))
  }

  estimate_one <- function(b) {
    z <- resample_series(b)
    est <- tryCatch(
      est_fun(z),
      error = function(e) NA_real_
    )
    sanitize_theta(est)
  }

  # choose estimator function
  est_fun <- if (estimator == "runs") {
    function(z) extremal_index_runs(z, threshold, run_length)
  } else {
    function(z) extremal_index_intervals(z, threshold)
  }

  theta_hat <- sanitize_theta(tryCatch(est_fun(x), error = function(e) NA_real_))

  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1L, parallel::detectCores() - 1L)
    }
    use_mclapply <- .Platform$OS.type != "windows" &&
      requireNamespace("parallel", quietly = TRUE) &&
      n_cores > 1L
    if (use_mclapply) {
      boots <- unlist(parallel::mclapply(
        seq_len(B),
        estimate_one,
        mc.cores = n_cores
      ))
    } else {
      boots <- vapply(seq_len(B), estimate_one, numeric(1))
    }
  } else {
    boots <- vapply(seq_len(B), estimate_one, numeric(1))
  }

  ci <- stats::quantile(boots, c(0.025, 0.975), na.rm = TRUE)
  list(estimate = theta_hat,
       theta_hat = theta_hat,
       replicates = boots,
       ci = ci,
       estimator = estimator,
       bootstrap_type = bootstrap_type,
       block_size = block_size,
       B = B,
       seed = seed)
}
