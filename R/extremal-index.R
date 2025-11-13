#' Extremal index and hitting time utilities
#'
#' Implements estimation of the extremal index and empirical return time
#' statistics for a univariate stationary time series.
#'
#' @name extremal_index
NULL

#' Identify indices of threshold exceedances
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold value.
#'
#' @return Integer vector of indices where `x > threshold`.
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' exc_idx <- threshold_exceedances(x, threshold = 0.9)
#' head(exc_idx)
#' @export
threshold_exceedances <- function(x, threshold) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  assertthat::assert_that(is.numeric(threshold), length(threshold) == 1)
  which(x > threshold)
}

#' Group exceedances into clusters via runs method
#'
#' @param indices Integer vector of sorted exceedance indices.
#' @param run_length Integer maximum gap to join exceedances into one cluster.
#'
#' @return List with elements `clusters` and `n_clusters`.
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' exc_idx <- threshold_exceedances(x, threshold = 0.9)
#' clusters <- cluster_exceedances(exc_idx, run_length = 2)
#' clusters$n_clusters
#' @export
cluster_exceedances <- function(indices, run_length) {
  assertthat::assert_that(is.integer(indices) || is.numeric(indices))
  assertthat::assert_that(assertthat::is.count(run_length))
  if (length(indices) == 0) {
    return(list(clusters = list(), n_clusters = 0L))
  }
  idx <- as.integer(indices)
  idx <- sort(idx)
  clusters <- list()
  clusters[[1]] <- idx[1]
  n <- 1L
  for (i in seq(2L, length(idx))) {
    if (idx[i] - idx[i-1] <= run_length) {
      clusters[[n]] <- c(clusters[[n]], idx[i])
    } else {
      n <- n + 1L
      clusters[[n]] <- idx[i]
    }
  }
  list(clusters = clusters, n_clusters = as.integer(length(clusters)))
}

#' Estimate Extremal Index Using Runs Method
#'
#' @description
#' Estimates the extremal index θ from time series data using the runs
#' estimator. The extremal index quantifies the degree of clustering in
#' extreme values, with θ = 1 indicating independence and θ < 1 indicating
#' clustering.
#'
#' @details
#' ## Overview
#' The extremal index θ ∈ (0, 1] is a fundamental parameter in extreme
#' value theory for dependent sequences. It measures the tendency of extreme
#' values to appear in clusters rather than individually.
#'
#' ## Interpretation of θ
#' - **θ = 1**: Extreme values occur independently (like IID data)
#' - **θ < 1**: Extreme values cluster together
#' - **θ = 0.5**: On average, extremes appear in pairs
#' - **θ = 0.33**: On average, extremes appear in triplets
#'
#' In chaotic dynamical systems, θ is typically less than 1 because the
#' system's deterministic nature causes extreme events to cluster. The
#' value of θ depends on the system's mixing properties and the choice
#' of threshold.
#'
#' ## The Runs Method
#' The runs estimator defines clusters based on the gaps between exceedances.
#' Two exceedances belong to the same cluster if they are separated by at
#' most `run_length` time steps. The estimator is:
#' \deqn{\hat{\theta} = \frac{N_c}{N}}
#' where \eqn{N_c} is the number of clusters and N is the total number of
#' exceedances.
#'
#' ## Choosing run_length
#' The choice of `run_length` affects the estimate:
#' - **Too small**: May split natural clusters, overestimating θ
#' - **Too large**: May merge distinct clusters, underestimating θ
#'
#' Common choices:
#' - run_length = 1 or 2 for most applications
#' - run_length based on ACF structure (first zero crossing)
#' - run_length based on extremal index diagnostics
#'
#' ## Relationship to Return Times
#' The extremal index affects return times for extreme events. For a
#' threshold u with exceedance probability p, the mean cluster size is
#' 1/θ and the mean inter-cluster time is 1/(θp).
#'
#' @section Mathematical Background:
#' For a stationary sequence {X_n}, the extremal index is defined as:
#' \deqn{\theta = \lim_{n \to \infty} \frac{P(M_n \le u_n)^n}{P(X_1 \le u_n)}}
#' where \eqn{M_n = \max(X_1, \ldots, X_n)} and \eqn{u_n} is a high threshold
#' satisfying nP(X_1 > u_n) → τ for some τ > 0.
#'
#' The runs estimator was introduced by Smith & Weissman (1994) and
#' provides a consistent estimator under appropriate mixing conditions.
#'
#' @param x Numeric vector. The time series to analyze. Should be stationary
#'   or at least have stationary extremal behavior. Must contain enough data
#'   to get reliable exceedance counts (typically n > 500 for threshold at
#'   90th-95th percentile).
#'
#' @param threshold Numeric scalar. Values above this are considered exceedances.
#'   Should be a high quantile of the data (typically 0.90-0.99). The choice
#'   affects both the estimate and its variance:
#'   - Higher threshold: fewer exceedances, higher variance, less bias
#'   - Lower threshold: more exceedances, lower variance, potential bias
#'   Use \code{\link{threshold_diagnostics}} for guidance on selection.
#'
#' @param run_length Integer. The maximum gap (in time steps) between
#'   exceedances that are considered part of the same cluster. Typical
#'   values are 1 or 2. Larger values may be appropriate for systems with
#'   long-range dependence. Must be positive.
#'
#' @return
#' Numeric scalar giving the extremal index estimate, a value in (0, 1].
#' Returns NA if there are no exceedances above the threshold.
#'
#' **Interpreting the result**:
#' - Values close to 1: weak clustering, nearly independent extremes
#' - Values around 0.5-0.7: moderate clustering (common in chaotic systems)
#' - Values below 0.5: strong clustering, extremes appear in bursts
#'
#' Use \code{\link{bootstrap_extremal_index}} to obtain confidence intervals
#' for quantifying uncertainty.
#'
#' @references
#' Smith, R. L., & Weissman, I. (1994). Estimating the extremal index.
#' *Journal of the Royal Statistical Society: Series B (Methodological)*,
#' 56(3), 515-528.
#'
#' Ferro, C. A. T., & Segers, J. (2003). Inference for clusters of extreme
#' values. *Journal of the Royal Statistical Society: Series B (Statistical
#' Methodology)*, 65(2), 545-556. \doi{10.1111/1467-9868.00401}
#'
#' Leadbetter, M. R. (1983). Extremes and local dependence in stationary
#' sequences. *Zeitschrift für Wahrscheinlichkeitstheorie und verwandte
#' Gebiete*, 65(2), 291-306. \doi{10.1007/BF00532484}
#'
#' @seealso
#' \code{\link{extremal_index_intervals}} for the alternative intervals estimator,
#' \code{\link{bootstrap_extremal_index}} to obtain confidence intervals,
#' \code{\link{cluster_sizes}} to analyze cluster structure,
#' \code{\link{threshold_exceedances}} for extracting exceedances.
#'
#' For a comprehensive tutorial, see \code{vignette("estimating-theta-logistic")}.
#'
#' @family extremal index functions
#'
#' @examples
#' # Simulate chaotic time series
#' x <- simulate_logistic_map(n = 2000, r = 3.8, x0 = 0.2)
#'
#' # Choose high threshold (95th percentile)
#' threshold <- quantile(x, 0.95)
#'
#' # Estimate extremal index with run_length = 2
#' theta <- extremal_index_runs(x, threshold, run_length = 2)
#' cat("Extremal index estimate:", round(theta, 3), "\n")
#'
#' # Interpretation
#' if (theta < 0.7) {
#'   cat("Strong clustering detected\n")
#'   cat("Mean cluster size:", round(1/theta, 2), "\n")
#' } else if (theta < 0.9) {
#'   cat("Moderate clustering detected\n")
#' } else {
#'   cat("Weak clustering, nearly independent extremes\n")
#' }
#'
#' # Compare different run_length values
#' cat("\nSensitivity to run_length:\n")
#' for (r in 1:5) {
#'   theta_r <- extremal_index_runs(x, threshold, run_length = r)
#'   cat("run_length =", r, ": theta =", round(theta_r, 3), "\n")
#' }
#'
#' # Effect of threshold choice
#' cat("\nSensitivity to threshold:\n")
#' for (q in c(0.90, 0.95, 0.99)) {
#'   thr <- quantile(x, q)
#'   theta_q <- extremal_index_runs(x, thr, run_length = 2)
#'   n_exc <- sum(x > thr)
#'   cat(sprintf("%.2f quantile: theta = %.3f (%d exceedances)\n",
#'               q, theta_q, n_exc))
#' }
#'
#' # Analyze cluster structure
#' exc_indices <- threshold_exceedances(x, threshold)
#' clusters <- cluster_exceedances(exc_indices, run_length = 2)
#' cat("\nCluster statistics:\n")
#' cat("Number of clusters:", clusters$n_clusters, "\n")
#' cat("Number of exceedances:", length(exc_indices), "\n")
#' cat("Theta:", clusters$n_clusters / length(exc_indices), "\n")
#'
#' # Visualize exceedances and clusters
#' plot(x, type = "l", col = "gray", main = "Time Series with Exceedances",
#'      xlab = "Time", ylab = "Value")
#' abline(h = threshold, col = "red", lty = 2, lwd = 2)
#' points(exc_indices, x[exc_indices], col = "darkred", pch = 16, cex = 0.8)
#' legend("topright", legend = c("Series", "Threshold", "Exceedances"),
#'        col = c("gray", "red", "darkred"),
#'        lty = c(1, 2, NA), pch = c(NA, NA, 16), bty = "n")
#'
#' \donttest{
#' # Get confidence interval via bootstrap
#' boot_result <- bootstrap_extremal_index(
#'   x, threshold, run_length = 2, B = 500
#' )
#' cat("\n95% Confidence Interval for theta:\n")
#' cat(sprintf("[%.3f, %.3f]\n", boot_result$ci[1], boot_result$ci[2]))
#' }
#'
#' @export
extremal_index_runs <- function(x, threshold, run_length) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  assertthat::assert_that(is.numeric(threshold), length(threshold) == 1)
  assertthat::assert_that(assertthat::is.count(run_length))
  ix <- threshold_exceedances(x, threshold)
  ce <- cluster_exceedances(ix, run_length)
  n_exc <- length(ix)
  if (n_exc == 0) return(NA_real_)
  ce$n_clusters / n_exc
}

#' Intervals estimator of extremal index (Ferro & Segers)
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold value.
#'
#' @return Estimated extremal index. Returns `NA` if fewer than two exceedances
#'   occur above `threshold`.
#' @references
#' Ferro, C. A. T., and Segers, J. (2003). Inference for clusters of extreme values.
#' Journal of the Royal Statistical Society: Series B (Statistical Methodology), 65(2), 545-556.
#' @seealso [extremal_index_runs()] for alternative runs estimator,
#'   [bootstrap_extremal_index()] for confidence intervals
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' theta <- extremal_index_intervals(x, threshold = 0.9)
#' theta
#' @export
extremal_index_intervals <- function(x, threshold) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  ix <- threshold_exceedances(x, threshold)
  if (length(ix) < 2) return(NA_real_)
  gaps <- diff(ix)
  gaps <- gaps[gaps > 0]
  2 * mean(pmin(gaps, 2)) - 1
}

#' Compute hitting/return times
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold value.
#'
#' @return Numeric vector of gaps between exceedances. If fewer than two
#'   exceedances are present the function returns an empty vector.
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' hts <- hitting_times(x, threshold = 0.9)
#' head(hts)
#' @export
hitting_times <- function(x, threshold) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  assertthat::assert_that(is.numeric(threshold), length(threshold) == 1)
  ix <- threshold_exceedances(x, threshold)
  if (length(ix) < 2) return(numeric(0))
  as.numeric(diff(ix))
}

#' Plot empirical hitting time survival vs exponential
#'
#' @param times Numeric vector of observed hitting times.
#' @param theta Numeric extremal index estimate (rate parameter).
#'
#' @return ggplot object. Requires the **ggplot2** package.
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' hts <- hitting_times(x, threshold = 0.9)
#' theta <- extremal_index_runs(x, threshold = 0.9, run_length = 2)
#' plot_hts(hts, theta)
#' @importFrom ggplot2 ggplot aes geom_step geom_line labs theme_minimal
#' @importFrom stats ecdf
#' @export
plot_hts <- function(times, theta) {
  assertthat::assert_that(is.numeric(times))
  assertthat::assert_that(is.numeric(theta), theta > 0)
  library(ggplot2)
  df <- data.frame(t = sort(times))
  df$emp_surv <- 1 - ecdf(times)(df$t)
  df$theo_surv <- exp(-theta * df$t)
  ggplot(df, aes(x = t)) +
    geom_step(aes(y = emp_surv), direction = "hv") +
    geom_line(aes(y = theo_surv), linetype = "dashed") +
    labs(y = "Survival", title = "Hitting Time Statistics: Empirical vs Exponential") +
    theme_minimal()
}
