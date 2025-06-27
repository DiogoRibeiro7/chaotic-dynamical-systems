#' @title Extremal Index and Hitting Time Statistics
#' @description
#' Implements estimation of the extremal index and empirical hitting/return time statistics
#' for a univariate stationary time series, following Freitas, Freitas & Todd (2012).
#' Includes:
#' \itemize{
#'   \item threshold_exceedances: Identify exceedance indices above threshold.
#'   \item cluster_exceedances: Group exceedances into clusters using runs method.
#'   \item extremal_index_runs: Estimate extremal index via runs estimator.
#'   \item extremal_index_intervals: Estimate extremal index via intervals estimator.
#'   \item hitting_times: Compute hitting/return time distribution.
#'   \item plot_hts: Plot empirical vs theoretical exponential survival.
#' }
#' @details
#' The runs estimator defines clusters as exceedances separated by at most r-1 non-exceedances.
#' \code{theta_hat} = number of clusters / total exceedances.
#' The intervals estimator uses inter-exceedance gaps (Ferro & Segers, 2003).
#' Hitting times are measured in units of index distance.
#'
#' @author Diogo
#' @examples
#' # Generate synthetic AR(1) process and estimate
#' set.seed(42)
#' x <- arima.sim(model=list(ar=0.7), n=10000)
#' thr <- quantile(x, 0.95)
' -> documentation
# ensure dependencies
if (!requireNamespace("assertthat", quietly=TRUE)) {
  stop("Package 'assertthat' is required")
}

#' Identify indices of threshold exceedances
#'
#' @param x numeric vector: time series data.
#' @param threshold numeric: threshold value.
#' @return integer vector of indices where x > threshold.
#' @examples
#' threshold_exceedances(rnorm(100), 1.5)
threshold_exceedances <- function(x, threshold) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  assertthat::assert_that(is.numeric(threshold), length(threshold) == 1)
  which(x > threshold)
}

#' Group exceedances into clusters via runs method
#'
#' @param indices integer vector: sorted exceedance indices.
#' @param run_length integer: maximum gap to join exceedances into one cluster.
#' @return list with components:
#'   clusters: list of integer vectors (indices per cluster),
#'   n_clusters: integer number of clusters.
#' @examples
#' cluster_exceedances(1:10, run_length=2)
cluster_exceedances <- function(indices, run_length) {
  assertthat::assert_that(is.integer(indices) || is.numeric(indices))
  assertthat::assert_that(is.count(run_length))
  if (length(indices) == 0) {
    return(list(clusters = list(), n_clusters = 0L))
  }
  # ensure integer, sorted
  idx <- as.integer(indices)
  idx <- sort(idx)
  clusters <- list()
  current <- idx[1]
  clusters[[1]] <- current
n <- 1L
  for (i in seq(2L, length(idx))) {
    if (idx[i] - idx[i-1] <= run_length) {
      # same cluster
      clusters[[n]] <- c(clusters[[n]], idx[i])
    } else {
      # new cluster
      n <- n + 1L
      clusters[[n]] <- idx[i]
    }
  }
  list(clusters = clusters, n_clusters = as.integer(length(clusters)))
}

#' Runs estimator of extremal index
#'
#' @param x numeric vector: time series data.
#' @param threshold numeric: threshold value.
#' @param run_length integer: run parameter r in runs method.
#' @return numeric: estimated extremal index theta_hat.
#' @examples
#' extremal_index_runs(rnorm(1000), quantile(rnorm(1000),0.95), 5)
extremal_index_runs <- function(x, threshold, run_length) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  assertthat::assert_that(is.numeric(threshold), length(threshold) == 1)
  assertthat::assert_that(is.count(run_length))
  ix <- threshold_exceedances(x, threshold)
  ce <- cluster_exceedances(ix, run_length)
  n_exc <- length(ix)
  # avoid division by zero
  if (n_exc == 0) return(NA_real_)
  theta_hat <- ce$n_clusters / n_exc
  theta_hat
}

#' Intervals estimator of extremal index (Ferro & Segers)
#'
#' @param x numeric vector: time series data.
#' @param threshold numeric: threshold value.
#' @return numeric: estimated extremal index theta_hat.
#' @references Ferro, C.A.T., Segers, J. (2003)
#' @examples
#' extremal_index_intervals(rnorm(1000), 1.5)
extremal_index_intervals <- function(x, threshold) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  ix <- threshold_exceedances(x, threshold)
  if (length(ix) < 2) return(NA_real_)
  # inter-exceedance gaps
  gaps <- diff(ix)
  # remove zeros
  gaps <- gaps[gaps > 0]
  # estimate theta: theta_hat = 2 * (sum(min(gaps,2)) / length(gaps)) - 1
  # alternative: theta_hat = 1 / mean(gaps)
  theta1 <- 2 * mean(pmin(gaps, 2)) - 1
  theta2 <- 1 / mean(gaps)
  # choose default
  theta1
}

#' Compute hitting/return times
#'
#' @param x numeric vector: time series data.
#' @param threshold numeric: threshold value.
#' @return numeric vector of gaps between exceedances.
#' @examples
#' hitting_times(rnorm(100), 1)
hitting_times <- function(x, threshold) {
  assertthat::assert_that(is.numeric(x), length(x) > 1)
  assertthat::assert_that(is.numeric(threshold), length(threshold) == 1)
  ix <- threshold_exceedances(x, threshold)
  if (length(ix) < 2) return(numeric(0))
  as.numeric(diff(ix))
}

#' Plot empirical hitting time survival vs exponential
#'
#' @param times numeric vector: observed hitting times.
#' @param theta numeric: extremal index estimate (rate parameter).
#' @return ggplot object.
#' @examples
#' times <- hitting_times(rnorm(1000),1.5)
#' plot_hts(times,0.3)
plot_hts <- function(times, theta) {
  assertthat::assert_that(is.numeric(times))
  assertthat::assert_that(is.numeric(theta), theta > 0)
  library(ggplot2)
  # empirical survival
  df <- data.frame(t = sort(times))
  df$emp_surv <- 1 - ecdf(times)(df$t)
  # theoretical
  df$theo_surv <- exp(-theta * df$t)
  ggplot(df, aes(x = t)) +
    geom_step(aes(y = emp_surv), direction = "hv") +
    geom_line(aes(y = theo_surv), linetype = "dashed") +
    labs(y = "Survival", title = "Hitting Time Statistics: Empirical vs Exponential") +
    theme_minimal()
}
