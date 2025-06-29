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

#' Runs estimator of extremal index
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold value.
#' @param run_length Integer run parameter `r` in runs method.
#'
#' @return Estimated extremal index.
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
#' @return Estimated extremal index.
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
#' @return Numeric vector of gaps between exceedances.
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
#' @return ggplot object.
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
