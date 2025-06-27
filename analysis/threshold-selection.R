#' Threshold selection diagnostics
#'
#' Provides tools for assessing appropriate thresholds for POT analysis,
#' including Mean Residual Life (MRL) and Hill plots.
#'
#' @param x Numeric vector of observations.
#' @param thresholds Numeric vector of candidate thresholds for MRL.
#' @param k_values Integer vector of order statistics counts for the Hill plot.
#'
#' @return List with components `mrl` and `hill`, containing data frames for
#'   each diagnostic.
#' @examples
#' set.seed(123)
#' x <- rpois(1000, lambda = 3)
#' diag <- threshold_diagnostics(x, seq(0, 6, by = 0.5), 1:50)
#' mrl_plot(diag$mrl)
#' hill_plot(diag$hill)
#' @export
threshold_diagnostics <- function(x, thresholds, k_values) {
  stopifnot(is.numeric(x), is.numeric(thresholds), is.numeric(k_values))
  mrl_df <- mean_residual_life(x, thresholds)
  hill_df <- hill_estimates(x, k_values)
  list(mrl = mrl_df, hill = hill_df)
}

#' Hill estimator across k values
#'
#' Computes the Hill estimator for heavy-tail index over a range of
#' order statistics counts `k`.
#'
#' @param x Numeric vector of observations (positive values).
#' @param k_values Integer vector specifying number of top order statistics.
#'
#' @return Data frame with columns `k` and `hill` containing the estimates.
#' @examples
#' hill_estimates(rexp(1000), 1:50)
#' @export
hill_estimates <- function(x, k_values) {
  stopifnot(is.numeric(x), is.numeric(k_values))
  x <- sort(x, decreasing = TRUE)
  n <- length(x)
  k_values <- k_values[k_values < n]
  hill <- sapply(k_values, function(k) {
    mean(log(x[1:k])) - log(x[k + 1])
  })
  data.frame(k = k_values, hill = hill)
}

#' Plot Hill estimates
#'
#' @param hill_df Data frame as returned by [hill_estimates()].
#'
#' @return ggplot object visualizing the Hill plot.
#' @examples
#' df <- hill_estimates(rexp(1000), 1:50)
#' hill_plot(df)
#' @export
hill_plot <- function(hill_df) {
  stopifnot(is.data.frame(hill_df),
            all(c("k", "hill") %in% names(hill_df)))
  library(ggplot2)
  ggplot(hill_df, aes(x = k, y = hill)) +
    geom_line() +
    geom_point() +
    labs(x = "Order statistic k", y = "Hill estimate",
         title = "Hill Plot") +
    theme_minimal()
}

#' Plot Mean Residual Life (MRL)
#'
#' Creates a Mean Residual Life plot from a data frame produced by
#' [mean_residual_life()].
#'
#' @param mrl_df Data frame with columns `threshold` and `mean_excess`.
#'
#' @return ggplot object visualizing the MRL curve.
#' @examples
#' df <- mean_residual_life(rexp(1000), seq(0, 2, by = 0.2))
#' mrl_plot(df)
#' @export
mrl_plot <- function(mrl_df) {
  stopifnot(is.data.frame(mrl_df),
            all(c("threshold", "mean_excess") %in% names(mrl_df)))
  library(ggplot2)
  ggplot(mrl_df, aes(x = threshold, y = mean_excess)) +
    geom_point() +
    geom_line() +
    labs(x = "Threshold", y = "Mean Excess",
         title = "Mean Residual Life Plot") +
    theme_minimal()
}

if (identical(environment(), globalenv())) {
  set.seed(123)
  x <- rexp(1000)
  diag <- threshold_diagnostics(x, seq(0, 2, by = 0.2), 1:30)
  print(head(diag$mrl))
  print(head(diag$hill))
}
