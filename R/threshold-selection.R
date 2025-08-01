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
#'   each diagnostic. If some thresholds or `k_values` are invalid they are
#'   dropped silently from the output.
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
#'   Values of `k` greater than `length(x) - 1` are ignored.
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
#' @return ggplot object visualizing the Hill plot. Requires the **ggplot2**
#'   package.
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
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
