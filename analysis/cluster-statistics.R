#' Cluster size utilities
#'
#' Provides tools to compute the distribution of cluster sizes of threshold
#' exceedances, summarize them, and visualize the distribution.
#'
#' Args:
#'   x: Numeric vector containing the time series.
#'   threshold: Numeric threshold defining exceedances.
#'   run_length: Integer specifying the maximum gap between exceedances that
#'     belongs to the same cluster.
#'
#' Returns:
#'   Various depending on the function; see details below.
#'
#' @export
cluster_sizes <- function(x, threshold, run_length) {
  # Validate input types
  stopifnot(is.numeric(x),
            is.numeric(threshold), length(threshold) == 1,
            is.numeric(run_length), run_length >= 1)
  # Use helper from extremal_index.R to find exceedances
  ix <- threshold_exceedances(x, threshold)
  # Form clusters using the runs method
  ce <- cluster_exceedances(ix, as.integer(run_length))
  # Return lengths of each cluster
  vapply(ce$clusters, length, integer(1))
}

#' Summary statistics for cluster sizes
#'
#' Computes the mean and variance of cluster sizes.
#'
#' Args:
#'   sizes: Integer vector of cluster sizes.
#'
#' Returns:
#'   Named numeric vector with elements `mean_size` and `var_size`.
#'
#' @export
cluster_summary <- function(sizes) {
  stopifnot(is.numeric(sizes))
  c(mean_size = mean(sizes), var_size = var(sizes))
}

#' Plot cluster size distribution
#'
#' Creates a bar chart of cluster size frequencies.
#'
#' Args:
#'   sizes: Integer vector of cluster sizes.
#'
#' Returns:
#'   A ggplot object showing the distribution.
#'
#' @export
cluster_histogram <- function(sizes) {
  stopifnot(is.numeric(sizes))
  library(ggplot2)
  df <- as.data.frame(table(size = sizes))
  df$size <- as.integer(as.character(df$size))
  ggplot(df, aes(x = size, y = Freq)) +
    geom_col(fill = "steelblue") +
    labs(x = "Cluster size", y = "Frequency",
         title = "Cluster Size Distribution") +
    theme_minimal()
}

if (identical(environment(), globalenv())) {
  set.seed(123)
  # Example AR(1) process
  x <- arima.sim(model = list(ar = 0.5), n = 2000)
  thr <- quantile(x, 0.95)
  sizes <- cluster_sizes(x, thr, 3)
  print(head(sizes))
  print(cluster_summary(sizes))
}
