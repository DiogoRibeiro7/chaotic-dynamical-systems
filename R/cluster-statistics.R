#' Cluster size utilities
#'
#' Provides tools to compute the distribution of cluster sizes of threshold
#' exceedances, summarize them, and visualize the distribution.
#'
#' @param x Numeric vector containing the time series.
#' @param threshold Numeric threshold defining exceedances.
#' @param run_length Integer specifying the maximum gap between exceedances
#'     that belongs to the same cluster.
#'
#' @return Various depending on the function; see details below.
#' @export
cluster_sizes <- function(x, threshold, run_length) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_number(threshold)
  checkmate::assert_int(run_length, lower = 1)
  ix <- threshold_exceedances(x, threshold)
  ce <- cluster_exceedances(ix, as.integer(run_length))
  vapply(ce$clusters, length, integer(1))
}

#' Summary statistics for cluster sizes
#'
#' Computes the mean and variance of cluster sizes.
#'
#' @param sizes Integer vector of cluster sizes.
#'
#' @return Named numeric vector with elements `mean_size` and `var_size`.
#' @export
cluster_summary <- function(sizes) {
  checkmate::assert_numeric(sizes, any.missing = FALSE)
  c(mean_size = mean(sizes), var_size = var(sizes))
}

#' Plot cluster size distribution
#'
#' Creates a bar chart of cluster size frequencies.
#'
#' @param sizes Integer vector of cluster sizes.
#'
#' @return A ggplot object showing the distribution.
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @export
cluster_histogram <- function(sizes) {
  checkmate::assert_numeric(sizes, any.missing = FALSE)
  library(ggplot2)
  df <- as.data.frame(table(size = sizes))
  df$size <- as.integer(as.character(df$size))
  ggplot(df, aes(x = size, y = Freq)) +
    geom_col(fill = "steelblue") +
    labs(x = "Cluster size", y = "Frequency",
         title = "Cluster Size Distribution") +
    theme_minimal()
}
