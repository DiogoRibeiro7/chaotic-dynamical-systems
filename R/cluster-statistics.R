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
#' @examples
#' # Simulate logistic map
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#'
#' # Compute cluster sizes
#' sizes <- cluster_sizes(x, threshold = 0.9, run_length = 2)
#' head(sizes)
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
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' sizes <- cluster_sizes(x, threshold = 0.9, run_length = 2)
#' cluster_summary(sizes)
#' @export
cluster_summary <- function(sizes) {
  checkmate::assert_numeric(sizes, any.missing = FALSE)
  c(mean_size = mean(sizes), var_size = var(sizes))
}

#' Decluster threshold exceedances
#'
#' @description
#' Identify clusters of exceedances over `threshold` using the runs method
#' and return one summary value per cluster. The returned `value` column is
#' an approximately IID series of cluster representatives suitable as direct
#' input to [fit_gpd()] without violating its independence assumption.
#'
#' @details
#' Two consecutive exceedances belong to the same cluster when their
#' time-index gap is at most `run_length`. Each cluster is then reduced to a
#' single number via `stat`:
#'
#' - `"max"` (default): block-maximum-style representative, the standard
#'   choice for GPD declustering.
#' - `"first"` / `"last"`: preserve the timing of cluster onset / decay.
#' - `"sum"`: cluster intensity (sum of exceedances above zero).
#' - `"mean"`: average exceedance within the cluster.
#'
#' For chaotic dynamical systems extremes typically cluster (extremal index
#' \eqn{\theta < 1}), so applying `fit_gpd` directly to raw exceedances
#' underestimates the scale and biases the shape parameter. Declustering
#' first restores the IID assumption that GPD asymptotics rest on.
#'
#' @param x Numeric vector. The time series to decluster.
#' @param threshold Numeric scalar. Exceedances above this value are clustered.
#' @param run_length Integer (\eqn{\ge 1}). Maximum time gap between two
#'   exceedances assigned to the same cluster. Defaults to 1 (only strictly
#'   consecutive exceedances cluster).
#' @param stat Character. How to reduce each cluster to a single value.
#'   One of `"max"`, `"first"`, `"last"`, `"sum"`, `"mean"`.
#'
#' @return A data frame with one row per cluster and columns:
#'   \describe{
#'     \item{cluster}{Integer cluster id, starting at 1.}
#'     \item{start_index}{Time index of the first exceedance in the cluster.}
#'     \item{end_index}{Time index of the last exceedance in the cluster.}
#'     \item{n}{Number of exceedances in the cluster.}
#'     \item{value}{The cluster representative chosen by `stat`.}
#'   }
#'   When no exceedance lies above `threshold`, a zero-row data frame with
#'   these columns and the correct types is returned.
#'
#' @references
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme
#' Values*. Springer, Chapter 5.3.
#'
#' Smith, R. L., & Weissman, I. (1994). Estimating the extremal index.
#' *Journal of the Royal Statistical Society: Series B*, 56(3), 515-528.
#'
#' @seealso [fit_gpd()] for the downstream GPD fit, [cluster_sizes()] for the
#'   raw cluster size distribution, [extremal_index_runs()] for the
#'   underlying clustering statistic.
#'
#' @examples
#' x <- simulate_logistic_map(2000, r = 3.8, x0 = 0.2)
#' u <- quantile(x, 0.95)
#'
#' dec <- decluster(x, threshold = u, run_length = 2)
#' head(dec)
#'
#' # Declustered cluster maxima feed straight into fit_gpd
#' if (requireNamespace("evd", quietly = TRUE) && nrow(dec) > 5) {
#'   fit_gpd(dec$value, threshold = u)
#' }
#'
#' @export
decluster <- function(x, threshold, run_length = 1L,
                      stat = c("max", "first", "last", "sum", "mean")) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 1L)
  checkmate::assert_number(threshold, finite = TRUE)
  checkmate::assert_int(run_length, lower = 1L)
  stat <- match.arg(stat)

  empty <- data.frame(
    cluster     = integer(0),
    start_index = integer(0),
    end_index   = integer(0),
    n           = integer(0),
    value       = numeric(0)
  )

  ix <- threshold_exceedances(x, threshold)
  if (length(ix) == 0L) return(empty)

  ce <- cluster_exceedances(ix, as.integer(run_length))
  clusters <- ce$clusters

  reduce_one <- function(cluster_ix) {
    vals <- x[cluster_ix]
    switch(stat,
      max   = max(vals),
      first = vals[1L],
      last  = vals[length(vals)],
      sum   = sum(vals),
      mean  = mean(vals)
    )
  }

  data.frame(
    cluster     = seq_along(clusters),
    start_index = vapply(clusters, function(cl) cl[1L],         integer(1L)),
    end_index   = vapply(clusters, function(cl) cl[length(cl)], integer(1L)),
    n           = vapply(clusters, length,                      integer(1L)),
    value       = vapply(clusters, reduce_one,                  numeric(1L))
  )
}

#' Plot cluster size distribution
#'
#' Creates a bar chart of cluster size frequencies.
#'
#' @param sizes Integer vector of cluster sizes.
#'
#' @return A ggplot object showing the distribution.
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' sizes <- cluster_sizes(x, threshold = 0.9, run_length = 2)
#' cluster_histogram(sizes)
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @export
cluster_histogram <- function(sizes) {
  checkmate::assert_numeric(sizes, any.missing = FALSE)
  df <- as.data.frame(table(size = sizes))
  df$size <- as.integer(as.character(df$size))
  ggplot(df, aes(x = size, y = Freq)) +
    geom_col(fill = "steelblue") +
    labs(x = "Cluster size", y = "Frequency",
         title = "Cluster Size Distribution") +
    theme_minimal()
}
