#' Multivariate Extreme Value Utilities
#'
#' Collection of functions for multivariate extremal analysis and
#' tail dependence diagnostics.
#'
#' @name multivariate_extremes
NULL

#' Multivariate extremal index
#'
#' Estimates a multivariate extremal index for a dataset with two or
#' more variables. The estimator uses a runs approach applied to joint
#' exceedances across any component and averages it with the component
#' wise runs estimators.
#'
#' @param df [data.frame] or [matrix] with numeric columns.
#' @param thresholds [numeric] Vector of length equal to `ncol(df)` or a
#'   single threshold applied to all columns.
#' @param run_length [integer] Run parameter for the runs estimator.
#'
#' @return [numeric] Estimated extremal index between 0 and 1, or `NA`
#'   if no valid estimates are available.
#' @examples
#' set.seed(1)
#' df <- data.frame(a = rnorm(1000), b = rnorm(1000), c = rnorm(1000))
#' extremal_index_multivariate(df, 0.9)
#' @export
extremal_index_multivariate <- function(df, thresholds, run_length = 3L) {
  df <- as.data.frame(df)
  checkmate::assert_data_frame(df, types = "numeric", min.cols = 2)
  p <- ncol(df)
  if (length(thresholds) == 1) thresholds <- rep(thresholds, p)
  checkmate::assert_numeric(thresholds, any.missing = FALSE, len = p)
  checkmate::assert_count(run_length)

  exceed_mat <- mapply(function(col, thr) col > thr & !is.na(col), df, thresholds)
  exceed_any <- apply(exceed_mat, 1, any)
  indices <- which(exceed_any)
  ce <- cluster_exceedances(indices, run_length)
  n_exc <- length(indices)
  theta_joint <- if (n_exc == 0) NA_real_ else ce$n_clusters / n_exc
  thetas <- vapply(seq_len(p), function(j) {
    extremal_index_runs(df[[j]], thresholds[j], run_length)
  }, numeric(1))
  vals <- c(theta_joint, thetas)
  if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
}

#' Bivariate wrapper for backward compatibility
#'
#' Calls [extremal_index_multivariate()] for the first two columns of `df`.
#'
#' @inheritParams extremal_index_multivariate
#'
#' @return [numeric] Estimated extremal index for the first two columns or `NA`.
#' @examples
#' set.seed(1)
#' df <- data.frame(a = rnorm(1000), b = rnorm(1000))
#' extremal_index_bivariate(df, 0.9)
#' @seealso [extremal_index_multivariate()]
#' @export
extremal_index_bivariate <- function(df, thresholds, run_length = 3L) {
  df <- as.data.frame(df)
  checkmate::assert_data_frame(df, types = "numeric", min.cols = 2)
  if (length(thresholds) == 1) thresholds <- rep(thresholds, 2)
  checkmate::assert_numeric(thresholds, any.missing = FALSE, len = 2)
  checkmate::assert_count(run_length)
  extremal_index_multivariate(df[, 1:2], thresholds, run_length)
}

#' Asymmetric tail dependence coefficient
#'
#' Computes a tail dependence coefficient allowing separate thresholds
#' for the two variables and supporting upper or lower tail analysis.
#'
#' @param x [numeric] Vector of observations.
#' @param y [numeric] Vector of the same length as `x`.
#' @param ux [numeric] Threshold for `x`.
#' @param uy [numeric] Threshold for `y`.
#' @param lower [logical] If `TRUE` compute lower tail dependence.
#'
#' @return [numeric] Tail dependence coefficient in [0,1] or `NA` if the denominator is zero.
#' @examples
#' x <- rnorm(1000)
#' y <- 0.5 * x + rnorm(1000, sd = 0.5)
#' tx <- quantile(x, 0.95)
#' ty <- quantile(y, 0.9)
#' tail_dependence_asymmetric(x, y, tx, ty)
#' @export
tail_dependence_asymmetric <- function(x, y, ux, uy, lower = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(y, len = length(x))
  checkmate::assert_number(ux, finite = TRUE)
  checkmate::assert_number(uy, finite = TRUE)
  checkmate::assert_flag(lower)
  cc <- stats::complete.cases(x, y)
  x <- x[cc]
  y <- y[cc]
  if (lower) {
    num <- mean(x <= ux & y <= uy)
    den <- min(mean(x <= ux), mean(y <= uy))
  } else {
    num <- mean(x > ux & y > uy)
    den <- min(mean(x > ux), mean(y > uy))
  }
  if (den == 0) NA_real_ else num / den
}

#' Upper tail dependence
#'
#' Convenience wrapper for [tail_dependence_asymmetric()] computing
#' upper tail dependence with possibly different quantile levels.
#'
#' @inheritParams tail_dependence_asymmetric
#'
#' @return [numeric] Upper tail dependence coefficient.
#' @export
upper_tail_dependence <- function(x, y, ux, uy) {
  tail_dependence_asymmetric(x, y, ux, uy, lower = FALSE)
}

#' Lower tail dependence
#'
#' Convenience wrapper for [tail_dependence_asymmetric()] computing
#' lower tail dependence with possibly different quantile levels.
#'
#' @inheritParams tail_dependence_asymmetric
#'
#' @return [numeric] Lower tail dependence coefficient.
#' @export
lower_tail_dependence <- function(x, y, ux, uy) {
  tail_dependence_asymmetric(x, y, ux, uy, lower = TRUE)
}

#' Plot bivariate exceedance clusters
#'
#' Visualizes exceedance clusters for two variables, colouring points
#' by cluster membership.
#'
#' @inheritParams extremal_index_multivariate
#'
#' @return [ggplot2::ggplot] Scatter plot with clusters coloured.
#' @examples
#' set.seed(1)
#' df <- data.frame(a = rnorm(100), b = rnorm(100))
#' plot_exceedance_clusters(df, 0.9)
#' @importFrom ggplot2 ggplot geom_point geom_hline geom_vline aes labs
#'   guides guide_legend theme_minimal
#' @export
plot_exceedance_clusters <- function(df, thresholds, run_length = 3L) {
  df <- as.data.frame(df)
  checkmate::assert_data_frame(df, types = "numeric", min.cols = 2)
  if (length(thresholds) == 1) thresholds <- rep(thresholds, 2)
  checkmate::assert_numeric(thresholds, any.missing = FALSE, len = 2)
  checkmate::assert_count(run_length)
  exc_any <- which(df[[1]] > thresholds[1] | df[[2]] > thresholds[2])
  ce <- cluster_exceedances(exc_any, run_length)
  cluster_id <- rep(NA_integer_, nrow(df))
  for (i in seq_along(ce$clusters)) cluster_id[ce$clusters[[i]]] <- i
  ggplot2::ggplot(df, ggplot2::aes(x = df[[1]], y = df[[2]])) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_point(data = df[exc_any, ],
               ggplot2::aes(color = factor(cluster_id[exc_any]))) +
    ggplot2::geom_vline(xintercept = thresholds[1], linetype = "dashed") +
    ggplot2::geom_hline(yintercept = thresholds[2], linetype = "dashed") +
    ggplot2::labs(x = names(df)[1], y = names(df)[2], color = "Cluster",
         title = "Bivariate Exceedance Clusters") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::theme_minimal()
}

#' Tail dependence heatmap
#'
#' Computes pairwise upper tail dependence coefficients and displays
#' them in a heatmap.
#'
#' @param df [data.frame] or [matrix] of numeric columns.
#' @param quantile_level [numeric] High quantile level for the tail dependence
#'   coefficient.
#'
#' @return [ggplot2::ggplot] Heatmap of pairwise tail dependence coefficients.
#' @examples
#' set.seed(1)
#' df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
#' tail_dependence_heatmap(df)
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs
#'   theme_minimal
#' @export
tail_dependence_heatmap <- function(df, quantile_level = 0.9) {
  df <- as.data.frame(df)
  checkmate::assert_data_frame(df, types = "numeric", min.cols = 2)
  checkmate::assert_number(quantile_level, lower = 0, upper = 1)
  p <- ncol(df)
  combs <- utils::combn(p, 2)
  vals <- apply(combs, 2, function(idx) {
    tail_dependence_coefficient(df[[idx[1]]], df[[idx[2]]], quantile_level)
  })
  res <- data.frame(
    Var1 = names(df)[combs[1, ]],
    Var2 = names(df)[combs[2, ]],
    lambda = vals
  )
  ggplot2::ggplot(res, ggplot2::aes(x = Var1, y = Var2, fill = lambda)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(title = "Tail Dependence Heatmap", x = NULL, y = NULL,
         fill = "Lambda") +
    ggplot2::theme_minimal()
}
