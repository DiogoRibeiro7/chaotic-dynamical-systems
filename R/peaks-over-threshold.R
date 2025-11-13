#' Identify exceedances above a threshold
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold value.
#'
#' @return Numeric vector of exceedances (values above `threshold`). If no
#'   values exceed `threshold` an empty vector is returned.
#' @seealso [fit_gpd()] for fitting GPD to exceedances,
#'   [threshold_exceedances()] for indices of exceedances,
#'   [block_maxima()] for alternative block maxima approach
#' @examples
#' exceedances(rnorm(100), 1.5)
#' @export
exceedances <- function(x, threshold) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_number(threshold)
  x[x > threshold]
}

#' Fit a Generalized Pareto Distribution (GPD)
#'
#' Attempts to fit a GPD to the exceedances above `threshold`.
#' Uses `evd::fpot` if available, otherwise `evir::gpd` or
#' `ismev::gpd.fit` as fallbacks.
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold defining exceedances.
#'
#' @return Fitted model object. Stops with an error if none of the supporting
#'   GPD-fitting packages (`evd`, `evir`, `ismev`) are installed.
#' @references
#' Pickands, J. (1975). Statistical inference using extreme order statistics.
#' The Annals of Statistics, 3(1), 119-131.
#'
#' Davison, A. C., and Smith, R. L. (1990). Models for exceedances over high thresholds.
#' Journal of the Royal Statistical Society: Series B (Methodological), 52(3), 393-425.
#' @seealso [exceedances()] for extracting exceedances,
#'   [threshold_diagnostics()] for threshold selection,
#'   [fit_gev()] for alternative block maxima approach
#' @examples
#' fit_gpd(rnorm(1000), 1.5)
#' @export
fit_gpd <- function(x, threshold) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_number(threshold)
  if (requireNamespace("evd", quietly = TRUE)) {
    evd::fpot(x, threshold)
  } else if (requireNamespace("evir", quietly = TRUE)) {
    evir::gpd(x, threshold)
  } else if (requireNamespace("ismev", quietly = TRUE)) {
    ismev::gpd.fit(x, threshold, show = FALSE)
  } else {
    stop("One of 'evd', 'evir' or 'ismev' packages is required")
  }
}

#' Mean Residual Life (MRL) values
#'
#' Computes the average excess above a sequence of thresholds.
#'
#' @param x Numeric vector of observations.
#' @param thresholds Numeric vector of thresholds to evaluate.
#'
#' @return Data frame with columns `threshold` and `mean_excess`. Thresholds with
#'   no exceedances produce `NA` in the `mean_excess` column.
#' @examples
#' mrl <- mean_residual_life(rnorm(1000), seq(0, 2, 0.2))
#' @export
mean_residual_life <- function(x, thresholds) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_numeric(thresholds, any.missing = FALSE)
  mean_excess <- sapply(thresholds, function(u) {
    exc <- x[x > u] - u
    if (length(exc) == 0) return(NA_real_)
    mean(exc)
  })
  data.frame(threshold = thresholds, mean_excess = mean_excess)
}

#' Plot Mean Residual Life (MRL)
#'
#' @param mrl_df Data frame as returned by [mean_residual_life()].
#'
#' @return ggplot object visualizing the MRL curve. Requires the **ggplot2**
#'   package.
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal
#' @examples
#' df <- mean_residual_life(rnorm(1000), seq(0, 2, 0.2))
#' mrl_plot(df)
#' @export
mrl_plot <- function(mrl_df) {
  checkmate::assert_data_frame(mrl_df)
  checkmate::assert_subset(c("threshold", "mean_excess"), names(mrl_df))
  ggplot(mrl_df, aes(x = threshold, y = mean_excess)) +
    geom_point() +
    geom_line() +
    labs(x = "Threshold", y = "Mean Excess",
         title = "Mean Residual Life Plot") +
    theme_minimal()
}
