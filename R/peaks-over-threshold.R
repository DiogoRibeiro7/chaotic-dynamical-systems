#' Identify exceedances above a threshold
#'
#' @param x Numeric vector of observations.
#' @param threshold Numeric threshold value.
#'
#' @return Numeric vector of exceedances (values above `threshold`). If no
#'   values exceed `threshold` an empty vector is returned.
#' @examples
#' exceedances(rnorm(100), 1.5)
#' @export
exceedances <- function(x, threshold) {
  stopifnot(is.numeric(x), is.numeric(threshold), length(threshold) == 1)
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
#' @examples
#' fit_gpd(rnorm(1000), 1.5)
#' @export
fit_gpd <- function(x, threshold) {
  stopifnot(is.numeric(x), is.numeric(threshold), length(threshold) == 1)
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
  stopifnot(is.numeric(x), is.numeric(thresholds))
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
