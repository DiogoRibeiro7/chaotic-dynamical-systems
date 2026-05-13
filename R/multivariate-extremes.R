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
  checkmate::assert_data_frame(
    df,
    types = "numeric",
    min.cols = 2,
    .var.name = "df"
  )
  p <- ncol(df)
  if (length(thresholds) == 1) thresholds <- rep(thresholds, p)
  checkmate::assert_numeric(
    thresholds,
    any.missing = FALSE,
    len = p,
    .var.name = "thresholds"
  )
  checkmate::assert_count(
    run_length,
    positive = TRUE,
    .var.name = "run_length"
  )

  exceed_mat <- mapply(function(col, thr) col > thr & !is.na(col), df, thresholds)
  exceed_any <- apply(exceed_mat, 1, any)
  indices <- which(exceed_any)
  ce <- cluster_exceedances(indices, run_length)
  n_exc <- length(indices)
  n_clusters <- if (!is.null(ce$n_clusters)) {
    as.numeric(ce$n_clusters)
  } else if (!is.null(ce$clusters)) {
    as.numeric(length(ce$clusters))
  } else {
    as.numeric(length(ce))
  }
  theta_joint <- if (n_exc == 0) NA_real_ else n_clusters / n_exc
  sanitize_theta <- function(v) {
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v)) {
      return(NA_real_)
    }
    min(1, max(0, as.numeric(v)))
  }
  thetas <- vapply(seq_len(p), function(j) {
    sanitize_theta(tryCatch(
      extremal_index_runs(df[[j]], thresholds[j], run_length),
      error = function(e) NA_real_
    ))
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
  checkmate::assert_data_frame(
    df,
    types = "numeric",
    min.cols = 2,
    .var.name = "df"
  )
  if (length(thresholds) == 1) thresholds <- rep(thresholds, 2)
  if (length(thresholds) > 2) thresholds <- thresholds[1:2]
  checkmate::assert_numeric(
    thresholds,
    any.missing = FALSE,
    len = 2,
    .var.name = "thresholds"
  )
  checkmate::assert_count(
    run_length,
    positive = TRUE,
    .var.name = "run_length"
  )
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
  checkmate::assert_numeric(x, .var.name = "x")
  checkmate::assert_numeric(y, len = length(x), .var.name = "y")
  checkmate::assert_number(ux, finite = TRUE, .var.name = "ux")
  checkmate::assert_number(uy, finite = TRUE, .var.name = "uy")
  checkmate::assert_flag(lower, .var.name = "lower")
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
  checkmate::assert_data_frame(
    df,
    types = "numeric",
    min.cols = 2,
    .var.name = "df"
  )
  if (length(thresholds) == 1) thresholds <- rep(thresholds, 2)
  checkmate::assert_numeric(
    thresholds,
    any.missing = FALSE,
    len = 2,
    .var.name = "thresholds"
  )
  checkmate::assert_count(
    run_length,
    positive = TRUE,
    .var.name = "run_length"
  )
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
  checkmate::assert_data_frame(
    df,
    types = "numeric",
    min.cols = 2,
    .var.name = "df"
  )
  checkmate::assert_number(
    quantile_level,
    lower = 0,
    upper = 1,
    .var.name = "quantile_level"
  )
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

#' End-to-end multivariate extremes workflow
#'
#' Computes a practical summary for multivariate extremes combining
#' thresholding, pairwise tail dependence, joint exceedance diagnostics,
#' and a multivariate extremal index estimate.
#'
#' @details
#' ## Assumptions
#' - Input columns are numeric observables from the same time index.
#' - The series are approximately stationary in the tail region.
#' - Thresholds are high enough for POT-style asymptotics to be informative.
#'
#' ## Notes
#' This workflow is intended as a diagnostic entrypoint rather than a
#' replacement for full model validation. Always inspect sensitivity to
#' threshold choices and run lengths.
#'
#' @param df [data.frame] or [matrix] with numeric columns.
#' @param quantile_level [numeric] Quantile level in (0,1) used to derive
#'   per-variable thresholds.
#' @param run_length [integer] Run parameter for extremal-index clustering.
#' @param include_lower_tail [logical] Whether to also compute lower-tail
#'   dependence coefficients.
#'
#' @return A list with components:
#' \describe{
#'   \item{thresholds}{Named numeric vector of per-variable thresholds.}
#'   \item{pairwise_dependence}{Data frame with pairwise upper/lower tail dependence.}
#'   \item{joint_exceedance_rate}{Proportion of rows with at least one exceedance.}
#'   \item{all_exceedance_rate}{Proportion of rows exceeding all thresholds.}
#'   \item{multivariate_extremal_index}{Estimated extremal index from
#'     [extremal_index_multivariate()].}
#'   \item{settings}{List of workflow settings for reproducibility.}
#' }
#' @examples
#' set.seed(1)
#' x <- rnorm(1000)
#' y <- 0.6 * x + rnorm(1000, sd = 0.8)
#' z <- -0.2 * x + rnorm(1000, sd = 1.0)
#' df <- data.frame(x = x, y = y, z = z)
#'
#' wf <- multivariate_extreme_workflow(df, quantile_level = 0.95, run_length = 3)
#' wf$multivariate_extremal_index
#' head(wf$pairwise_dependence)
#' @export
multivariate_extreme_workflow <- function(
    df,
    quantile_level = 0.95,
    run_length = 3L,
    include_lower_tail = TRUE
) {
  df <- as.data.frame(df)
  checkmate::assert_data_frame(
    df,
    types = "numeric",
    min.cols = 2,
    .var.name = "df"
  )
  checkmate::assert_number(
    quantile_level,
    lower = 0,
    upper = 1,
    .var.name = "quantile_level"
  )
  checkmate::assert_count(
    run_length,
    positive = TRUE,
    .var.name = "run_length"
  )
  checkmate::assert_flag(include_lower_tail, .var.name = "include_lower_tail")

  p <- ncol(df)
  nm <- names(df)
  thresholds <- stats::quantile(df[[1]], probs = quantile_level, na.rm = TRUE, type = 8)
  thresholds <- vapply(df, function(col) {
    stats::quantile(col, probs = quantile_level, na.rm = TRUE, type = 8)
  }, numeric(1))
  names(thresholds) <- nm

  lower_thresholds <- vapply(df, function(col) {
    stats::quantile(col, probs = 1 - quantile_level, na.rm = TRUE, type = 8)
  }, numeric(1))
  names(lower_thresholds) <- nm

  combs <- utils::combn(p, 2)
  pairwise <- apply(combs, 2, function(idx) {
    i <- idx[1]
    j <- idx[2]
    up <- upper_tail_dependence(
      df[[i]], df[[j]],
      ux = thresholds[i], uy = thresholds[j]
    )
    low <- if (include_lower_tail) {
      lower_tail_dependence(
        df[[i]], df[[j]],
        ux = lower_thresholds[i], uy = lower_thresholds[j]
      )
    } else {
      NA_real_
    }
    c(i = i, j = j, upper = up, lower = low)
  })
  pairwise <- t(pairwise)
  pairwise_dependence <- data.frame(
    var1 = nm[pairwise[, "i"]],
    var2 = nm[pairwise[, "j"]],
    upper_tail = as.numeric(pairwise[, "upper"]),
    lower_tail = as.numeric(pairwise[, "lower"])
  )

  exceed_mat <- mapply(function(col, thr) col > thr & !is.na(col), df, thresholds)
  joint_exceedance_rate <- mean(apply(exceed_mat, 1, any))
  all_exceedance_rate <- mean(apply(exceed_mat, 1, all))

  mult_theta <- extremal_index_multivariate(
    df = df,
    thresholds = thresholds,
    run_length = run_length
  )

  list(
    thresholds = thresholds,
    pairwise_dependence = pairwise_dependence,
    joint_exceedance_rate = joint_exceedance_rate,
    all_exceedance_rate = all_exceedance_rate,
    multivariate_extremal_index = mult_theta,
    settings = list(
      quantile_level = quantile_level,
      run_length = run_length,
      include_lower_tail = include_lower_tail
    )
  )
}
