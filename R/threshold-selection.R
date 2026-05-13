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
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_numeric(thresholds, any.missing = FALSE)
  checkmate::assert_integerish(k_values, any.missing = FALSE)
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
#' @references
#' Hill, B. M. (1975). A simple general approach to inference about the tail of a distribution.
#' The Annals of Statistics, 3(5), 1163-1174.
#' @examples
#' hill_estimates(rexp(1000), 1:50)
#' @export
hill_estimates <- function(x, k_values) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_integerish(k_values, any.missing = FALSE)
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
  checkmate::assert_data_frame(hill_df)
  checkmate::assert_subset(c("k", "hill"), names(hill_df))
  ggplot(hill_df, aes(x = k, y = hill)) +
    geom_line() +
    geom_point() +
    labs(x = "Order statistic k", y = "Hill estimate",
         title = "Hill Plot") +
    theme_minimal()
}

#' Automatic threshold selection for POT analysis
#'
#' Ranks candidate thresholds using a composite score that combines:
#' (1) MRL smoothness, (2) extremal-index stability, and
#' (3) exceedance adequacy.
#'
#' @param x Numeric vector of observations.
#' @param candidate_probs Numeric vector of quantile probabilities in (0, 1).
#' @param min_exceedances Integer minimum desirable number of exceedances.
#' @param estimator Character string, either `"runs"` or `"intervals"`.
#' @param run_length Integer run parameter for the runs estimator.
#' @param weights Named numeric vector with elements `mrl`, `stability`,
#'   and `exceedance` controlling score aggregation.
#'
#' @return A list with:
#' \describe{
#'   \item{recommended_threshold}{Selected threshold value.}
#'   \item{recommended_probability}{Selected quantile probability.}
#'   \item{score}{Composite score of the selected threshold.}
#'   \item{ranking}{Data frame of all candidates sorted by score.}
#' }
#' @examples
#' set.seed(42)
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' auto <- select_threshold_auto(x)
#' auto$recommended_threshold
#' head(auto$ranking)
#' @export
select_threshold_auto <- function(
    x,
    candidate_probs = seq(0.90, 0.99, by = 0.01),
    min_exceedances = 20L,
    estimator = c("runs", "intervals"),
    run_length = 3L,
    weights = c(mrl = 0.4, stability = 0.4, exceedance = 0.2)
) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 50)
  checkmate::assert_numeric(candidate_probs, any.missing = FALSE)
  checkmate::assert_true(all(candidate_probs > 0 & candidate_probs < 1))
  checkmate::assert_int(min_exceedances, lower = 1)
  estimator <- match.arg(estimator)
  checkmate::assert_int(run_length, lower = 1)
  checkmate::assert_numeric(weights, len = 3, any.missing = FALSE, lower = 0)
  if (is.null(names(weights)) ||
      !all(c("mrl", "stability", "exceedance") %in% names(weights))) {
    stop("weights must be a named numeric vector with names: mrl, stability, exceedance")
  }

  weights <- weights[c("mrl", "stability", "exceedance")]
  if (sum(weights) <= 0) {
    stop("weights must sum to a positive value")
  }
  weights <- weights / sum(weights)

  candidate_probs <- sort(unique(candidate_probs))
  thresholds <- as.numeric(stats::quantile(x, probs = candidate_probs, names = FALSE, type = 8))
  n <- length(x)
  exceed_counts <- vapply(thresholds, function(u) sum(x > u), integer(1))

  mrl_df <- mean_residual_life(x, thresholds)
  mrl <- mrl_df$mean_excess

  # MRL smoothness score: smaller local curvature is preferred.
  mrl_curvature <- rep(NA_real_, length(mrl))
  if (length(mrl) >= 3) {
    for (i in 2:(length(mrl) - 1)) {
      if (all(is.finite(c(mrl[i - 1], mrl[i], mrl[i + 1])))) {
        mrl_curvature[i] <- abs(mrl[i + 1] - 2 * mrl[i] + mrl[i - 1])
      }
    }
  }
  if (length(mrl) >= 2) {
    mrl_curvature[1] <- mrl_curvature[2]
    mrl_curvature[length(mrl)] <- mrl_curvature[length(mrl) - 1]
  }

  score_from_inverse <- function(v) {
    out <- rep(NA_real_, length(v))
    ok <- is.finite(v)
    if (!any(ok)) return(out)
    vv <- v[ok]
    vmin <- min(vv)
    vmax <- max(vv)
    if (vmax == vmin) {
      out[ok] <- 1
    } else {
      out[ok] <- 1 - (vv - vmin) / (vmax - vmin)
    }
    out
  }
  mrl_score <- score_from_inverse(mrl_curvature)

  est_fun <- if (estimator == "runs") {
    function(u) extremal_index_runs(x, u, run_length = run_length)
  } else {
    function(u) extremal_index_intervals(x, u)
  }
  sanitize_theta <- function(v) {
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v)) {
      return(NA_real_)
    }
    min(1, max(0, as.numeric(v)))
  }
  theta <- vapply(thresholds, function(u) {
    sanitize_theta(tryCatch(est_fun(u), error = function(e) NA_real_))
  }, numeric(1))

  # Stability score: lower local variability in theta is better.
  theta_var <- rep(NA_real_, length(theta))
  if (length(theta) >= 3) {
    for (i in 2:(length(theta) - 1)) {
      w <- theta[(i - 1):(i + 1)]
      if (all(is.finite(w))) {
        theta_var[i] <- stats::sd(w)
      }
    }
  }
  if (length(theta) >= 2) {
    theta_var[1] <- theta_var[2]
    theta_var[length(theta)] <- theta_var[length(theta) - 1]
  }
  stability_score <- score_from_inverse(theta_var)

  exceedance_score <- pmin(1, exceed_counts / min_exceedances)

  comp <- cbind(mrl = mrl_score, stability = stability_score, exceedance = exceedance_score)
  score <- rep(NA_real_, nrow(comp))
  for (i in seq_len(nrow(comp))) {
    ok <- is.finite(comp[i, ])
    if (any(ok)) {
      w <- weights[ok]
      score[i] <- sum(comp[i, ok] * w) / sum(w)
    }
  }

  rationale <- ifelse(
    exceed_counts < min_exceedances,
    "Too few exceedances",
    ifelse(
      !is.finite(stability_score),
      "Insufficient stability information",
      ifelse(stability_score >= 0.66, "Stable extremal-index region",
             "Candidate with moderate stability")
    )
  )

  ranking <- data.frame(
    probability = candidate_probs,
    threshold = thresholds,
    n_exceedances = exceed_counts,
    mrl_score = mrl_score,
    stability_score = stability_score,
    exceedance_score = exceedance_score,
    score = score,
    rationale = rationale
  )
  ranking <- ranking[order(ranking$score, decreasing = TRUE, na.last = TRUE), ]
  rownames(ranking) <- NULL

  if (!any(is.finite(ranking$score))) {
    stop("Unable to score candidate thresholds; check data and candidate_probs")
  }

  best <- ranking[which.max(ranking$score), ]
  list(
    recommended_threshold = best$threshold,
    recommended_probability = best$probability,
    score = best$score,
    ranking = ranking
  )
}
