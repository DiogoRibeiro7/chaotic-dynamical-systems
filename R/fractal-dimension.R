#' Estimate correlation dimension
#'
#' Estimates the Grassberger-Procaccia correlation dimension of a univariate
#' time series after delay embedding. Temporal neighbours can be excluded with
#' a Theiler window, and the log-log slope is fitted only on an explicit or
#' automatically selected scaling region.
#'
#' @param x Numeric vector of finite observations.
#' @param m Embedding dimension. Defaults to 2.
#' @param tau Time delay for embedding. Defaults to 1.
#' @param r_vals Optional positive numeric vector of radii at which to compute
#'   the correlation sum. If `NULL`, 30 log-spaced radii are chosen between
#'   the 1st and 90th percentiles of the positive admissible pair distances.
#' @param theiler Non-negative integer Theiler window. Pairs of embedded states
#'   separated by at most this many time indices are excluded.
#' @param scaling_range Optional positive numeric vector of length two giving
#'   the radius interval used for the log-log fit. If `NULL`, the function
#'   uses points with correlation sums between 0.02 and 0.5, falling back to
#'   all non-saturated points when necessary.
#' @param min_scaling_points Minimum number of radii required for the scaling
#'   fit. Must be at least 3.
#'
#' @details
#' The automatic scaling rule is a practical diagnostic heuristic, not proof
#' that a true asymptotic scaling region exists. Inspect `r`, `C`,
#' `scaling_indices`, `r_squared`, and `local_slope_sd` before interpreting
#' the returned dimension. A positive Theiler window helps reduce spurious
#' low-dimensional structure caused by serially adjacent observations.
#'
#' @return A list with the original `r`, `C`, and `dimension` components,
#'   plus `scaling_indices`, `scaling_range`, `r_squared`, `slope_se`,
#'   `local_slope_sd`, `n_pairs`, and `theiler`.
#'
#' @references
#' Grassberger, P. and Procaccia, I. (1983). Measuring the strangeness of
#' strange attractors. *Physica D*, 9, 189-208.
#' DOI: 10.1016/0167-2789(83)90298-1.
#'
#' Theiler, J. (1986). Spurious dimension from correlation algorithms applied
#' to limited time-series data. *Physical Review A*, 34, 2427-2432.
#' DOI: 10.1103/PhysRevA.34.2427.
#'
#' @examples
#' x <- simulate_logistic_map(1000, 3.8, 0.2)
#' cd <- estimate_correlation_dimension(x, theiler = 2L)
#' cd$dimension
#' cd$scaling_range
#' @export
estimate_correlation_dimension <- function(
    x,
    m = 2L,
    tau = 1L,
    r_vals = NULL,
    theiler = 0L,
    scaling_range = NULL,
    min_scaling_points = 5L
) {
  checkmate::assert_int(m, lower = 1)
  checkmate::assert_int(tau, lower = 1)
  checkmate::assert_int(theiler, lower = 0)
  checkmate::assert_int(min_scaling_points, lower = 3)
  checkmate::assert_numeric(
    x,
    min.len = (m - 1) * tau + 2,
    any.missing = FALSE,
    finite = TRUE
  )
  checkmate::assert_numeric(
    r_vals,
    lower = 0,
    any.missing = FALSE,
    finite = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_numeric(
    scaling_range,
    len = 2,
    lower = 0,
    any.missing = FALSE,
    finite = TRUE,
    null.ok = TRUE
  )

  n_embed <- length(x) - (m - 1L) * tau
  embed_mat <- vapply(
    seq_len(m),
    function(j) x[seq_len(n_embed) + (j - 1L) * tau],
    numeric(n_embed)
  )

  pair_distances <- unlist(lapply(seq_len(n_embed - 1L), function(i) {
    first_j <- i + theiler + 1L
    if (first_j > n_embed) return(numeric(0))
    js <- seq.int(first_j, n_embed)
    deltas <- sweep(
      embed_mat[js, , drop = FALSE],
      2L,
      embed_mat[i, ],
      "-"
    )
    sqrt(rowSums(deltas^2))
  }), use.names = FALSE)

  if (length(pair_distances) == 0L) {
    stop("Theiler window excludes every embedded-state pair.", call. = FALSE)
  }

  positive_distances <- pair_distances[
    is.finite(pair_distances) & pair_distances > 0
  ]
  if (length(positive_distances) < min_scaling_points) {
    stop("Not enough positive pair distances for a scaling fit.", call. = FALSE)
  }

  if (is.null(r_vals)) {
    bounds <- as.numeric(stats::quantile(
      positive_distances,
      probs = c(0.01, 0.90),
      names = FALSE,
      type = 8
    ))
    if (!(bounds[2] > bounds[1])) {
      bounds <- range(positive_distances)
    }
    if (!(bounds[2] > bounds[1])) {
      stop("Pair distances do not span a usable radius range.", call. = FALSE)
    }
    r_vals <- exp(seq(log(bounds[1]), log(bounds[2]), length.out = 30L))
  } else {
    if (any(r_vals <= 0)) {
      stop("All `r_vals` must be strictly positive.", call. = FALSE)
    }
    r_vals <- sort(unique(as.numeric(r_vals)))
  }

  corr <- vapply(
    r_vals,
    function(r) mean(pair_distances < r),
    numeric(1)
  )
  valid <- is.finite(corr) & corr > 0 & corr < 1

  if (is.null(scaling_range)) {
    scaling_indices <- which(valid & corr >= 0.02 & corr <= 0.5)
    if (length(scaling_indices) < min_scaling_points) {
      scaling_indices <- which(valid)
    }
  } else {
    scaling_range <- sort(as.numeric(scaling_range))
    if (!(scaling_range[2] > scaling_range[1])) {
      stop("`scaling_range` must have two distinct values.", call. = FALSE)
    }
    scaling_indices <- which(
      valid &
        r_vals >= scaling_range[1] &
        r_vals <= scaling_range[2]
    )
  }

  if (length(scaling_indices) < min_scaling_points) {
    stop(
      "Scaling region has fewer points than `min_scaling_points`.",
      call. = FALSE
    )
  }

  log_r <- log(r_vals[scaling_indices])
  log_c <- log(corr[scaling_indices])
  fit <- stats::lm(log_c ~ log_r)
  fit_summary <- summary(fit)
  local_slopes <- diff(log_c) / diff(log_r)

  list(
    r = r_vals,
    C = corr,
    dimension = unname(fit$coefficients[[2L]]),
    scaling_indices = scaling_indices,
    scaling_range = range(r_vals[scaling_indices]),
    r_squared = unname(fit_summary$r.squared),
    slope_se = unname(fit_summary$coefficients[2L, "Std. Error"]),
    local_slope_sd = if (length(local_slopes) > 1L) {
      stats::sd(local_slopes)
    } else {
      NA_real_
    },
    n_pairs = length(pair_distances),
    theiler = theiler
  )
}
