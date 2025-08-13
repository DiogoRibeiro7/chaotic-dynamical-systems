#' Estimate correlation dimension
#'
#' Implements a simple Grassberger-Procaccia algorithm to estimate the
#' correlation dimension of a univariate time series.
#'
#' @param x Numeric vector of observations.
#' @param m Embedding dimension. Defaults to 2.
#' @param tau Time delay for embedding. Defaults to 1.
#' @param r_vals Optional numeric vector of radii at which to compute the
#'   correlation sum. If `NULL`, a range is chosen automatically.
#'
#' @return A list with elements `r`, `C`, and `dimension` containing the
#'   radii, correlation sums, and estimated correlation dimension.
#'
#' @examples
#' x <- simulate_logistic_map(1000, 3.8, 0.2)
#' cd <- estimate_correlation_dimension(x)
#' cd$dimension
#' @export
estimate_correlation_dimension <- function(x, m = 2L, tau = 1L, r_vals = NULL) {
  checkmate::assert_int(m, lower = 1)
  checkmate::assert_int(tau, lower = 1)
  checkmate::assert_numeric(x, min.len = (m - 1) * tau + 2)
  checkmate::assert_numeric(r_vals, lower = 0, null.ok = TRUE)
  n <- length(x) - (m - 1) * tau
  embed_mat <- sapply(0:(m - 1), function(k) x[(1:n) + k * tau])
  dists <- dist(embed_mat, method = "euclidean")
  if (is.null(r_vals)) {
    max_r <- max(dists)
    min_r <- max_r / 100
    r_vals <- exp(seq(log(min_r), log(max_r), length.out = 20))
  }
  corr <- vapply(r_vals, function(r) mean(dists < r), numeric(1))
  lr <- log(r_vals)
  lc <- log(corr)
  fit <- stats::lm(lc ~ lr)
  list(r = r_vals, C = corr, dimension = unname(coef(fit)[2]))
}
