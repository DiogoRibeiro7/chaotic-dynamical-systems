#' Compute block maxima
#'
#' Split a univariate time series into non-overlapping blocks of size
#' `block_size` and compute the maximum within each block.
#'
#' @param x Numeric vector of observations.
#' @param block_size Integer. Length of each block.
#'
#' @return Numeric vector of block maxima.
#' @examples
#' block_maxima(rnorm(1000), 50)
#' @export
block_maxima <- function(x, block_size) {
  stopifnot(is.numeric(x),
            length(x) >= block_size,
            is.numeric(block_size),
            block_size > 0)
  n_blocks <- floor(length(x) / block_size)
  idx <- seq_len(n_blocks * block_size)
  mat <- matrix(x[idx], nrow = block_size, ncol = n_blocks)
  apply(mat, 2L, max)
}

#' Fit a GEV distribution to block maxima
#'
#' Uses `evd::fgev` if available; otherwise falls back to
#' `ismev::gev.fit`.
#'
#' @param block_maxima Numeric vector of maxima, typically output
#'   from [block_maxima()].
#'
#' @return Fitted model object.
#' @examples
#' m <- block_maxima(rnorm(1000), 50)
#' fit <- fit_gev(m)
#' @export
fit_gev <- function(block_maxima) {
  stopifnot(is.numeric(block_maxima), length(block_maxima) > 1)
  if (requireNamespace("evd", quietly = TRUE)) {
    evd::fgev(block_maxima)
  } else if (requireNamespace("ismev", quietly = TRUE)) {
    ismev::gev.fit(block_maxima, show = FALSE)
  } else {
    stop("Package 'evd' or 'ismev' required for GEV fitting")
  }
}
