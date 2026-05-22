# ---------------------------------------------------------------------------
# r-largest order statistics method for GEV.
#
# Block maxima throws away information by keeping only the per-block maximum.
# The r-largest extension keeps the top r values per block and fits GEV via
# the joint likelihood derived from the limiting Poisson process (Coles 2001
# §3.5). This is useful when blocks are short and the single-maximum
# estimator is high-variance.
# ---------------------------------------------------------------------------

#' Extract the r largest order statistics in each block
#'
#' Splits `x` into `floor(length(x) / block_size)` non-overlapping blocks
#' and returns the top `r` values from each, sorted in decreasing order.
#'
#' @param x Numeric vector. The time series.
#' @param block_size Integer. Length of each block; the last partial block
#'   is discarded.
#' @param r Integer (\eqn{\ge 1}). Number of top values to retain per block.
#'   Must be \eqn{\le} `block_size`.
#'
#' @return A numeric matrix with `floor(length(x) / block_size)` rows and
#'   `r` columns. Each row holds the r largest values in that block,
#'   sorted in decreasing order so that column 1 is the block maximum.
#'
#' @examples
#' x <- simulate_logistic_map(2000, r = 3.8, x0 = 0.2)
#' rl <- block_r_largest(x, block_size = 50, r = 3)
#' dim(rl)
#'
#' @seealso [block_maxima()] for the classical r = 1 case,
#'   [fit_gev_rlargest()] to fit the joint likelihood.
#'
#' @export
block_r_largest <- function(x, block_size, r) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 1L)
  checkmate::assert_int(block_size, lower = 1L)
  checkmate::assert_int(r, lower = 1L, upper = block_size)

  n_blocks <- length(x) %/% block_size
  if (n_blocks == 0L) {
    stop("block_size larger than length(x); no full block available")
  }

  rl <- matrix(0, nrow = n_blocks, ncol = r)
  for (b in seq_len(n_blocks)) {
    block <- x[((b - 1L) * block_size + 1L):(b * block_size)]
    rl[b, ] <- sort(block, decreasing = TRUE)[seq_len(r)]
  }
  rl
}

# Joint log-likelihood for the r-largest method (Coles 2001 eq. 3.27).
# rl is a matrix with rows = blocks, columns = top-r values (sorted decreasing).
.gev_rlargest_loglik <- function(par, rl) {
  mu    <- par[1L]
  sigma <- par[2L]
  xi    <- par[3L]
  if (!is.finite(sigma) || sigma <= 0) return(-Inf)
  n_b <- nrow(rl)
  r   <- ncol(rl)

  z <- (rl - mu) / sigma
  if (abs(xi) < 1e-8) {
    # Gumbel limit: -n_b*r*log(sigma) - sum(z) - sum(exp(-z_{i,r}))
    -n_b * r * log(sigma) - sum(z) - sum(exp(-z[, r]))
  } else {
    s_all <- 1 + xi * z
    if (any(!is.finite(s_all)) || any(s_all <= 0)) return(-Inf)
    -n_b * r * log(sigma) -
      (1 + 1 / xi) * sum(log(s_all)) -
      sum(s_all[, r]^(-1 / xi))
  }
}

#' Fit GEV via the r-largest order statistics method
#'
#' @description
#' Fits the GEV location / scale / shape parameters using the joint
#' likelihood for the top `r` values in each block (Coles 2001 §3.5).
#' Equivalent to [fit_gev()] when `ncol(rl) == 1`, and lower-variance
#' when `r > 1` because each block contributes `r` informative values
#' instead of just one.
#'
#' @details
#' Maximum-likelihood fit by [stats::optim()] with BFGS, starting from a
#' standard GEV fit on the block maxima (the leftmost column of `rl`).
#' Standard errors are computed from the observed information matrix
#' via numerical Hessian inversion. The returned object inherits from
#' `chaotic_model` so the existing [print()], [summary()], [tidy()],
#' [glance()], [augment()], and [profile_likelihood()] machinery all
#' work.
#'
#' @param rl Numeric matrix from [block_r_largest()].
#'
#' @return A `chaotic_model` with `model = "gev_rlargest"`, with elements
#'   `estimate`, `std.err`, `loglik`, `data` (the input matrix), and
#'   `r` (the number of columns).
#'
#' @references
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme
#' Values*. Springer, §3.5.
#'
#' Smith, R. L. (1986). Extreme value theory based on the r largest annual
#' events. *Journal of Hydrology*, 86(1-2), 27-43.
#'
#' @seealso [block_r_largest()], [fit_gev()] for the classical r = 1 fit.
#'
#' @examples
#' x  <- simulate_logistic_map(2000, r = 3.8, x0 = 0.2)
#' rl <- block_r_largest(x, block_size = 50, r = 3)
#' fit_gev_rlargest(rl)
#'
#' @export
fit_gev_rlargest <- function(rl) {
  checkmate::assert_matrix(rl, mode = "numeric", any.missing = FALSE,
                           min.rows = 2L, min.cols = 1L)
  # Verify rows are sorted in decreasing order.
  if (any(t(apply(rl, 1L, function(row) diff(row) > 0)))) {
    stop("Each row of rl must be sorted in decreasing order; ",
         "use block_r_largest() to produce a valid input.")
  }

  # Starting values from a vanilla GEV fit on the column-1 block maxima.
  bm <- rl[, 1L]
  start <- tryCatch({
    fit0 <- fit_gev(bm)
    p0   <- .extract_fit_params(fit0)
    c(p0[["mu"]], p0[["sigma"]], p0[["xi"]])
  }, error = function(e) {
    c(mean(bm), stats::sd(bm), 0.1)
  })

  objective <- function(par) -.gev_rlargest_loglik(par, rl)

  out <- stats::optim(start, objective, method = "BFGS",
                      hessian = TRUE,
                      control = list(reltol = 1e-8, maxit = 1000L))

  if (out$convergence != 0L) {
    warning("optim() reported non-zero convergence code (",
            out$convergence, ") in fit_gev_rlargest()")
  }

  estimate <- out$par
  names(estimate) <- c("loc", "scale", "shape")
  se <- tryCatch(sqrt(diag(solve(out$hessian))),
                 error = function(e) rep(NA_real_, 3L))
  names(se) <- names(estimate)

  fit <- list(
    estimate = estimate,
    std.err  = se,
    loglik   = -out$value,
    deviance = 2 * out$value,
    data     = rl,
    r        = ncol(rl),
    nobs     = nrow(rl) * ncol(rl),
    convergence = out$convergence
  )
  wrap_chaotic_model(fit, model = "gev_rlargest", method = "chaoticds::fit_gev_rlargest")
}
