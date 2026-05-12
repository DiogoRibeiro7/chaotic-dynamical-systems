#' Hitting-Time and Extreme-Value Analysis for Dynamical Systems
#'
#' @description
#' Provides functions to compute hitting-time statistics (HTS) and extreme-value block maxima (EVL)
#' for one-dimensional maps. It follows best practices: roxygen2 documentation, type assertions via checkmate,
#' clear function interfaces, and vectorized operations where appropriate.
#'
#' @importFrom checkmate assertFunction assertNumber assertCount assertNumeric
#' @importFrom evd fgev
NULL

#' Simulate Orbit of a One-Dimensional Map
#'
#' @param map_fn Function taking a numeric scalar and returning a numeric scalar. The map f: [0,1] -> [0,1].
#' @param init Numeric scalar in [0,1]. Initial condition.
#' @param n_iter Integer > 0. Number of iterations to simulate.
#' @return Numeric vector of orbit values (length = n_iter).
#' @examples
#' orbit <- simulate_orbit(function(x) 4*x*(1-x), init = 0.1, n_iter = 1000)\#'
#' @export
simulate_orbit <- function(map_fn, init, n_iter) {
  checkmate::assertFunction(map_fn, args = "x")
  checkmate::assertNumber(init, lower = 0, upper = 1, finite = TRUE)
  checkmate::assertCount(n_iter)

  # Pre-allocate vector for performance
  orbit <- numeric(n_iter)
  orbit[1] <- map_fn(init)

  for (i in 2:n_iter) {
    orbit[i] <- map_fn(orbit[i - 1])
  }

  return(orbit)
}

#' Compute Hitting (Return) Times to an Epsilon-Ball
#'
#' @param orbit Numeric vector. Precomputed orbit values.
#' @param zeta Numeric scalar in [0,1]. Center of the target ball.
#' @param eps Positive numeric. Radius of the epsilon-ball.
#' @return Integer vector of indices where the orbit enters the ball.
#' @examples
#' orbit <- simulate_orbit(function(x) 4*x*(1-x), init = 0.5, n_iter = 1e5)
#' hits  <- compute_return_times(orbit, zeta = 0.5, eps = 0.005)
#' hist(hits, breaks = 50, main = "Hitting-Time Distribution")
#' @export
compute_return_times <- function(orbit, zeta, eps) {
  checkmate::assertNumeric(orbit, any.missing = FALSE)
  checkmate::assertNumber(zeta, lower = 0, upper = 1, finite = TRUE)
  checkmate::assertNumber(eps, lower = 0, finite = TRUE)

  # Logical vector marking when orbit is within eps of zeta
  hits_logical <- abs(orbit - zeta) <= eps
  # Extract indices of hits
  hit_indices <- which(hits_logical)

  return(hit_indices)
}

#' Compute Block Maxima for Extreme-Value Law Fitting
#'
#' @param orbit Numeric vector. Precomputed orbit values.
#' @param obs_fn Function taking a numeric vector and returning a numeric vector. Observable g(x).
#' @param block_size Integer > 0. Number of iterates per block.
#' @return Numeric vector of block maxima (length = floor(length(orbit)/block_size)).
#' @examples
#' orbit    <- simulate_orbit(function(x) 4*x*(1-x), init = 0.1, n_iter = 1e6)
#' extremes <- compute_block_maxima(orbit, obs_fn = function(x) -log(abs(x - 0.5)), block_size = 1000)
#' @export
compute_block_maxima <- function(orbit, obs_fn, block_size) {
  checkmate::assertNumeric(orbit, any.missing = FALSE)
  checkmate::assertFunction(obs_fn, args = "x")
  checkmate::assertCount(block_size)

  n       <- length(orbit)
  n_blocks <- floor(n / block_size)
  maxima  <- numeric(n_blocks)

  for (b in seq_len(n_blocks)) {
    start_idx <- (b - 1) * block_size + 1
    end_idx   <- b * block_size
    block_vals <- obs_fn(orbit[start_idx:end_idx])
    maxima[b]  <- max(block_vals, na.rm = TRUE)
  }

  return(maxima)
}

#' Fit Generalized Extreme Value (GEV) Distribution
#'
#' @param maxima Numeric vector. Block maxima of the observable.
#' @return Object of class 'fgev' (from evd).
#' @examples
#' maxima <- rgev(100, loc = 0, scale = 1, shape = 0.2)
#' fit    <- fit_gev(maxima)
#' summary(fit)
#' @export
fit_gev <- function(maxima) {
  checkmate::assertNumeric(maxima, any.missing = FALSE)
  # Fit the GEV model to the block maxima
  fit <- evd::fgev(maxima)
  return(fit)
}

# Example Workflow (uncomment to run) --------------------------------------
# library(checkmate)
# library(evd)
# 
# # 1. Simulate orbit for logistic map
# orbit <- simulate_orbit(
#   map_fn = function(x) 4*x*(1 - x),
#   init   = 0.1,
#   n_iter = 1e6
# )
# 
# # 2. Compute hitting times
# hits <- compute_return_times(
#   orbit = orbit,
#   zeta  = 0.5,
#   eps   = 0.005
# )
# hist(hits, breaks = 50, main = "Hitting-Time Distribution")
# 
# # 3. Compute block maxima of observable phi(x) = -log(|x - 0.5|)
# extremes <- compute_block_maxima(
#   orbit     = orbit,
#   obs_fn    = function(x) -log(abs(x - 0.5)),
#   block_size = 1000
# )
# 
# # 4. Fit GEV and inspect results
# gev_fit <- fit_gev(extremes)
# print(summary(gev_fit))
