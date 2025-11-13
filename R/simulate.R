#' Simulate Logistic Map Dynamics
#'
#' @description
#' Generates a time series from the logistic map, a classic one-dimensional
#' chaotic dynamical system. The logistic map exhibits complex behavior
#' including fixed points, periodic orbits, and deterministic chaos depending
#' on the parameter r.
#'
#' @details
#' ## Overview
#' The logistic map is defined by the iteration:
#' \deqn{x_{n+1} = r \cdot x_n \cdot (1 - x_n)}
#'
#' Despite its simplicity, this map demonstrates the route to chaos through
#' period-doubling bifurcations and is fundamental to understanding chaotic
#' dynamics.
#'
#' ## Parameter Regions
#' The parameter r controls the system's behavior:
#' - **r < 1**: Extinction (x → 0)
#' - **1 < r < 3**: Convergence to fixed point
#' - **3 < r < 1 + √6 ≈ 3.45**: Oscillation between two values
#' - **3.45 < r < 3.57**: Period-doubling cascade
#' - **r > 3.57**: Chaotic regime (with periodic windows)
#' - **r = 4**: Fully chaotic (every orbit is dense)
#'
#' For studying extreme value theory, r = 3.8 or r = 4.0 are common choices
#' as they produce robust chaotic dynamics.
#'
#' @section Mathematical Background:
#' The logistic map was introduced by Robert May (1976) as a model for
#' population dynamics. The equation represents population growth with
#' reproduction (r·x) and competition/limiting resources (1-x).
#'
#' For r > 3.57, the system exhibits sensitive dependence on initial
#' conditions, a hallmark of chaos. The largest Lyapunov exponent is
#' positive in the chaotic regime, confirming exponential divergence of
#' nearby trajectories.
#'
#' @param n Integer. Number of iterations to simulate. Must be positive.
#'   Typical values range from 500 (quick exploration) to 10000 (statistical
#'   analysis). Larger n provides better estimates of long-term statistics
#'   but takes longer to compute.
#'
#' @param r Numeric. The logistic map parameter. Valid range is [0, 4],
#'   though values outside [2.5, 4] are rarely used. Recommended values:
#'   - r = 3.8 for robust chaotic dynamics
#'   - r = 4.0 for fully chaotic behavior with known invariant density
#'   - r = 3.2 for periodic behavior (educational comparison)
#'
#' @param x0 Numeric. Initial condition. Must be in the open interval (0, 1).
#'   The dynamics are typically not sensitive to x0 for chaotic parameter
#'   values (after a short transient), but periodic regimes may have multiple
#'   attractors. Default is 0.2.
#'
#' @return
#' Numeric vector of length n containing the simulated time series.
#' All values are typically in (0, 1) for standard parameter values.
#' The first element equals x0. For r = 4, the invariant density is
#' beta(0.5, 0.5), resulting in a characteristic U-shaped histogram.
#'
#' @references
#' May, R. M. (1976). Simple mathematical models with very complicated
#' dynamics. *Nature*, 261(5560), 459-467. \doi{10.1038/261459a0}
#'
#' Strogatz, S. H. (2015). *Nonlinear Dynamics and Chaos: With Applications
#' to Physics, Biology, Chemistry, and Engineering* (2nd ed.). Westview Press.
#'
#' @seealso
#' \code{\link{logistic_bifurcation}} to explore the bifurcation diagram across
#' parameter values, \code{\link{simulate_henon_map}} for a 2D chaotic system,
#' \code{\link{estimate_lyapunov_exponent}} to quantify chaotic behavior.
#'
#' For using this in extreme value analysis, see
#' \code{vignette("estimating-theta-logistic")}.
#'
#' @family simulation functions
#'
#' @examples
#' # Basic usage: Generate 500 iterations in chaotic regime
#' series <- simulate_logistic_map(n = 500, r = 3.8, x0 = 0.2)
#' head(series)
#'
#' # Visualize the chaotic time series
#' plot(series, type = "l", col = "steelblue",
#'      main = "Logistic Map Time Series (r = 3.8)",
#'      xlab = "Iteration", ylab = "x")
#'
#' # Compare different dynamical regimes
#' par(mfrow = c(2, 2))
#' plot(simulate_logistic_map(200, r = 2.5, x0 = 0.2), type = "l",
#'      main = "r = 2.5 (Fixed Point)", ylab = "x", col = "darkgreen")
#' plot(simulate_logistic_map(200, r = 3.2, x0 = 0.2), type = "l",
#'      main = "r = 3.2 (Period 2)", ylab = "x", col = "orange")
#' plot(simulate_logistic_map(200, r = 3.8, x0 = 0.2), type = "l",
#'      main = "r = 3.8 (Chaos)", ylab = "x", col = "red")
#' plot(simulate_logistic_map(200, r = 4.0, x0 = 0.2), type = "l",
#'      main = "r = 4.0 (Full Chaos)", ylab = "x", col = "purple")
#' par(mfrow = c(1, 1))
#'
#' # Examine the invariant distribution for r = 4
#' long_series <- simulate_logistic_map(n = 10000, r = 4.0, x0 = 0.2)
#' hist(long_series, breaks = 50, probability = TRUE,
#'      main = "Invariant Density (r = 4.0)",
#'      xlab = "x", col = "lightblue", border = "white")
#' # Note the U-shape characteristic of beta(0.5, 0.5)
#'
#' # Sensitivity to initial conditions (chaos demonstration)
#' x1 <- simulate_logistic_map(100, r = 4.0, x0 = 0.2)
#' x2 <- simulate_logistic_map(100, r = 4.0, x0 = 0.200001)
#' plot(abs(x1 - x2), type = "l", log = "y",
#'      main = "Sensitive Dependence on Initial Conditions",
#'      xlab = "Iteration", ylab = "|x1 - x2| (log scale)")
#' # Trajectories diverge exponentially despite nearly identical start
#'
#' \donttest{
#' # Generate very long series for statistical analysis
#' statistical_sample <- simulate_logistic_map(n = 100000, r = 3.8, x0 = 0.2)
#'
#' # Estimate extremal index
#' threshold <- quantile(statistical_sample, 0.95)
#' theta <- extremal_index_runs(statistical_sample, threshold, run_length = 2)
#' print(theta)
#' }
#'
#' @export
simulate_logistic_map <- function(n, r, x0) {
  checkmate::assert_count(n)
  checkmate::assert_number(r)
  checkmate::assert_number(x0, lower = 0, upper = 1)
  if (x0 <= 0 || x0 >= 1) {
    stop("x0 must be strictly between 0 and 1")
  }
  x <- numeric(n)
  x[1] <- x0
  for (i in 1:(n - 1)) {
    x[i + 1] <- r * x[i] * (1 - x[i])  # logistic iteration
  }
  x
}

#' Simulate the Hénon map
#'
#' Generates an orbit for the two-dimensional Hénon map:
#'   x[n+1] = 1 - a * x[n]^2 + y[n]
#'   y[n+1] = b * x[n]
#'
#' @param n Integer. Number of iterations to generate.
#' @param a Numeric. Parameter `a` controlling nonlinearity.
#' @param b Numeric. Parameter `b` controlling contraction.
#' @param x0 Numeric. Initial x value.
#' @param y0 Numeric. Initial y value.
#'
#' @return Data frame with columns `x` and `y` containing the orbit. The
#'   function stops if `n` is less than one.
#' @seealso [simulate_logistic_map()] for 1D logistic map,
#'   [simulate_lozi_map()] for piecewise-linear alternative
#' @examples
#' orbit <- simulate_henon_map(100, 1.4, 0.3)
#' @export
simulate_henon_map <- function(n, a = 1.4, b = 0.3, x0 = 0, y0 = 0) {
  checkmate::assert_count(n)
  checkmate::assert_number(a)
  checkmate::assert_number(b)
  checkmate::assert_number(x0)
  checkmate::assert_number(y0)
  x <- numeric(n)
  y <- numeric(n)
  x[1] <- x0
  y[1] <- y0
  for (i in 1:(n - 1)) {
    x[i + 1] <- 1 - a * x[i]^2 + y[i]
    y[i + 1] <- b * x[i]
  }
  data.frame(x = x, y = y)
}

#' Logistic map bifurcation diagram
#'
#' Generate data for the classic logistic map bifurcation diagram by
#' iterating the map for a sequence of parameter values.
#'
#' @param r_seq Numeric vector of \eqn{r} parameters to evaluate.
#' @param n_iter Integer. Number of iterations for each parameter.
#' @param discard Integer. Number of initial iterations to discard as
#'   transient. Must be less than \code{n_iter}.
#' @param x0 Numeric. Initial value in (0, 1) for the orbit.
#'
#' @return Data frame with columns `r` and `x` containing orbit values after
#'   the transient period for each parameter in `r_seq`.
#'
#' @examples
#' r_vals <- seq(2.5, 4, length.out = 200)
#' bifdat <- logistic_bifurcation(r_vals, n_iter = 200, discard = 100)
#' plot(bifdat$r, bifdat$x, pch = '.', cex = 0.5)
#' @export
logistic_bifurcation <- function(r_seq, n_iter = 200, discard = 100, x0 = 0.2) {
  checkmate::assert_numeric(r_seq, any.missing = FALSE, min.len = 1)
  checkmate::assert_int(discard, lower = 0)
  checkmate::assert_int(n_iter, lower = discard + 1)
  checkmate::assert_number(x0, lower = 0, upper = 1)
  if (x0 <= 0 || x0 >= 1) {
    stop("x0 must be strictly between 0 and 1")
  }

  keep <- n_iter - discard
  total <- length(r_seq) * keep
  r_out <- numeric(total)
  x_out <- numeric(total)
  idx <- 1L

  for (r in r_seq) {
    x <- x0
    for (i in seq_len(n_iter)) {
      x <- r * x * (1 - x)
      if (i > discard) {
        r_out[idx] <- r
        x_out[idx] <- x
        idx <- idx + 1L
      }
    }
  }

  data.frame(r = r_out, x = x_out)
}
#' Simulate the tent map
#'
#' Generates a time series from the tent map:
#'   x[n+1] = r * x[n]             if x[n] < 0.5
#'             r * (1 - x[n])      otherwise
#'
#' @param n Integer. Number of iterations to generate.
#' @param r Numeric. Slope parameter (0 < r <= 2). Defaults to 2.
#' @param x0 Numeric. Initial value in (0, 1). Defaults to 0.1.
#'
#' @return Numeric vector containing the orbit of length n.
#' @examples
#' series <- simulate_tent_map(100, r = 2, x0 = 0.1)
#' @export
simulate_tent_map <- function(n, r = 2, x0 = 0.1) {
  checkmate::assert_count(n)
  checkmate::assert_number(r, lower = 0, upper = 2)
  if (r <= 0) {
    stop("r must be strictly greater than 0")
  }
  checkmate::assert_number(x0, lower = 0, upper = 1)
  if (x0 <= 0 || x0 >= 1) {
    stop("x0 must be strictly between 0 and 1")
  }
  x <- numeric(n)
  x[1] <- x0
  for (i in 1:(n - 1)) {
    if (x[i] < 0.5) {
      x[i + 1] <- r * x[i]
    } else {
      x[i + 1] <- r * (1 - x[i])
    }
  }
  x
}

#' Simulate the Lozi map
#'
#' Generates an orbit for the two-dimensional Lozi map:
#'   x[n+1] = 1 - a * abs(x[n]) + b * y[n]
#'   y[n+1] = x[n]
#'
#' @param n Integer. Number of iterations to generate.
#' @param a Numeric. Parameter controlling the nonlinearity. Defaults to 1.7.
#' @param b Numeric. Parameter controlling the contraction. Defaults to 0.5.
#' @param x0 Numeric. Initial x value. Defaults to 0.
#' @param y0 Numeric. Initial y value. Defaults to 0.
#'
#' @return Data frame with columns `x` and `y` of length `n`.
#' @examples
#' orbit <- simulate_lozi_map(100)
#' @export
simulate_lozi_map <- function(n, a = 1.7, b = 0.5, x0 = 0, y0 = 0) {
  checkmate::assert_count(n)
  checkmate::assert_number(a)
  checkmate::assert_number(b)
  checkmate::assert_number(x0)
  checkmate::assert_number(y0)
  x <- numeric(n)
  y <- numeric(n)
  x[1] <- x0
  y[1] <- y0
  for (i in 1:(n - 1)) {
    x[i + 1] <- 1 - a * abs(x[i]) + b * y[i]
    y[i + 1] <- x[i]
  }
  data.frame(x = x, y = y)
}
#' Simulate the Arnold cat map
#'
#' Generates an orbit for the two-dimensional Arnold cat map:
#'   x[n+1] = (x[n] + y[n]) mod 1
#'   y[n+1] = (x[n] + 2 * y[n]) mod 1
#'
#' @param n Integer. Number of iterations to generate.
#' @param x0 Numeric. Initial x value. Defaults to 0.1.
#' @param y0 Numeric. Initial y value. Defaults to 0.1.
#'
#' @return Data frame with columns `x` and `y` of length `n`.
#' @examples
#' orbit <- simulate_cat_map(100)
#' @export
simulate_cat_map <- function(n, x0 = 0.1, y0 = 0.1) {
  checkmate::assert_count(n)
  checkmate::assert_number(x0)
  checkmate::assert_number(y0)
  x <- numeric(n)
  y <- numeric(n)
  x[1] <- x0 %% 1
  y[1] <- y0 %% 1
  for (i in 1:(n - 1)) {
    x_new <- (x[i] + y[i]) %% 1
    y_new <- (x[i] + 2 * y[i]) %% 1
    x[i + 1] <- x_new
    y[i + 1] <- y_new
  }
  data.frame(x = x, y = y)
}
