# Fixed-step classical Runge-Kutta integrator (RK4). Used by the continuous-
# time chaotic simulators in this file. Kept internal so the public API stays
# small. The Lorenz / Rossler / Duffing systems are non-stiff at the default
# step sizes used here, so a fixed-step scheme is adequate; users who need
# adaptive stepping for harder problems can call deSolve::ode directly.
.rk4_solve <- function(deriv, y0, t_max, dt, transient = 0) {
  total <- transient + t_max
  n_steps <- as.integer(round(total / dt))
  if (n_steps < 1L) {
    stop("dt is too large relative to t_max (no integration steps produced)")
  }
  t_full <- seq(0, by = dt, length.out = n_steps + 1L)
  d <- length(y0)
  Y <- matrix(0, nrow = n_steps + 1L, ncol = d)
  Y[1L, ] <- y0
  for (i in seq_len(n_steps)) {
    yi <- Y[i, ]
    ti <- t_full[i]
    k1 <- deriv(ti,          yi)
    k2 <- deriv(ti + dt / 2, yi + dt / 2 * k1)
    k3 <- deriv(ti + dt / 2, yi + dt / 2 * k2)
    k4 <- deriv(ti + dt,     yi + dt     * k3)
    Y[i + 1L, ] <- yi + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  keep <- t_full >= transient
  list(t = t_full[keep] - transient, Y = Y[keep, , drop = FALSE])
}

#' Simulate the Lorenz system
#'
#' @description
#' Integrates the three-dimensional Lorenz system, a canonical continuous-time
#' chaotic attractor introduced by Edward Lorenz (1963) as a simplified model
#' of atmospheric convection. With the classical parameters
#' (\eqn{\sigma = 10}, \eqn{\rho = 28}, \eqn{\beta = 8/3}) the trajectory
#' settles onto the famous butterfly-shaped strange attractor.
#'
#' @details
#' The system is
#' \deqn{\dot{x} = \sigma (y - x),}
#' \deqn{\dot{y} = x (\rho - z) - y,}
#' \deqn{\dot{z} = x y - \beta z.}
#'
#' Integration uses a fixed-step classical RK4 scheme. The defaults
#' (\code{dt = 0.01}, \code{t_max = 50}) give roughly five Lyapunov times of
#' trajectory on the attractor and are a good starting point for extreme value
#' analyses of any of \code{x}, \code{y}, or \code{z}.
#'
#' @param t_max Numeric. Total integration time after any \code{transient}.
#' @param dt Numeric. Integration step size.
#' @param x0,y0,z0 Numeric. Initial conditions. The default
#'   \code{(1, 1, 1.05)} lands on the attractor after a brief transient.
#' @param sigma,rho,beta Numeric. Lorenz parameters. Defaults are the
#'   classical chaotic regime.
#' @param transient Numeric (\eqn{\ge 0}). Integration time discarded from the
#'   start of the trajectory to let it settle onto the attractor. The returned
#'   \code{t} column starts at zero regardless.
#'
#' @return Data frame with columns \code{t}, \code{x}, \code{y}, \code{z}.
#'
#' @references
#' Lorenz, E. N. (1963). Deterministic nonperiodic flow. *Journal of the
#' Atmospheric Sciences*, 20(2), 130-141.
#' \doi{10.1175/1520-0469(1963)020<0130:DNF>2.0.CO;2}
#'
#' @seealso [simulate_rossler()], [simulate_duffing()], [simulate_henon_map()].
#' @family simulation functions
#'
#' @examples
#' traj <- simulate_lorenz(t_max = 20, dt = 0.01, transient = 5)
#' plot(traj$x, traj$z, type = "l", xlab = "x", ylab = "z",
#'      main = "Lorenz attractor (x-z projection)")
#'
#' \donttest{
#' # Extreme value analysis on the z-coordinate
#' traj <- simulate_lorenz(t_max = 500, dt = 0.01, transient = 20)
#' z <- traj$z
#' threshold <- quantile(z, 0.95)
#' theta <- extremal_index_runs(z, threshold, run_length = 5)
#' theta
#' }
#'
#' @export
simulate_lorenz <- function(t_max = 50, dt = 0.01,
                            x0 = 1, y0 = 1, z0 = 1.05,
                            sigma = 10, rho = 28, beta = 8 / 3,
                            transient = 0) {
  checkmate::assert_number(t_max, lower = 0, finite = TRUE)
  checkmate::assert_number(dt, lower = 0, finite = TRUE)
  if (t_max <= 0) stop("t_max must be strictly positive")
  if (dt <= 0)    stop("dt must be strictly positive")
  checkmate::assert_number(x0, finite = TRUE)
  checkmate::assert_number(y0, finite = TRUE)
  checkmate::assert_number(z0, finite = TRUE)
  checkmate::assert_number(sigma, finite = TRUE)
  checkmate::assert_number(rho, finite = TRUE)
  checkmate::assert_number(beta, finite = TRUE)
  checkmate::assert_number(transient, lower = 0, finite = TRUE)

  deriv <- function(t, y) {
    c(sigma * (y[2L] - y[1L]),
      y[1L] * (rho - y[3L]) - y[2L],
      y[1L] * y[2L] - beta * y[3L])
  }
  sol <- .rk4_solve(deriv, c(x0, y0, z0), t_max, dt, transient)
  data.frame(t = sol$t,
             x = sol$Y[, 1L],
             y = sol$Y[, 2L],
             z = sol$Y[, 3L])
}

#' Simulate the Rossler system
#'
#' @description
#' Integrates the three-dimensional Rossler system (Otto Rossler, 1976), a
#' continuous-time chaotic flow built from a single quadratic nonlinearity.
#' With the standard parameters (\eqn{a = 0.2}, \eqn{b = 0.2}, \eqn{c = 5.7})
#' the trajectory traces out the spiral-and-fold Rossler attractor.
#'
#' @details
#' The system is
#' \deqn{\dot{x} = -(y + z),}
#' \deqn{\dot{y} = x + a y,}
#' \deqn{\dot{z} = b + z (x - c).}
#'
#' Compared with Lorenz, the Rossler attractor evolves on a slower timescale,
#' which is why \code{t_max} defaults to \code{200} and \code{dt} to
#' \code{0.05}.
#'
#' @param t_max Numeric. Total integration time after any \code{transient}.
#' @param dt Numeric. Integration step size.
#' @param x0,y0,z0 Numeric. Initial conditions.
#' @param a,b,c Numeric. Rossler parameters. Defaults are the standard
#'   chaotic regime.
#' @param transient Numeric (\eqn{\ge 0}). Integration time discarded from the
#'   start of the trajectory.
#'
#' @return Data frame with columns \code{t}, \code{x}, \code{y}, \code{z}.
#'
#' @references
#' Rossler, O. E. (1976). An equation for continuous chaos. *Physics Letters
#' A*, 57(5), 397-398. \doi{10.1016/0375-9601(76)90101-8}
#'
#' @seealso [simulate_lorenz()], [simulate_duffing()].
#' @family simulation functions
#'
#' @examples
#' traj <- simulate_rossler(t_max = 100, dt = 0.05, transient = 20)
#' plot(traj$x, traj$y, type = "l", xlab = "x", ylab = "y",
#'      main = "Rossler attractor (x-y projection)")
#'
#' @export
simulate_rossler <- function(t_max = 200, dt = 0.05,
                             x0 = 0, y0 = 1, z0 = 0,
                             a = 0.2, b = 0.2, c = 5.7,
                             transient = 0) {
  checkmate::assert_number(t_max, lower = 0, finite = TRUE)
  checkmate::assert_number(dt, lower = 0, finite = TRUE)
  if (t_max <= 0) stop("t_max must be strictly positive")
  if (dt <= 0)    stop("dt must be strictly positive")
  checkmate::assert_number(x0, finite = TRUE)
  checkmate::assert_number(y0, finite = TRUE)
  checkmate::assert_number(z0, finite = TRUE)
  checkmate::assert_number(a, finite = TRUE)
  checkmate::assert_number(b, finite = TRUE)
  checkmate::assert_number(c, finite = TRUE)
  checkmate::assert_number(transient, lower = 0, finite = TRUE)

  a_par <- a; b_par <- b; c_par <- c
  deriv <- function(t, y) {
    c(-(y[2L] + y[3L]),
      y[1L] + a_par * y[2L],
      b_par + y[3L] * (y[1L] - c_par))
  }
  sol <- .rk4_solve(deriv, c(x0, y0, z0), t_max, dt, transient)
  data.frame(t = sol$t,
             x = sol$Y[, 1L],
             y = sol$Y[, 2L],
             z = sol$Y[, 3L])
}

#' Simulate the forced Duffing oscillator
#'
#' @description
#' Integrates the periodically forced Duffing oscillator, a damped nonlinear
#' oscillator that exhibits chaotic motion across well-known parameter
#' windows. With the default parameters
#' (\eqn{\alpha = -1}, \eqn{\beta = 1}, \eqn{\delta = 0.2}, \eqn{\gamma = 0.3},
#' \eqn{\omega = 1}) the system lives in the classical double-well chaotic
#' regime.
#'
#' @details
#' The state vector is \eqn{(x, v)} with \eqn{v = \dot{x}} and dynamics
#' \deqn{\dot{x} = v,}
#' \deqn{\dot{v} = -\delta v - \alpha x - \beta x^3 + \gamma \cos(\omega t).}
#'
#' Unlike Lorenz and Rossler the system has explicit time dependence (the
#' periodic forcing), so the integrator passes \code{t} into the derivative.
#'
#' @param t_max Numeric. Total integration time after any \code{transient}.
#' @param dt Numeric. Integration step size.
#' @param x0,v0 Numeric. Initial position and velocity.
#' @param alpha,beta Numeric. Linear and cubic stiffness coefficients. The
#'   classical double-well chaotic regime uses \eqn{\alpha < 0}, \eqn{\beta > 0}.
#' @param delta Numeric. Damping coefficient.
#' @param gamma,omega Numeric. Forcing amplitude and angular frequency.
#' @param transient Numeric (\eqn{\ge 0}). Integration time discarded from the
#'   start of the trajectory.
#'
#' @return Data frame with columns \code{t}, \code{x}, \code{v}.
#'
#' @references
#' Guckenheimer, J., & Holmes, P. (1983). *Nonlinear Oscillations, Dynamical
#' Systems, and Bifurcations of Vector Fields*. Springer.
#' \doi{10.1007/978-1-4612-1140-2}
#'
#' @seealso [simulate_lorenz()], [simulate_rossler()].
#' @family simulation functions
#'
#' @examples
#' traj <- simulate_duffing(t_max = 80, dt = 0.05, transient = 20)
#' plot(traj$x, traj$v, type = "l", xlab = "x", ylab = "v",
#'      main = "Duffing oscillator (phase portrait)")
#'
#' @export
simulate_duffing <- function(t_max = 100, dt = 0.05,
                             x0 = 1, v0 = 0,
                             alpha = -1, beta = 1, delta = 0.2,
                             gamma = 0.3, omega = 1,
                             transient = 0) {
  checkmate::assert_number(t_max, lower = 0, finite = TRUE)
  checkmate::assert_number(dt, lower = 0, finite = TRUE)
  if (t_max <= 0) stop("t_max must be strictly positive")
  if (dt <= 0)    stop("dt must be strictly positive")
  checkmate::assert_number(x0, finite = TRUE)
  checkmate::assert_number(v0, finite = TRUE)
  checkmate::assert_number(alpha, finite = TRUE)
  checkmate::assert_number(beta, finite = TRUE)
  checkmate::assert_number(delta, finite = TRUE)
  checkmate::assert_number(gamma, finite = TRUE)
  checkmate::assert_number(omega, finite = TRUE)
  checkmate::assert_number(transient, lower = 0, finite = TRUE)

  deriv <- function(t, y) {
    c(y[2L],
      -delta * y[2L] - alpha * y[1L] - beta * y[1L]^3 +
        gamma * cos(omega * t))
  }
  sol <- .rk4_solve(deriv, c(x0, v0), t_max, dt, transient)
  data.frame(t = sol$t,
             x = sol$Y[, 1L],
             v = sol$Y[, 2L])
}
