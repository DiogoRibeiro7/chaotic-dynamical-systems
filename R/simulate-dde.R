# Mackey-Glass delay-differential equation simulator.
#
# Explicit Euler discretisation with delay buffer; standard for this
# system because the dynamics are well-conditioned at the conventional
# parameter values (beta = 0.2, gamma = 0.1, n = 10, tau = 17) for any
# reasonable dt. Higher-order DDE integrators (RK4 with interpolated
# delay) are not worth the extra complexity here.

#' Simulate the Mackey-Glass delay-differential equation
#'
#' @description
#' Integrates the Mackey-Glass equation, a one-dimensional
#' delay-differential equation widely used as a chaotic benchmark:
#' \deqn{\frac{dx}{dt} = \beta \,\frac{x(t - \tau)}{1 + x(t - \tau)^n}
#'                       - \gamma\, x(t).}
#'
#' With the conventional parameters (\eqn{\beta = 0.2}, \eqn{\gamma = 0.1},
#' \eqn{n = 10}, \eqn{\tau = 17}) the system is chaotic with a strange
#' attractor of fractal dimension ~3.
#'
#' @details
#' Discretisation: explicit Euler with a fixed delay buffer
#' of size `round(tau / dt)`. The history segment \eqn{x(t)} for
#' \eqn{t \in [-\tau, 0]} is taken to be the constant `x0`. The integrator
#' produces `round(t_max / dt) + 1` post-transient points.
#'
#' @param t_max Numeric (\eqn{> 0}). Total integration time after the
#'   transient. Defaults to 200.
#' @param dt Numeric (\eqn{> 0}). Integration step size. Defaults to 0.1;
#'   smaller values give a sharper attractor at higher computational cost.
#' @param x0 Numeric. Initial / history value, used for \eqn{t \in
#'   [-\tau, 0]}. Defaults to 1.2.
#' @param beta,gamma Numeric. Production and decay rates. Defaults to 0.2
#'   and 0.1.
#' @param n Numeric. Hill exponent in the production term. Defaults to 10.
#' @param tau Numeric (\eqn{> 0}). Delay. Defaults to 17.
#' @param transient Numeric (\eqn{\ge 0}). Time discarded from the
#'   beginning of the trajectory. Defaults to 0; set to ~`tau * 4` if you
#'   want to start cleanly on the attractor.
#'
#' @return Data frame with columns `t` and `x`. The first time stamp is 0.
#'
#' @references
#' Mackey, M. C., & Glass, L. (1977). Oscillation and chaos in
#' physiological control systems. *Science*, 197(4300), 287-289.
#' \doi{10.1126/science.267326}
#'
#' @seealso [simulate_lorenz()] for the canonical continuous chaotic flow.
#' @family simulation functions
#'
#' @examples
#' traj <- simulate_mackey_glass(t_max = 50, dt = 0.1, transient = 30)
#' plot(traj$t, traj$x, type = "l",
#'      main = "Mackey-Glass (tau = 17)", xlab = "t", ylab = "x")
#'
#' @export
simulate_mackey_glass <- function(t_max = 200, dt = 0.1, x0 = 1.2,
                                   beta = 0.2, gamma = 0.1,
                                   n = 10, tau = 17, transient = 0) {
  checkmate::assert_number(t_max, lower = 1e-9, finite = TRUE)
  checkmate::assert_number(dt,    lower = 1e-9, finite = TRUE)
  checkmate::assert_number(x0)
  checkmate::assert_number(beta, finite = TRUE)
  checkmate::assert_number(gamma, finite = TRUE)
  checkmate::assert_number(n, lower = 0, finite = TRUE)
  checkmate::assert_number(tau, lower = 1e-9, finite = TRUE)
  checkmate::assert_number(transient, lower = 0, finite = TRUE)

  n_delay     <- as.integer(round(tau / dt))
  n_transient <- as.integer(round(transient / dt))
  n_keep      <- as.integer(round(t_max / dt))
  n_total     <- n_transient + n_keep

  # Buffer: indices 1..n_delay+1 hold the constant history (x0); the
  # integrator advances from index n_delay+1 to n_delay+1+n_total.
  buffer <- numeric(n_delay + 1L + n_total)
  buffer[seq_len(n_delay + 1L)] <- x0

  for (k in seq.int(n_delay + 1L, n_delay + n_total)) {
    x_d   <- buffer[k - n_delay]
    dxdt  <- beta * x_d / (1 + x_d^n) - gamma * buffer[k]
    buffer[k + 1L] <- buffer[k] + dt * dxdt
  }

  out_start <- n_delay + 1L + n_transient
  out_end   <- n_delay + 1L + n_total
  data.frame(
    t = seq(0, by = dt, length.out = out_end - out_start + 1L),
    x = buffer[out_start:out_end]
  )
}
