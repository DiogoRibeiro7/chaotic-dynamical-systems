#' Simulate globally coupled Kuramoto oscillators
#'
#' Simulates the classical all-to-all Kuramoto phase-oscillator model with a
#' fixed-step fourth-order Runge-Kutta integrator. The R implementation is the
#' reference specification; use simulate_kuramoto_cpp() for the matching C++
#' fast path.
#'
#' @details
#' The model is
#' \deqn{\dot{\theta}_i = \omega_i +
#'   \frac{K}{N}\sum_{j=1}^{N}\sin(\theta_j-\theta_i).}
#'
#' The coupling term is evaluated in linear time using the complex order
#' parameter identity
#' \deqn{\frac{1}{N}\sum_j \sin(\theta_j-\theta_i)
#'   = S\cos(\theta_i) - C\sin(\theta_i),}
#' where \eqn{C = N^{-1}\sum_j\cos(\theta_j)} and
#' \eqn{S = N^{-1}\sum_j\sin(\theta_j)}. This avoids an explicit
#' \eqn{O(N^2)} pairwise-coupling loop.
#'
#' If omega is NULL, natural frequencies are deterministically spaced on
#' [-1, 1]. If theta0 is NULL, initial phases are evenly distributed on
#' [0, 2*pi). Returned phases are unwrapped.
#'
#' @param t_max Numeric. Simulation time retained after the transient.
#' @param dt Numeric. Integration step size.
#' @param n Integer. Number of oscillators. Must be at least 2.
#' @param coupling Numeric. Global coupling strength K. Positive values are
#'   attractive and negative values are repulsive.
#' @param omega NULL, a numeric scalar, or a numeric vector of length n.
#'   Natural frequencies. A scalar is replicated.
#' @param theta0 NULL, a numeric scalar, or a numeric vector of length n.
#'   Initial phases in radians. A scalar is replicated.
#' @param transient Numeric. Initial simulation time to discard.
#'
#' @return An object of class kuramoto_simulation, a list containing:
#' \describe{
#'   \item{t}{Numeric vector of retained times.}
#'   \item{theta}{Numeric matrix of unwrapped phases, one oscillator per column.}
#'   \item{order_parameter}{Kuramoto synchronization magnitude in [0, 1].}
#'   \item{omega}{Natural frequencies used in the simulation.}
#'   \item{coupling}{Coupling strength.}
#'   \item{dt}{Integration step size.}
#' }
#'
#' @references
#' Kuramoto, Y. (1975). Self-entrainment of a population of coupled nonlinear
#' oscillators. In H. Araki (Ed.), International Symposium on Mathematical
#' Problems in Theoretical Physics, Lecture Notes in Physics 39, 420-422.
#'
#' Strogatz, S. H. (2000). From Kuramoto to Crawford: exploring the onset of
#' synchronization in populations of coupled oscillators. Physica D, 143,
#' 1-20.
#'
#' @examples
#' sim <- simulate_kuramoto(
#'   t_max = 10,
#'   dt = 0.05,
#'   n = 16,
#'   coupling = 1.5
#' )
#' plot(sim$t, sim$order_parameter, type = "l",
#'      xlab = "Time", ylab = "Order parameter")
#'
#' @family simulation functions
#' @export
simulate_kuramoto <- function(
    t_max = 50,
    dt = 0.05,
    n = 32L,
    coupling = 1,
    omega = NULL,
    theta0 = NULL,
    transient = 0) {
  inputs <- .kuramoto_inputs(
    t_max = t_max,
    dt = dt,
    n = n,
    coupling = coupling,
    omega = omega,
    theta0 = theta0,
    transient = transient
  )

  total_steps <- inputs$n_transient + inputs$n_keep
  theta <- matrix(0, nrow = total_steps + 1L, ncol = inputs$n)
  theta[1L, ] <- inputs$theta0

  if (total_steps > 0L) {
    for (step in seq_len(total_steps)) {
      current <- theta[step, ]
      k1 <- .kuramoto_derivative(current, inputs$omega, inputs$coupling)
      k2 <- .kuramoto_derivative(
        current + 0.5 * inputs$dt * k1,
        inputs$omega,
        inputs$coupling
      )
      k3 <- .kuramoto_derivative(
        current + 0.5 * inputs$dt * k2,
        inputs$omega,
        inputs$coupling
      )
      k4 <- .kuramoto_derivative(
        current + inputs$dt * k3,
        inputs$omega,
        inputs$coupling
      )
      theta[step + 1L, ] <- current +
        inputs$dt * (k1 + 2 * k2 + 2 * k3 + k4) / 6
    }
  }

  keep <- seq.int(
    inputs$n_transient + 1L,
    total_steps + 1L
  )
  kept_theta <- theta[keep, , drop = FALSE]

  .new_kuramoto_simulation(
    theta = kept_theta,
    omega = inputs$omega,
    coupling = inputs$coupling,
    dt = inputs$dt
  )
}

#' Fast globally coupled Kuramoto oscillator simulation
#'
#' Uses the same RK4 scheme and linear-time order-parameter formulation as
#' simulate_kuramoto(), with the integration loop evaluated in C++.
#'
#' @inheritParams simulate_kuramoto
#' @return A kuramoto_simulation object with the same structure as
#'   simulate_kuramoto().
#' @rdname simulate_kuramoto
#' @export
simulate_kuramoto_cpp <- function(
    t_max = 50,
    dt = 0.05,
    n = 32L,
    coupling = 1,
    omega = NULL,
    theta0 = NULL,
    transient = 0) {
  inputs <- .kuramoto_inputs(
    t_max = t_max,
    dt = dt,
    n = n,
    coupling = coupling,
    omega = omega,
    theta0 = theta0,
    transient = transient
  )

  theta <- .Call(
    "_chaoticds_kuramoto_simulate_cpp_impl",
    as.integer(inputs$n_keep),
    as.integer(inputs$n_transient),
    as.numeric(inputs$dt),
    as.numeric(inputs$coupling),
    as.numeric(inputs$omega),
    as.numeric(inputs$theta0)
  )

  .new_kuramoto_simulation(
    theta = theta,
    omega = inputs$omega,
    coupling = inputs$coupling,
    dt = inputs$dt
  )
}

#' Kuramoto synchronization order parameter
#'
#' Computes the magnitude of the complex Kuramoto order parameter for one
#' phase vector or for every row of a phase matrix.
#'
#' @param theta Numeric vector or matrix of oscillator phases in radians.
#'
#' @return A numeric scalar for a vector input, or one value per matrix row.
#'   Values lie in [0, 1], where 1 indicates complete phase synchronization.
#'
#' @examples
#' kuramoto_order_parameter(c(0, pi))
#' kuramoto_order_parameter(matrix(c(0, 0, 0, pi), nrow = 2, byrow = TRUE))
#'
#' @export
kuramoto_order_parameter <- function(theta) {
  checkmate::assert_numeric(theta, any.missing = FALSE, finite = TRUE)

  if (is.null(dim(theta))) {
    return(sqrt(mean(cos(theta))^2 + mean(sin(theta))^2))
  }

  checkmate::assert_matrix(theta, mode = "numeric", any.missing = FALSE)
  sqrt(
    rowMeans(cos(theta))^2 +
      rowMeans(sin(theta))^2
  )
}

.kuramoto_inputs <- function(
    t_max,
    dt,
    n,
    coupling,
    omega,
    theta0,
    transient) {
  checkmate::assert_number(t_max, lower = .Machine$double.eps, finite = TRUE)
  checkmate::assert_number(dt, lower = .Machine$double.eps, finite = TRUE)
  checkmate::assert_int(n, lower = 2L)
  checkmate::assert_number(coupling, finite = TRUE)
  checkmate::assert_number(transient, lower = 0, finite = TRUE)

  omega <- .kuramoto_vector(
    omega,
    n = n,
    default = seq(-1, 1, length.out = n),
    name = "omega"
  )
  theta0 <- .kuramoto_vector(
    theta0,
    n = n,
    default = 2 * pi * (seq_len(n) - 1L) / n,
    name = "theta0"
  )

  n_keep <- as.integer(round(t_max / dt))
  n_transient <- as.integer(round(transient / dt))
  if (n_keep < 1L) {
    stop("dt is too large relative to t_max", call. = FALSE)
  }

  list(
    n = n,
    n_keep = n_keep,
    n_transient = n_transient,
    dt = dt,
    coupling = coupling,
    omega = omega,
    theta0 = theta0
  )
}

.kuramoto_vector <- function(x, n, default, name) {
  if (is.null(x)) {
    return(as.numeric(default))
  }

  checkmate::assert_numeric(
    x,
    any.missing = FALSE,
    finite = TRUE,
    .var.name = name
  )
  if (length(x) == 1L) {
    x <- rep(x, n)
  }
  if (length(x) != n) {
    stop(name, " must be NULL, a scalar, or have length n", call. = FALSE)
  }
  as.numeric(x)
}

.kuramoto_derivative <- function(theta, omega, coupling) {
  mean_cos <- mean(cos(theta))
  mean_sin <- mean(sin(theta))
  omega + coupling * (
    mean_sin * cos(theta) -
      mean_cos * sin(theta)
  )
}

.new_kuramoto_simulation <- function(theta, omega, coupling, dt) {
  structure(
    list(
      t = seq.int(0L, nrow(theta) - 1L) * dt,
      theta = theta,
      order_parameter = kuramoto_order_parameter(theta),
      omega = omega,
      coupling = coupling,
      dt = dt
    ),
    class = "kuramoto_simulation"
  )
}
