# ---------------------------------------------------------------------------
# Lyapunov spectrum via Benettin's algorithm for discrete maps.
#
# Steps along the orbit and propagates an orthonormal d x d basis V through
# the linearised dynamics: V_{k+1} = J(x_k) %*% V_k, QR-decomposed each step
# to keep V orthonormal. The log-diagonal entries of R accumulate into
# lambda_i; dividing by n_iter gives the spectrum.
#
# The generic lyapunov_spectrum() takes user-supplied map and Jacobian
# functions; the convenience wrappers below hard-code the Jacobians for
# the maps shipped with the package (logistic, Henon, Lozi). Continuous-
# time spectra (Lorenz, Rossler, ...) require integrating the variational
# equation and live as Phase 3 follow-up work.
# ---------------------------------------------------------------------------

#' Lyapunov spectrum of a discrete map
#'
#' @description
#' Estimates the full Lyapunov spectrum \eqn{\lambda_1 \ge \ldots \ge
#' \lambda_d} of a discrete map \eqn{x \mapsto f(x)} via Benettin's QR
#' algorithm: at each step the linearised tangent map \eqn{J(x_k)} acts on
#' an orthonormal basis \eqn{V_k}, and the running average of the log
#' diagonal of the QR factor gives the Lyapunov exponents.
#'
#' @details
#' For a chaotic system, \eqn{\lambda_1 > 0}. The sum of the spectrum
#' equals the time-averaged log determinant of the Jacobian, so for
#' area-contracting maps (\eqn{|\det J| < 1}) the sum is negative.
#'
#' Implementation notes:
#'
#' - The QR factorisation is sign-corrected so that the diagonal of `R`
#'   is positive. This keeps the basis update deterministic and avoids
#'   the sign ambiguity that would otherwise show up after every step.
#' - A short transient of orbit-only iteration discards the initial
#'   alignment of the basis with the unstable manifold; defaults to
#'   1000 iterations, which is plenty for the canonical maps.
#'
#' @param map_fn Function. Takes a numeric vector `x` of length `d` and
#'   returns the next state, also of length `d`.
#' @param jacobian_fn Function. Takes `x` and returns the \eqn{d \times d}
#'   Jacobian matrix \eqn{J(x) = \partial f / \partial x}.
#' @param x0 Numeric vector of length `d`. Initial condition.
#' @param n_iter Integer (\eqn{\ge 1}). Number of iterations to average
#'   the spectrum over. Defaults to 5000.
#' @param transient Integer (\eqn{\ge 0}). Iterations of orbit-only
#'   evolution before starting the QR sweep. Defaults to 1000.
#'
#' @return Numeric vector of length `d` holding the Lyapunov exponents in
#'   decreasing order.
#'
#' @references
#' Benettin, G., Galgani, L., Giorgilli, A., & Strelcyn, J. M. (1980).
#' Lyapunov characteristic exponents for smooth dynamical systems and
#' for Hamiltonian systems; a method for computing all of them.
#' *Meccanica*, 15(1), 9-30. \doi{10.1007/BF02128236}
#'
#' @seealso [estimate_lyapunov_exponent()] for the largest exponent only
#'   from a scalar time series, [lyapunov_spectrum_henon()] and friends
#'   for hard-coded presets.
#'
#' @examples
#' # Henon map at standard chaotic parameters.
#' henon_map <- function(state) c(1 - 1.4 * state[1]^2 + state[2], 0.3 * state[1])
#' henon_jac <- function(state) {
#'   matrix(c(-2 * 1.4 * state[1], 1,
#'             0.3,                0),
#'          nrow = 2, byrow = TRUE)
#' }
#' lyapunov_spectrum(henon_map, henon_jac, c(0, 0), n_iter = 2000)
#'
#' @export
lyapunov_spectrum <- function(map_fn, jacobian_fn, x0,
                              n_iter = 5000L, transient = 1000L) {
  checkmate::assert_function(map_fn)
  checkmate::assert_function(jacobian_fn)
  checkmate::assert_numeric(x0, any.missing = FALSE, min.len = 1L)
  checkmate::assert_int(n_iter, lower = 1L)
  checkmate::assert_int(transient, lower = 0L)

  d <- length(x0)
  x <- as.numeric(x0)

  # Burn off the orbit-only transient before starting Lyapunov accumulation.
  for (k in seq_len(transient)) {
    x <- as.numeric(map_fn(x))
  }

  V      <- diag(1, nrow = d, ncol = d)
  lambda <- numeric(d)

  for (k in seq_len(n_iter)) {
    J <- jacobian_fn(x)
    if (!is.matrix(J) || nrow(J) != d || ncol(J) != d) {
      stop("jacobian_fn() must return a ", d, " x ", d, " matrix")
    }
    V <- J %*% V
    qr_decomp <- qr(V)
    Q <- qr.Q(qr_decomp)
    R <- qr.R(qr_decomp)
    # Sign-correct so diag(R) > 0; this also resigns Q consistently.
    signs <- sign(diag(R))
    signs[signs == 0] <- 1
    Q <- Q %*% diag(signs, nrow = d, ncol = d)
    R <- diag(signs, nrow = d, ncol = d) %*% R
    lambda <- lambda + log(abs(diag(R)))
    V <- Q
    x <- as.numeric(map_fn(x))
  }

  lambda / n_iter
}

#' Lyapunov spectrum of the Henon map
#'
#' Convenience wrapper around [lyapunov_spectrum()] that hard-codes the
#' Henon-map dynamics and Jacobian. At the canonical parameters
#' (`a = 1.4`, `b = 0.3`) the spectrum is approximately
#' \eqn{(\lambda_1, \lambda_2) \approx (0.418, -1.622)}.
#'
#' @param n_iter,transient As in [lyapunov_spectrum()].
#' @param a,b Henon parameters.
#' @param x0,y0 Initial condition.
#' @return Length-2 numeric vector of Lyapunov exponents.
#' @seealso [lyapunov_spectrum()], [simulate_henon_map()].
#' @examples
#' lyapunov_spectrum_henon(n_iter = 2000)
#' @export
lyapunov_spectrum_henon <- function(n_iter = 5000L, transient = 1000L,
                                     a = 1.4, b = 0.3, x0 = 0, y0 = 0) {
  map_fn <- function(s) {
    c(1 - a * s[1L] * s[1L] + s[2L],
            b * s[1L])
  }
  jacobian_fn <- function(s) {
    matrix(c(-2 * a * s[1L], 1,
                          b, 0),
           nrow = 2, byrow = TRUE)
  }
  lyapunov_spectrum(map_fn, jacobian_fn, c(x0, y0), n_iter, transient)
}

#' Lyapunov spectrum of the Lozi map
#'
#' Convenience wrapper around [lyapunov_spectrum()] that hard-codes the
#' Lozi-map dynamics and Jacobian. At the canonical parameters
#' (`a = 1.7`, `b = 0.5`) the spectrum is approximately
#' \eqn{(\lambda_1, \lambda_2) \approx (0.47, -1.16)}.
#'
#' @param n_iter,transient As in [lyapunov_spectrum()].
#' @param a,b Lozi parameters.
#' @param x0,y0 Initial condition.
#' @return Length-2 numeric vector of Lyapunov exponents.
#' @seealso [lyapunov_spectrum()], [simulate_lozi_map()].
#' @examples
#' lyapunov_spectrum_lozi(n_iter = 2000)
#' @export
lyapunov_spectrum_lozi <- function(n_iter = 5000L, transient = 1000L,
                                    a = 1.7, b = 0.5, x0 = 0.1, y0 = 0.1) {
  map_fn <- function(s) {
    c(1 - a * abs(s[1L]) + b * s[2L],
                          s[1L])
  }
  jacobian_fn <- function(s) {
    matrix(c(-a * sign(s[1L]), b,
                             1, 0),
           nrow = 2, byrow = TRUE)
  }
  lyapunov_spectrum(map_fn, jacobian_fn, c(x0, y0), n_iter, transient)
}

#' Lyapunov spectrum of a continuous-time flow
#'
#' @description
#' Estimates the full Lyapunov spectrum of an autonomous ODE
#' \eqn{\dot x = f(x)} by integrating the orbit and the variational
#' equation \eqn{\dot V = J(x) V} jointly with a fixed-step RK4 scheme,
#' QR-decomposing the tangent basis every `qr_interval` time units, and
#' accumulating \eqn{\log |\det R_{ii}|}.
#'
#' @details
#' Standard Benettin/Wolf approach for flows. The orbit and the
#' \eqn{d \times d} tangent matrix \eqn{V} are advanced with the same RK4
#' step, sharing intermediate evaluations of the Jacobian at the half- and
#' full-step states. QR re-orthogonalisation prevents \eqn{V} from
#' collapsing onto the leading expanding direction; the time-averaged
#' diagonal logs of \eqn{R} give the spectrum.
#'
#' Sum check: for Lorenz with the canonical parameters the spectrum is
#' approximately \eqn{(0.906, 0, -14.572)} with sum
#' \eqn{-\sigma - 1 - \beta = -13.667}; for Rossler at \eqn{(0.2, 0.2, 5.7)}
#' it is approximately \eqn{(0.0714, 0, -5.392)}. The middle exponent is
#' identically zero in the direction of the flow.
#'
#' @param deriv_fn Function with signature `function(t, x)` returning the
#'   length-`d` derivative `dx/dt`.
#' @param jac_fn Function with signature `function(t, x)` returning the
#'   \eqn{d \times d} Jacobian.
#' @param x0 Numeric vector of length `d`. Initial state.
#' @param t_max Numeric (\eqn{> 0}). Integration time over which the
#'   spectrum is averaged, after the transient.
#' @param dt Numeric (\eqn{> 0}). RK4 step size.
#' @param qr_interval Numeric (\eqn{> 0}). Time between QR
#'   re-orthogonalisations.
#' @param transient Numeric (\eqn{\ge 0}). Orbit-only integration time
#'   discarded from the start.
#'
#' @return Numeric vector of length `d` holding the Lyapunov exponents in
#'   decreasing order.
#'
#' @references
#' Wolf, A., Swift, J. B., Swinney, H. L., & Vastano, J. A. (1985).
#' Determining Lyapunov exponents from a time series. *Physica D*,
#' 16(3), 285-317. \doi{10.1016/0167-2789(85)90011-9}
#'
#' @seealso [lyapunov_spectrum()] for the discrete-map version,
#'   [lyapunov_spectrum_lorenz()] and [lyapunov_spectrum_rossler()] for
#'   hard-coded presets.
#'
#' @examples
#' \donttest{
#' # Lorenz spectrum at canonical (10, 28, 8/3).
#' lyapunov_spectrum_lorenz(t_max = 200)
#' }
#'
#' @export
lyapunov_spectrum_continuous <- function(deriv_fn, jac_fn, x0,
                                          t_max = 1000, dt = 0.01,
                                          qr_interval = 1.0,
                                          transient = 100) {
  checkmate::assert_function(deriv_fn)
  checkmate::assert_function(jac_fn)
  checkmate::assert_numeric(x0, any.missing = FALSE, min.len = 1L)
  checkmate::assert_number(t_max,       lower = 1e-6, finite = TRUE)
  checkmate::assert_number(dt,          lower = 1e-9, finite = TRUE)
  checkmate::assert_number(qr_interval, lower = dt,   finite = TRUE)
  checkmate::assert_number(transient,   lower = 0,    finite = TRUE)

  d <- length(x0)
  x <- as.numeric(x0)

  rk4_orbit <- function(x, t) {
    k1 <- deriv_fn(t,          x)
    k2 <- deriv_fn(t + dt / 2, x + dt / 2 * k1)
    k3 <- deriv_fn(t + dt / 2, x + dt / 2 * k2)
    k4 <- deriv_fn(t + dt,     x + dt     * k3)
    x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }

  t_cur <- 0
  n_trans_steps <- as.integer(round(transient / dt))
  for (k in seq_len(n_trans_steps)) {
    x <- rk4_orbit(x, t_cur)
    t_cur <- t_cur + dt
  }

  V          <- diag(1, nrow = d, ncol = d)
  lambda_sum <- numeric(d)
  n_steps    <- as.integer(round(t_max / dt))
  qr_every   <- as.integer(round(qr_interval / dt))

  for (k in seq_len(n_steps)) {
    # Joint RK4 for (x, V).
    k1x <- deriv_fn(t_cur,            x);  k1V <- jac_fn(t_cur,            x)  %*% V
    x2  <- x + dt / 2 * k1x;               V2  <- V + dt / 2 * k1V
    t2  <- t_cur + dt / 2
    k2x <- deriv_fn(t2, x2);               k2V <- jac_fn(t2, x2)               %*% V2
    x3  <- x + dt / 2 * k2x;               V3  <- V + dt / 2 * k2V
    k3x <- deriv_fn(t2, x3);               k3V <- jac_fn(t2, x3)               %*% V3
    x4  <- x + dt     * k3x;               V4  <- V + dt     * k3V
    t4  <- t_cur + dt
    k4x <- deriv_fn(t4, x4);               k4V <- jac_fn(t4, x4)               %*% V4

    x <- x + dt / 6 * (k1x + 2 * k2x + 2 * k3x + k4x)
    V <- V + dt / 6 * (k1V + 2 * k2V + 2 * k3V + k4V)
    t_cur <- t4

    if (k %% qr_every == 0L) {
      qd <- qr(V)
      Q  <- qr.Q(qd)
      R  <- qr.R(qd)
      signs <- sign(diag(R))
      signs[signs == 0] <- 1
      Q  <- Q %*% diag(signs, nrow = d, ncol = d)
      R  <- diag(signs, nrow = d, ncol = d) %*% R
      lambda_sum <- lambda_sum + log(abs(diag(R)))
      V <- Q
    }
  }

  lambda_sum / t_max
}

#' Lyapunov spectrum of the Lorenz system
#'
#' Convenience wrapper around [lyapunov_spectrum_continuous()]. At the
#' canonical parameters (\eqn{\sigma = 10}, \eqn{\rho = 28},
#' \eqn{\beta = 8/3}) the spectrum is approximately
#' \eqn{(0.906, 0, -14.572)}, summing to \eqn{-\sigma - 1 - \beta}.
#'
#' @param t_max,dt,qr_interval,transient As in
#'   [lyapunov_spectrum_continuous()].
#' @param sigma,rho,beta Lorenz parameters.
#' @param x0,y0,z0 Initial condition.
#'
#' @return Length-3 numeric vector of Lyapunov exponents.
#' @seealso [lyapunov_spectrum_continuous()], [simulate_lorenz()].
#' @examples
#' \donttest{
#' lyapunov_spectrum_lorenz(t_max = 200)
#' }
#' @export
lyapunov_spectrum_lorenz <- function(t_max = 1000, dt = 0.01,
                                      qr_interval = 1.0, transient = 50,
                                      sigma = 10, rho = 28, beta = 8 / 3,
                                      x0 = 1, y0 = 1, z0 = 1.05) {
  deriv_fn <- function(t, s) {
    c(sigma * (s[2L] - s[1L]),
      s[1L] * (rho - s[3L]) - s[2L],
      s[1L] * s[2L] - beta * s[3L])
  }
  jac_fn <- function(t, s) {
    matrix(c(-sigma,     sigma, 0,
              rho - s[3L], -1,  -s[1L],
              s[2L],     s[1L], -beta),
           nrow = 3, byrow = TRUE)
  }
  lyapunov_spectrum_continuous(deriv_fn, jac_fn,
                                c(x0, y0, z0), t_max, dt,
                                qr_interval, transient)
}

#' Lyapunov spectrum of the Rossler system
#'
#' Convenience wrapper around [lyapunov_spectrum_continuous()]. At the
#' canonical parameters (\eqn{a = 0.2}, \eqn{b = 0.2}, \eqn{c = 5.7}) the
#' spectrum is approximately \eqn{(0.0714, 0, -5.392)}; the small positive
#' exponent makes Rossler a low-entropy chaotic flow relative to Lorenz.
#'
#' @param t_max,dt,qr_interval,transient As in
#'   [lyapunov_spectrum_continuous()].
#' @param a,b,c Rossler parameters.
#' @param x0,y0,z0 Initial condition.
#'
#' @return Length-3 numeric vector of Lyapunov exponents.
#' @seealso [lyapunov_spectrum_continuous()], [simulate_rossler()].
#' @examples
#' \donttest{
#' lyapunov_spectrum_rossler(t_max = 400)
#' }
#' @export
lyapunov_spectrum_rossler <- function(t_max = 2000, dt = 0.05,
                                       qr_interval = 1.0, transient = 100,
                                       a = 0.2, b = 0.2, c = 5.7,
                                       x0 = 0, y0 = 1, z0 = 0) {
  deriv_fn <- function(t, s) {
    c(-(s[2L] + s[3L]),
       s[1L] + a * s[2L],
       b + s[3L] * (s[1L] - c))
  }
  jac_fn <- function(t, s) {
    matrix(c(0,     -1,   -1,
             1,      a,    0,
             s[3L], 0,     s[1L] - c),
           nrow = 3, byrow = TRUE)
  }
  lyapunov_spectrum_continuous(deriv_fn, jac_fn,
                                c(x0, y0, z0), t_max, dt,
                                qr_interval, transient)
}

#' Lyapunov exponent of the logistic map
#'
#' One-dimensional special case of [lyapunov_spectrum()] for the logistic
#' map. At `r = 4` the Lyapunov exponent is the textbook value
#' \eqn{\log 2 \approx 0.693}.
#'
#' @param n_iter,transient As in [lyapunov_spectrum()].
#' @param r Logistic parameter.
#' @param x0 Initial condition in (0, 1).
#' @return Scalar Lyapunov exponent.
#' @seealso [lyapunov_spectrum()], [simulate_logistic_map()].
#' @examples
#' lyapunov_spectrum_logistic(n_iter = 5000, r = 4)
#' @export
lyapunov_spectrum_logistic <- function(n_iter = 5000L, transient = 1000L,
                                        r = 3.8, x0 = 0.2) {
  map_fn      <- function(s) r * s[1L] * (1 - s[1L])
  jacobian_fn <- function(s) matrix(r * (1 - 2 * s[1L]), nrow = 1L, ncol = 1L)
  unname(lyapunov_spectrum(map_fn, jacobian_fn, x0, n_iter, transient))
}
