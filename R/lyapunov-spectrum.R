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
