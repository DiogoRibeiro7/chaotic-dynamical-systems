#' Simulate a coupled map lattice
#'
#' Simulates a one-dimensional periodic lattice of diffusively coupled
#' logistic maps. The R implementation is the reference specification; use
#' simulate_coupled_map_lattice_cpp() for the same dynamics with the update
#' loop evaluated in C++.
#'
#' @details
#' For site \eqn{i} and iteration \eqn{t}, the update is
#' \deqn{x_i^{(t+1)} = (1 - \varepsilon) f(x_i^{(t)}) +
#'   \frac{\varepsilon}{2}\{f(x_{i-1}^{(t)}) + f(x_{i+1}^{(t)})\},}
#' where \eqn{f(x) = r x (1 - x)}. Site indices use periodic boundary
#' conditions. Optional Gaussian perturbations are added after the coupled
#' deterministic update.
#'
#' When x0 is NULL, the initial state is a deterministic set of distinct
#' values in (0, 1), which avoids the permanently synchronized orbit produced
#' by a constant initial state.
#'
#' @param n Integer. Number of lattice states to return, including the initial
#'   state.
#' @param lattice_size Integer. Number of sites. Must be at least 3.
#' @param r Numeric. Logistic-map parameter in [0, 4]. Defaults to 4.
#' @param coupling Numeric. Diffusive coupling strength \eqn{\varepsilon} in
#'   [0, 1]. Defaults to 0.2.
#' @param x0 NULL, a numeric scalar, or a numeric vector of length
#'   lattice_size. A scalar is replicated across sites.
#' @param noise_sd Numeric. Standard deviation of additive Gaussian noise
#'   applied independently to every site after each update. Defaults to 0.
#'
#' @return A numeric matrix with n rows and lattice_size columns. Row 1 is
#'   the initial lattice state and subsequent rows are successive iterations.
#'
#' @references
#' Kaneko, K. (1984). Period-doubling of kink-antikink patterns, quasiperiodicity
#' in antiferro-like structures and spatial intermittency in coupled logistic
#' lattice. Progress of Theoretical Physics, 72(3), 480-486.
#'
#' @examples
#' x <- simulate_coupled_map_lattice(
#'   n = 100,
#'   lattice_size = 16,
#'   r = 4,
#'   coupling = 0.2
#' )
#' matplot(x, type = "l", lty = 1, xlab = "Iteration", ylab = "State")
#'
#' @family simulation functions
#' @export
simulate_coupled_map_lattice <- function(
    n,
    lattice_size = 32L,
    r = 4,
    coupling = 0.2,
    x0 = NULL,
    noise_sd = 0) {
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_int(lattice_size, lower = 3L)
  checkmate::assert_number(r, lower = 0, upper = 4, finite = TRUE)
  checkmate::assert_number(coupling, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_number(noise_sd, lower = 0, finite = TRUE)

  initial <- .cml_initial_state(x0, lattice_size)
  out <- matrix(0, nrow = n, ncol = lattice_size)
  out[1L, ] <- initial

  if (n == 1L) {
    return(out)
  }

  noise <- .cml_noise(n, lattice_size, noise_sd)

  for (t in 2L:n) {
    previous <- out[t - 1L, ]
    mapped <- r * previous * (1 - previous)
    left <- mapped[c(lattice_size, seq_len(lattice_size - 1L))]
    right <- mapped[c(seq.int(2L, lattice_size), 1L)]

    out[t, ] <- (1 - coupling) * mapped +
      0.5 * coupling * (left + right) +
      noise[t - 1L, ]
  }

  out
}

#' Fast coupled map lattice simulation
#'
#' Uses the same model, validation, initial-state convention, and noise draws
#' as simulate_coupled_map_lattice(), while evaluating the lattice update
#' loop in C++.
#'
#' @inheritParams simulate_coupled_map_lattice
#' @return A numeric matrix with the same shape and semantics as
#'   simulate_coupled_map_lattice().
#' @rdname simulate_coupled_map_lattice
#' @export
simulate_coupled_map_lattice_cpp <- function(
    n,
    lattice_size = 32L,
    r = 4,
    coupling = 0.2,
    x0 = NULL,
    noise_sd = 0) {
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_int(lattice_size, lower = 3L)
  checkmate::assert_number(r, lower = 0, upper = 4, finite = TRUE)
  checkmate::assert_number(coupling, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_number(noise_sd, lower = 0, finite = TRUE)

  initial <- .cml_initial_state(x0, lattice_size)
  noise <- .cml_noise(n, lattice_size, noise_sd)

  .Call(
    "_chaoticds_cml_simulate_cpp_impl",
    as.integer(n),
    as.numeric(r),
    as.numeric(coupling),
    as.numeric(initial),
    noise
  )
}

.cml_initial_state <- function(x0, lattice_size) {
  if (is.null(x0)) {
    return((seq_len(lattice_size) - 0.5) / lattice_size)
  }

  checkmate::assert_numeric(x0, any.missing = FALSE, finite = TRUE)
  if (length(x0) == 1L) {
    x0 <- rep(x0, lattice_size)
  }
  if (length(x0) != lattice_size) {
    stop("x0 must be NULL, a scalar, or have length lattice_size", call. = FALSE)
  }
  if (any(x0 < 0 | x0 > 1)) {
    stop("all x0 values must lie in [0, 1]", call. = FALSE)
  }
  as.numeric(x0)
}

.cml_noise <- function(n, lattice_size, noise_sd) {
  if (n == 1L) {
    return(matrix(numeric(0), nrow = 0L, ncol = lattice_size))
  }
  if (noise_sd == 0) {
    return(matrix(0, nrow = n - 1L, ncol = lattice_size))
  }
  matrix(
    stats::rnorm((n - 1L) * lattice_size, sd = noise_sd),
    nrow = n - 1L,
    ncol = lattice_size,
    byrow = TRUE
  )
}
