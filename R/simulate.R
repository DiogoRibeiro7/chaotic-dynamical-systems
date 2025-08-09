#' Simulate the logistic map
#'
#' Generates a time series following the logistic map:
#'   x[n+1] = r * x[n] * (1 - x[n])
#'
#' @param n Integer. Number of iterations to generate.
#' @param r Numeric. Growth rate parameter.
#' @param x0 Numeric. Initial value in (0, 1).
#'
#' @return Numeric vector containing the generated orbit. The first value of the
#'   series is `x0`; subsequent values may leave the (0,1) interval depending on
#'   `r`.
#' @examples
#' series <- simulate_logistic_map(100, 3.8, 0.2)
#' @export
simulate_logistic_map <- function(n, r, x0) {
  checkmate::assert_count(n)
  checkmate::assert_number(r)
  checkmate::assert_number(x0)
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
#' @param x0 Numeric. Initial value for the orbit.
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
  checkmate::assert_number(x0)

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
  checkmate::assert_number(r, lower = 0, upper = 2, left.open = TRUE)
  checkmate::assert_number(x0, lower = 0, upper = 1, left.open = TRUE, right.open = TRUE)
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
