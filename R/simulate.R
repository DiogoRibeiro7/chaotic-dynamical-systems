#' Simulate the logistic map
#'
#' Generates a time series following the logistic map:
#'   x[n+1] = r * x[n] * (1 - x[n])
#'
#' @param n Integer. Number of iterations to generate.
#' @param r Numeric. Growth rate parameter.
#' @param x0 Numeric. Initial value in (0, 1).
#'
#' @return Numeric vector containing the generated orbit.
#' @examples
#' series <- simulate_logistic_map(100, 3.8, 0.2)
#' @export
simulate_logistic_map <- function(n, r, x0) {
  stopifnot(is.numeric(n), n > 0, is.numeric(r), is.numeric(x0))
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
#' @return Data frame with columns `x` and `y` containing the orbit.
#' @examples
#' orbit <- simulate_henon_map(100, 1.4, 0.3)
#' @export
simulate_henon_map <- function(n, a = 1.4, b = 0.3, x0 = 0, y0 = 0) {
  stopifnot(is.numeric(n), n > 0,
            is.numeric(a), is.numeric(b),
            is.numeric(x0), is.numeric(y0))
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
