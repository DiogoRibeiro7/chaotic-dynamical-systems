#' Simulate the H\u00e9non map
#'
#' Generates an orbit for the two-dimensional H\u00e9non map:
#'   x[n+1] = 1 - a * x[n]^2 + y[n]
#'   y[n+1] = b * x[n]
#'
#' @param n Integer. Number of iterations to generate.
#' @param a Numeric. Parameter \`a\` controlling nonlinearity.
#' @param b Numeric. Parameter \`b\` controlling contraction.
#' @param x0 Numeric. Initial x value.
#' @param y0 Numeric. Initial y value.
#'
#' @return Data frame with columns \code{x} and \code{y} containing the orbit.
#' @examples
#' orbit <- simulate_henon_map(100, 1.4, 0.3)
#' @export
simulate_henon_map <- function(n, a = 1.4, b = 0.3, x0 = 0, y0 = 0) {
  checkmate::assert_int(n, lower = 1)
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

if (identical(environment(), globalenv())) {
  # Example usage when running script directly
  set.seed(123)
  orbit <- simulate_henon_map(100, 1.4, 0.3)
  print(head(orbit))
}
