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

if (identical(environment(), globalenv())) {
  # Example usage when running script directly
  set.seed(123)
  series <- simulate_logistic_map(100, 3.8, 0.2)
  print(head(series))
}
