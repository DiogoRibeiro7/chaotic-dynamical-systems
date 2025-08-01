#' Logistic Map Time Series
#'
#' A time series generated from the logistic map with parameter r = 3.8,
#' demonstrating chaotic behavior. This dataset is useful for extreme value
#' analysis and testing extremal index estimation methods.
#'
#' @format A numeric vector with 5000 observations
#' @details 
#' Generated using the logistic map equation: x_{n+1} = r * x_n * (1 - x_n)
#' with r = 3.8 and initial condition x_0 = 0.2. This parameter value
#' produces chaotic dynamics with interesting extreme value properties.
#' 
#' @source Generated using \code{simulate_logistic_map(n = 5000, r = 3.8, x0 = 0.2)}
#' @examples
#' data(logistic_ts)
#' plot(logistic_ts[1:500], type = "l", main = "Logistic Map Time Series")
#' hist(logistic_ts, main = "Distribution of Logistic Map Values")
"logistic_ts"

#' Hénon Map Trajectory
#'
#' A two-dimensional time series generated from the Hénon map with parameters
#' a = 1.4 and b = 0.3, demonstrating chaotic attractor behavior.
#'
#' @format A data frame with 3000 rows and 2 variables:
#' \describe{
#'   \item{x}{x-coordinate of the trajectory}
#'   \item{y}{y-coordinate of the trajectory}
#' }
#' @details 
#' Generated using the Hénon map equations:
#' x_{n+1} = 1 - a * x_n^2 + y_n
#' y_{n+1} = b * x_n
#' with a = 1.4, b = 0.3, and initial conditions (x_0, y_0) = (0.1, 0.1).
#' 
#' @source Generated using \code{simulate_henon_map(n = 3000, a = 1.4, b = 0.3, x0 = 0.1, y0 = 0.1)}
#' @examples
#' data(henon_ts)
#' plot(henon_ts$x, henon_ts$y, pch = ".", main = "Hénon Attractor")
#' plot(henon_ts$x[1:500], type = "l", main = "Hénon Map x-component")
"henon_ts"

#' AR(1) Time Series
#'
#' A time series generated from an autoregressive model of order 1 with
#' coefficient 0.7, useful for comparison with chaotic time series.
#'
#' @format A numeric vector with 4000 observations
#' @details 
#' Generated using the AR(1) model: x_t = 0.7 * x_{t-1} + ε_t
#' where ε_t are independent normal random variables with mean 0 and variance 1.
#' This provides a reference for non-chaotic behavior in extreme value analysis.
#' 
#' @source Generated using \code{arima.sim(model = list(ar = 0.7), n = 4000)}
#' @examples
#' data(ar1_ts)
#' plot(ar1_ts[1:500], type = "l", main = "AR(1) Time Series")
#' acf(ar1_ts, main = "AR(1) Autocorrelation Function")
"ar1_ts"
