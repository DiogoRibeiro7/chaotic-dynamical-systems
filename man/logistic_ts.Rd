\name{logistic_ts}
\docType{data}
\alias{logistic_ts}
\title{Logistic Map Time Series}
\description{A time series generated from the logistic map with parameter r = 3.8.}
\usage{data(logistic_ts)}
\format{A numeric vector of length 5000.}
\source{Generated using \code{simulate_logistic_map(n = 5000, r = 3.8, x0 = 0.2)}}
\examples{data(logistic_ts)
plot(logistic_ts[1:500], type = "l")}
