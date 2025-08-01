\name{ar1_ts}
\docType{data}
\alias{ar1_ts}
\title{AR(1) Time Series}
\description{A time series generated from an AR(1) model with coefficient 0.7.}
\usage{data(ar1_ts)}
\format{A numeric vector with 4000 observations.}
\source{Generated using \code{arima.sim(model = list(ar = 0.7), n = 4000)}}
\examples{data(ar1_ts)
plot(ar1_ts[1:500], type = "l")}
