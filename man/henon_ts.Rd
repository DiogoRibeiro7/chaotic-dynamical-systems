\name{henon_ts}
\docType{data}
\alias{henon_ts}
\title{H\'enon Map Trajectory}
\description{A two-dimensional trajectory generated from the H\'enon map (a = 1.4, b = 0.3).}
\usage{data(henon_ts)}
\format{A data frame with 3000 rows and 2 variables.}
\source{Generated using \code{simulate_henon_map(n = 3000, a = 1.4, b = 0.3, x0 = 0.1, y0 = 0.1)}}
\examples{data(henon_ts)
plot(henon_ts$x, henon_ts$y, pch = ".")}
