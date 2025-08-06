\name{extremal_index_bivariate}
\alias{extremal_index_bivariate}
\title{Bivariate extremal index}
\description{Backward-compatible wrapper around \code{extremal_index_multivariate} applied to the first two columns of a data frame or matrix.}
\usage{extremal_index_bivariate(df, thresholds, run_length = 3L)}
\arguments{
  \item{df}{[data.frame] or [matrix] with at least two numeric columns.}
  \item{thresholds}{[numeric] Vector of length two or single threshold applied to both columns.}
  \item{run_length}{[integer] Run parameter for the runs estimator.}
}
\value{[numeric] Estimated bivariate extremal index in \eqn{[0,1]} or \code{NA} when no exceedances occur.}
\seealso{\code{\link{extremal_index_multivariate}}}
\examples{
set.seed(1)
df <- data.frame(a = rnorm(1000), b = rnorm(1000))
extremal_index_bivariate(df, 0.9)
}
