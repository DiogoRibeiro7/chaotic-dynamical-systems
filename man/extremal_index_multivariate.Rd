\name{extremal_index_multivariate}
\alias{extremal_index_multivariate}
\title{Multivariate extremal index}
\description{Estimate a multivariate extremal index for a data frame or matrix with two or more numeric columns using a runs estimator on joint exceedances.}
\usage{extremal_index_multivariate(df, thresholds, run_length = 3L)}
\arguments{
  \item{df}{[data.frame] or [matrix] with numeric columns.}
  \item{thresholds}{[numeric] Vector of length \code{ncol(df)} or single threshold applied to all columns.}
  \item{run_length}{[integer] Run parameter for the runs estimator.}
}
\value{[numeric] Estimated multivariate extremal index in \eqn{[0,1]} or \code{NA} when no valid estimates are available.}
\examples{
set.seed(1)
df <- data.frame(a = rnorm(1000), b = rnorm(1000), c = rnorm(1000))
extremal_index_multivariate(df, 0.9)
}
\seealso{\code{\link{extremal_index_bivariate}}}
