\name{tail_dependence_asymmetric}
\alias{tail_dependence_asymmetric}
\alias{upper_tail_dependence}
\alias{lower_tail_dependence}
\title{Asymmetric tail dependence}
\description{Compute upper or lower tail dependence coefficients allowing different thresholds for each variable.}
\usage{
 tail_dependence_asymmetric(x, y, ux, uy, lower = FALSE)
 upper_tail_dependence(x, y, ux, uy)
 lower_tail_dependence(x, y, ux, uy)
}
\arguments{
 \item{x}{[numeric] Vector of observations.}
 \item{y}{[numeric] Vector of the same length as \code{x}.}
 \item{ux}{[numeric] Threshold for \code{x}.}
 \item{uy}{[numeric] Threshold for \code{y}.}
 \item{lower}{[logical] If \code{TRUE} compute lower tail dependence.}
}
\value{[numeric] Tail dependence coefficient in \eqn{[0,1]} or \code{NA} if the denominator is zero.}
\examples{
set.seed(1)
x <- rnorm(1000)
y <- 0.5 * x + rnorm(1000, sd = 0.5)
ux <- quantile(x, 0.95)
uy <- quantile(y, 0.9)
tail_dependence_asymmetric(x, y, ux, uy)
}
