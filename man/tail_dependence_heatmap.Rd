\name{tail_dependence_heatmap}
\alias{tail_dependence_heatmap}
\title{Tail dependence heatmap}
\description{Display pairwise upper tail dependence coefficients in a heatmap.}
\usage{tail_dependence_heatmap(df, quantile_level = 0.9)}
\arguments{
 \item{df}{[data.frame] or [matrix] of numeric columns.}
 \item{quantile_level}{[numeric] High quantile level in \eqn{(0,1)}.}
}
\value{[ggplot2::ggplot] Heatmap of pairwise tail dependence coefficients.}
\examples{
set.seed(1)
df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
tail_dependence_heatmap(df)
}
