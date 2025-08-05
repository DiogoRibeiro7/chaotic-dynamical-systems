\name{plot_exceedance_clusters}
\alias{plot_exceedance_clusters}
\title{Plot bivariate exceedance clusters}
\description{Visualise exceedance clusters for two variables with cluster membership colours.}
\usage{plot_exceedance_clusters(df, thresholds, run_length = 3L)}
\arguments{
 \item{df}{[data.frame] or [matrix] with at least two numeric columns.}
 \item{thresholds}{[numeric] Vector of length two or single threshold applied to both columns.}
 \item{run_length}{[integer] Run parameter for the runs estimator.}
}
\value{[ggplot2::ggplot] Scatter plot with clusters coloured.}
\examples{
set.seed(1)
df <- data.frame(a = rnorm(100), b = rnorm(100))
plot_exceedance_clusters(df, 0.9)
}
