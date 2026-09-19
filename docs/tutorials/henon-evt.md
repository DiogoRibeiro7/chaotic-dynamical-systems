# Hénon map: block maxima versus POT

This tutorial compares two asymptotic views of the same chaotic trajectory.

## 1. Simulate the attractor

The Hénon map is

\[
x_{t+1}=1-a x_t^2+y_t,
\]

\[
y_{t+1}=b x_t.
\]

~~~r
library(chaoticds)

henon <- simulate_henon_map(
  n = 12000,
  a = 1.4,
  b = 0.3
)

x <- henon$x
~~~

Visualize the attractor:

~~~r
plot(
  henon$x,
  henon$y,
  pch = ".",
  xlab = "x",
  ylab = "y",
  main = "Hénon attractor"
)
~~~

The analysis below treats the \(x\)-component as the scalar observable.

## 2. Block maxima

~~~r
bm <- block_maxima(
  x,
  block_size = 100
)

gev <- fit_gev(bm)

summary(gev)
~~~

With 12,000 observations and blocks of 100, only about 120 maxima enter the fit.

That data reduction is deliberate: the asymptotic object is the block maximum.

## 3. Peaks over threshold

~~~r
u <- quantile(
  x,
  0.95
)

gpd <- fit_gpd(
  x,
  threshold = u
)

summary(gpd)
~~~

At a nominal 95th-percentile threshold, roughly 600 observations exceed \(u\) before accounting for clustering.

POT therefore retains much more tail information than ordinary block maxima.

## 4. But are those 600 exceedances independent?

Check the extremal index.

~~~r
theta <- extremal_index_runs(
  x,
  threshold = u,
  run_length = 2
)

theta
~~~

Inspect cluster sizes directly:

~~~r
sizes <- cluster_sizes(
  x,
  threshold = u,
  run_length = 2
)

cluster_summary(sizes)
~~~

If extremes cluster strongly, the effective tail information is smaller than the raw exceedance count suggests.

## 5. Threshold sensitivity

~~~r
thresholds <- quantile(
  x,
  seq(0.90, 0.99, by = 0.01)
)

mrl <- mean_residual_life(
  x,
  thresholds
)

mrl_plot(mrl)
~~~

A stable threshold region matters more than a conventional percentile.

## 6. Block-size sensitivity

Repeat the GEV fit at several block sizes.

~~~r
block_sizes <- c(
  50,
  75,
  100,
  150
)

fits <- lapply(
  block_sizes,
  function(m) {
    fit_gev(
      block_maxima(
        x,
        block_size = m
      )
    )
  }
)
~~~

The purpose is not to pick the fit with the most attractive estimate. It is to understand whether inference is stable to plausible block definitions.

## 7. Return-level uncertainty

For the GEV fit, use profile-based return-level inference.

~~~r
profile_return_level(
  gev,
  m = 100,
  level = 0.95
)
~~~

Long return periods can amplify uncertainty in the shape parameter.

## 8. Why the two methods may disagree

Potential reasons include:

- threshold too low;
- block size too small;
- too few blocks;
- too few exceedances;
- strong extremal clustering;
- support-boundary instability;
- finite-sample bias;
- non-stationarity;
- numerical optimization issues.

The correct response is diagnosis, not voting between models.

## 9. Interpretation

Block maxima and POT are complementary.

The interesting question in chaotic systems is often how much their conclusions change after dependence and clustering are treated explicitly.
