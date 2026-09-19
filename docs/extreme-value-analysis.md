# Extreme value analysis

chaoticds supports several complementary formulations of univariate extreme-value inference. The point is not to force every trajectory into one model, but to make the modelling choice explicit.

## Block maxima and the GEV family

For block size \(m\), define

\[
M_j = \max\{X_{(j-1)m+1},\ldots,X_{jm}\}.
\]

Under standard conditions, normalized maxima converge to the generalized extreme value family

\[
G(z)=\exp\left[-\left(1+\xi\frac{z-\mu}{\sigma}\right)^{-1/\xi}\right],
\]

on the support \(1+\xi(z-\mu)/\sigma>0\).

```r
bm <- block_maxima(x, block_size = 50)
gev <- fit_gev(bm)
summary(gev)
```

The block-size choice controls the bias–variance trade-off: larger blocks improve the asymptotic argument but leave fewer maxima.

## Peaks over threshold

For a sufficiently high threshold \(u\), excesses \(Y=X-u\mid X>u\) are approximated by a generalized Pareto distribution:

\[
H(y)=1-\left(1+\xi\frac{y}{\beta}\right)^{-1/\xi}.
\]

```r
u <- quantile(x, 0.95)
gpd <- fit_gpd(x, threshold = u)
summary(gpd)
```

POT usually uses more tail observations than block maxima, but threshold choice becomes the central modelling decision.

## Threshold diagnostics

The package includes `mean_residual_life()`, `mrl_plot()`, `hill_estimates()`, `hill_plot()`, `threshold_diagnostics()`, and automatic-selection helpers.

!!! warning
    A percentile such as 0.95 is a convenient starting point. It is not evidence that the GPD approximation is valid above that threshold.

## Point-process likelihood

`fit_ppp()` uses a Poisson point-process representation. This combines the frequency and magnitude of threshold exceedances in one likelihood and links naturally to GEV parameterization.

## r-largest order statistics

Ordinary block maxima keep the largest observation from every block. The r-largest likelihood keeps the largest \(r\):

```r
rl <- block_r_largest(x, block_size = 50, r = 3)
fit_rl <- fit_gev_rlargest(rl)
```

This recovers some information discarded by ordinary block maxima while retaining a block-based asymptotic framework.

## Return levels

Return levels are functions of the fitted tail parameters. Their uncertainty can be strongly asymmetric, especially for long return periods or shape estimates near support boundaries.

Use `calculate_return_levels()` for point estimates and `profile_return_level()` for likelihood-based uncertainty where supported.

## Which representation should I use?

| Question | Natural starting point |
|---|---|
| One representative extreme per time block | GEV |
| Many exceedances above a high threshold | GPD |
| Frequency and magnitude of exceedances jointly | Point process |
| More than one extreme per block | r-largest |

## Dependence is not optional

A good marginal tail fit does not imply independent extremes. For chaotic trajectories, combine the fitted model with [extremal-index analysis](extremal-index.md), cluster diagnostics, and mixing checks.