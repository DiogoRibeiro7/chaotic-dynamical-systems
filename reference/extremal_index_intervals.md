# Intervals estimator of extremal index (Ferro & Segers)

Intervals estimator of extremal index (Ferro & Segers)

## Usage

``` r
extremal_index_intervals(x, threshold)
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold value.

## Value

Estimated extremal index. Returns \`NA\` if fewer than two exceedances
occur above \`threshold\`.

## References

Ferro, C. A. T., and Segers, J. (2003). Inference for clusters of
extreme values. Journal of the Royal Statistical Society: Series B
(Statistical Methodology), 65(2), 545-556.

## See also

\[extremal_index_runs()\] for alternative runs estimator,
\[bootstrap_extremal_index()\] for confidence intervals

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
theta <- extremal_index_intervals(x, threshold = 0.9)
theta
#> [1] 3
```
