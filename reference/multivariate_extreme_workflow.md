# End-to-end multivariate extremes workflow

Computes a practical summary for multivariate extremes combining
thresholding, pairwise tail dependence, joint exceedance diagnostics,
and a multivariate extremal index estimate.

## Usage

``` r
multivariate_extreme_workflow(
  df,
  quantile_level = 0.95,
  run_length = 3L,
  include_lower_tail = TRUE
)
```

## Arguments

- df:

  \[data.frame\] or \[matrix\] with numeric columns.

- quantile_level:

  \[numeric\] Quantile level in (0,1) used to derive per-variable
  thresholds.

- run_length:

  \[integer\] Run parameter for extremal-index clustering.

- include_lower_tail:

  \[logical\] Whether to also compute lower-tail dependence
  coefficients.

## Value

A list with components:

- thresholds:

  Named numeric vector of per-variable thresholds.

- pairwise_dependence:

  Data frame with pairwise upper/lower tail dependence.

- joint_exceedance_rate:

  Proportion of rows with at least one exceedance.

- all_exceedance_rate:

  Proportion of rows exceeding all thresholds.

- multivariate_extremal_index:

  Estimated extremal index from \[extremal_index_multivariate()\].

- settings:

  List of workflow settings for reproducibility.

## Details

\## Assumptions - Input columns are numeric observables from the same
time index. - The series are approximately stationary in the tail
region. - Thresholds are high enough for POT-style asymptotics to be
informative.

\## Notes This workflow is intended as a diagnostic entrypoint rather
than a replacement for full model validation. Always inspect sensitivity
to threshold choices and run lengths.

## Examples

``` r
set.seed(1)
x <- rnorm(1000)
y <- 0.6 * x + rnorm(1000, sd = 0.8)
z <- -0.2 * x + rnorm(1000, sd = 1.0)
df <- data.frame(x = x, y = y, z = z)

wf <- multivariate_extreme_workflow(df, quantile_level = 0.95, run_length = 3)
wf$multivariate_extremal_index
#> [1] 0.8078947
head(wf$pairwise_dependence)
#>   var1 var2 upper_tail lower_tail
#> 1    x    y       0.32       0.34
#> 2    x    z       0.02       0.00
#> 3    y    z       0.00       0.04
```
