# Adaptive threshold selection

Automatically select optimal threshold based on mean residual life plot
stability.

## Usage

``` r
select_threshold_adaptive(x, quantile_range = c(0.9, 0.99), n_candidates = 20)
```

## Arguments

- x:

  Numeric vector

- quantile_range:

  Numeric vector of length 2 (min, max quantiles to test)

- n_candidates:

  Number of candidate thresholds

## Value

List with optimal threshold and diagnostics
