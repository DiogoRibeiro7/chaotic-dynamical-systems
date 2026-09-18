# Adaptive threshold selection

Computes a rolling high quantile over a sliding window to allow for
time-varying thresholds.

## Usage

``` r
adaptive_threshold_selection(x, window_size = 100L, prob = 0.95)
```

## Arguments

- x:

  Numeric vector of observations.

- window_size:

  Integer size of the rolling window.

- prob:

  Numeric probability for the quantile, default 0.95.

## Value

Numeric vector of length \`length(x) - window_size + 1\` with adaptive
thresholds.

## Examples

``` r
x <- rnorm(200)
thr <- adaptive_threshold_selection(x, 50)
```
