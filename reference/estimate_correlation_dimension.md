# Estimate correlation dimension

Implements a simple Grassberger-Procaccia algorithm to estimate the
correlation dimension of a univariate time series.

## Usage

``` r
estimate_correlation_dimension(x, m = 2L, tau = 1L, r_vals = NULL)
```

## Arguments

- x:

  Numeric vector of observations.

- m:

  Embedding dimension. Defaults to 2.

- tau:

  Time delay for embedding. Defaults to 1.

- r_vals:

  Optional numeric vector of radii at which to compute the correlation
  sum. If \`NULL\`, a range is chosen automatically.

## Value

A list with elements \`r\`, \`C\`, and \`dimension\` containing the
radii, correlation sums, and estimated correlation dimension.

## Examples

``` r
x <- simulate_logistic_map(1000, 3.8, 0.2)
cd <- estimate_correlation_dimension(x)
cd$dimension
#> [1] 0.89551
```
