# Parallel bootstrap extremal index

Parallel implementation of bootstrap extremal index estimation.

## Usage

``` r
bootstrap_extremal_index_parallel(
  x,
  threshold,
  run_length,
  B = 1000,
  n_cores = NULL,
  conf_level = 0.95
)
```

## Arguments

- x:

  Numeric vector

- threshold:

  Threshold value

- run_length:

  Run length parameter

- B:

  Number of bootstrap samples

- n_cores:

  Number of cores (default: detectCores() - 1)

- conf_level:

  Confidence level (default: 0.95)

## Value

List with estimate and confidence interval
