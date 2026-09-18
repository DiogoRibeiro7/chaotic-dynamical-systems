# Fast logistic map simulation (C++ implementation)

Efficient C++ implementation of the logistic map for improved
performance with large datasets.

## Usage

``` r
simulate_logistic_map_cpp(n, r, x0, noise_sd = 0)
```

## Arguments

- n:

  Number of iterations

- r:

  Parameter r of the logistic map

- x0:

  Initial value

- noise_sd:

  Additive Gaussian noise SD per iteration (default 0)

## Value

Numeric vector of the time series
