# Fast Ikeda map simulation (C++ implementation)

Efficient C++ implementation of the two-dimensional Ikeda map.

## Usage

``` r
simulate_ikeda_map_cpp(n, u = 0.9, x0 = 0, y0 = 0, noise_sd = 0)
```

## Arguments

- n:

  Number of iterations

- u:

  Dissipation parameter (default 0.9)

- x0:

  Initial x

- y0:

  Initial y

- noise_sd:

  Standard deviation of additive Gaussian noise. Defaults to 0.

## Value

DataFrame with columns x and y
