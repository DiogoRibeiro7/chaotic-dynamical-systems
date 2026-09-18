# Fast Arnold cat map simulation (C++ implementation)

Efficient C++ implementation of the Arnold cat map on the unit torus.

## Usage

``` r
simulate_cat_map_cpp(n, x0 = 0.1, y0 = 0.1, noise_sd = 0)
```

## Arguments

- n:

  Number of iterations

- x0:

  Initial x value

- y0:

  Initial y value

- noise_sd:

  Standard deviation of additive Gaussian noise. Defaults to 0.

## Value

DataFrame with x and y columns
