# Fast Hénon map simulation (C++ implementation)

Efficient C++ implementation of the Hénon map for improved performance.

## Usage

``` r
simulate_henon_map_cpp(n, a = 1.4, b = 0.3, x0 = 0, y0 = 0, noise_sd = 0)
```

## Arguments

- n:

  Number of iterations

- a:

  Parameter a

- b:

  Parameter b

- x0:

  Initial x value

- y0:

  Initial y value

- noise_sd:

  Additive Gaussian noise SD per iteration (default 0)

## Value

DataFrame with x and y columns
