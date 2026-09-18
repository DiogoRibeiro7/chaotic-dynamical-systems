# Fast Lozi map simulation (C++ implementation)

Efficient C++ implementation of the two-dimensional Lozi map.

## Usage

``` r
simulate_lozi_map_cpp(n, a = 1.7, b = 0.5, x0 = 0, y0 = 0, noise_sd = 0)
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

  Standard deviation of additive Gaussian noise. Defaults to 0.

## Value

DataFrame with x and y columns
