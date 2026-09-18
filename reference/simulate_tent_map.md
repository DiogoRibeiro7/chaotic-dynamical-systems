# Simulate the tent map

Generates a time series from the tent map: x\[n+1\] = r \* x\[n\] if
x\[n\] \< 0.5 r \* (1 - x\[n\]) otherwise

## Usage

``` r
simulate_tent_map(n, r = 2, x0 = 0.1, noise_sd = 0)
```

## Arguments

- n:

  Integer. Number of iterations to generate.

- r:

  Numeric. Slope parameter (0 \< r \<= 2). Defaults to 2.

- x0:

  Numeric. Initial value in (0, 1). Defaults to 0.1.

- noise_sd:

  Numeric (\\\ge 0\\). Standard deviation of additive Gaussian noise
  applied after each iteration. Defaults to 0 (deterministic).

## Value

Numeric vector containing the orbit of length n.

## Examples

``` r
series <- simulate_tent_map(100, r = 2, x0 = 0.1)
```
