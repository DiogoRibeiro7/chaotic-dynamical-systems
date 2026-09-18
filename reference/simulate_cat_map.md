# Simulate the Arnold cat map

Generates an orbit for the two-dimensional Arnold cat map: x\[n+1\] =
(x\[n\] + y\[n\]) mod 1 y\[n+1\] = (x\[n\] + 2 \* y\[n\]) mod 1

## Usage

``` r
simulate_cat_map(n, x0 = 0.1, y0 = 0.1, noise_sd = 0)
```

## Arguments

- n:

  Integer. Number of iterations to generate.

- x0:

  Numeric. Initial x value. Defaults to 0.1.

- y0:

  Numeric. Initial y value. Defaults to 0.1.

- noise_sd:

  Numeric (\\\ge 0\\). Standard deviation of additive Gaussian noise
  applied to each component after each iteration; the modulo is
  re-applied after perturbation. Defaults to 0 (deterministic).

## Value

Data frame with columns \`x\` and \`y\` of length \`n\`.

## Examples

``` r
orbit <- simulate_cat_map(100)
```
