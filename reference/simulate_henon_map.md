# Simulate the Hénon map

Generates an orbit for the two-dimensional Hénon map: x\[n+1\] = 1 - a
\* x\[n\]^2 + y\[n\] y\[n+1\] = b \* x\[n\]

## Usage

``` r
simulate_henon_map(n, a = 1.4, b = 0.3, x0 = 0, y0 = 0, noise_sd = 0)
```

## Arguments

- n:

  Integer. Number of iterations to generate.

- a:

  Numeric. Parameter \`a\` controlling nonlinearity.

- b:

  Numeric. Parameter \`b\` controlling contraction.

- x0:

  Numeric. Initial x value.

- y0:

  Numeric. Initial y value.

- noise_sd:

  Numeric (\\\ge 0\\). Standard deviation of additive Gaussian noise
  applied to each component after each iteration. Defaults to 0
  (deterministic).

## Value

Data frame with columns \`x\` and \`y\` containing the orbit. The
function stops if \`n\` is less than one.

## See also

\[simulate_logistic_map()\] for 1D logistic map, \[simulate_lozi_map()\]
for piecewise-linear alternative

## Examples

``` r
orbit <- simulate_henon_map(100, 1.4, 0.3)
```
