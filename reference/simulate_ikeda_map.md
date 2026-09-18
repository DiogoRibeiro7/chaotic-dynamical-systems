# Simulate the Ikeda map

Iterates the two-dimensional Ikeda map, which models the light field in
a nonlinear ring cavity: \$\$t_n = 0.4 - 6 / (1 + x_n^2 + y_n^2),\$\$
\$\$x\_{n+1} = 1 + u (x_n \cos t_n - y_n \sin t_n),\$\$ \$\$y\_{n+1} = u
(x_n \sin t_n + y_n \cos t_n).\$\$

With the standard parameter \`u = 0.9\` the trajectory traces a strange
attractor with a teardrop-shaped support.

## Usage

``` r
simulate_ikeda_map(n, u = 0.9, x0 = 0, y0 = 0, noise_sd = 0)
```

## Arguments

- n:

  Integer. Number of iterations to generate.

- u:

  Numeric. Dissipation parameter, typically in (0, 1). Defaults to 0.9
  (chaotic regime).

- x0:

  Numeric. Initial x. Defaults to 0.

- y0:

  Numeric. Initial y. Defaults to 0.

- noise_sd:

  Numeric (\\\ge 0\\). Standard deviation of additive Gaussian noise
  applied to each component after each iteration. Defaults to 0
  (deterministic).

## Value

Data frame with columns \`x\` and \`y\` of length \`n\`.

## References

Ikeda, K. (1979). Multiple-valued stationary state and its instability
of the transmitted light by a ring cavity system. \*Optics
Communications\*, 30(2), 257-261.
[doi:10.1016/0030-4018(79)90090-7](https://doi.org/10.1016/0030-4018%2879%2990090-7)

## See also

\[simulate_henon_map()\], \[simulate_lorenz()\].

Other simulation functions:
[`simulate_duffing()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_duffing.md),
[`simulate_logistic_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map.md),
[`simulate_lorenz()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lorenz.md),
[`simulate_mackey_glass()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_mackey_glass.md),
[`simulate_rossler()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_rossler.md),
[`simulate_standard_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_standard_map.md)

## Examples

``` r
orbit <- simulate_ikeda_map(5000, u = 0.9)
plot(orbit$x, orbit$y, pch = ".",
     xlab = "x", ylab = "y",
     main = "Ikeda map (u = 0.9)")

```
