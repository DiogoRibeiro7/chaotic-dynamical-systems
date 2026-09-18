# Simulate the Chirikov standard map

Iterates the area-preserving Chirikov-Taylor map on the torus \\\[0,
2\pi)^2\\: \$\$p\_{n+1} = (p_n + K \sin \theta_n) \bmod 2\pi,\$\$
\$\$\theta\_{n+1} = (\theta_n + p\_{n+1}) \bmod 2\pi.\$\$

The parameter \`K\` controls the strength of the nonlinear kick. The map
is integrable at \`K = 0\`, the last invariant KAM torus disappears near
the Chirikov value \`K \simeq 0.971635\`, and the dynamics are
progressively more chaotic for larger \`K\`.

## Usage

``` r
simulate_standard_map(n, K = 1.2, p0 = 1, theta0 = 1, noise_sd = 0)
```

## Arguments

- n:

  Integer. Number of iterations to generate.

- K:

  Numeric. Kick strength. Defaults to 1.2 (well into the chaotic
  regime).

- p0:

  Numeric. Initial momentum in \\\[0, 2\pi)\\. Defaults to 1.

- theta0:

  Numeric. Initial angle in \\\[0, 2\pi)\\. Defaults to 1.

- noise_sd:

  Numeric (\\\ge 0\\). Standard deviation of additive Gaussian noise
  applied to each component after each iteration; the modulo is
  re-applied after perturbation. Defaults to 0 (deterministic).

## Value

Data frame with columns \`p\` and \`theta\` of length \`n\`.

## References

Chirikov, B. V. (1979). A universal instability of many-dimensional
oscillator systems. \*Physics Reports\*, 52(5), 263-379.
[doi:10.1016/0370-1573(79)90023-1](https://doi.org/10.1016/0370-1573%2879%2990023-1)

## See also

\[simulate_henon_map()\], \[simulate_cat_map()\].

Other simulation functions:
[`simulate_duffing()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_duffing.md),
[`simulate_ikeda_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_ikeda_map.md),
[`simulate_logistic_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map.md),
[`simulate_lorenz()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lorenz.md),
[`simulate_mackey_glass()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_mackey_glass.md),
[`simulate_rossler()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_rossler.md)

## Examples

``` r
orbit <- simulate_standard_map(2000, K = 1.2)
plot(orbit$theta, orbit$p, pch = ".",
     xlab = expression(theta), ylab = "p",
     main = "Standard map (K = 1.2)")

```
