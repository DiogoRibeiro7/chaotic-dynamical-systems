# Simulate the Lorenz system

Integrates the three-dimensional Lorenz system, a canonical
continuous-time chaotic attractor introduced by Edward Lorenz (1963) as
a simplified model of atmospheric convection. With the classical
parameters (\\\sigma = 10\\, \\\rho = 28\\, \\\beta = 8/3\\) the
trajectory settles onto the famous butterfly-shaped strange attractor.

## Usage

``` r
simulate_lorenz(
  t_max = 50,
  dt = 0.01,
  x0 = 1,
  y0 = 1,
  z0 = 1.05,
  sigma = 10,
  rho = 28,
  beta = 8/3,
  transient = 0
)
```

## Arguments

- t_max:

  Numeric. Total integration time after any `transient`.

- dt:

  Numeric. Integration step size.

- x0, y0, z0:

  Numeric. Initial conditions. The default `(1, 1, 1.05)` lands on the
  attractor after a brief transient.

- sigma, rho, beta:

  Numeric. Lorenz parameters. Defaults are the classical chaotic regime.

- transient:

  Numeric (\\\ge 0\\). Integration time discarded from the start of the
  trajectory to let it settle onto the attractor. The returned `t`
  column starts at zero regardless.

## Value

Data frame with columns `t`, `x`, `y`, `z`.

## Details

The system is \$\$\dot{x} = \sigma (y - x),\$\$ \$\$\dot{y} = x (\rho -
z) - y,\$\$ \$\$\dot{z} = x y - \beta z.\$\$

Integration uses a fixed-step classical RK4 scheme. The defaults
(`dt = 0.01`, `t_max = 50`) give roughly five Lyapunov times of
trajectory on the attractor and are a good starting point for extreme
value analyses of any of `x`, `y`, or `z`.

## References

Lorenz, E. N. (1963). Deterministic nonperiodic flow. \*Journal of the
Atmospheric Sciences\*, 20(2), 130-141.
[doi:10.1175/1520-0469(1963)020\<0130:DNF\>2.0.CO;2](https://doi.org/10.1175/1520-0469%281963%29020%3C0130%3ADNF%3E2.0.CO%3B2)

## See also

\[simulate_rossler()\], \[simulate_duffing()\],
\[simulate_henon_map()\].

Other simulation functions:
[`simulate_duffing()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_duffing.md),
[`simulate_ikeda_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_ikeda_map.md),
[`simulate_logistic_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map.md),
[`simulate_mackey_glass()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_mackey_glass.md),
[`simulate_rossler()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_rossler.md),
[`simulate_standard_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_standard_map.md)

## Examples

``` r
traj <- simulate_lorenz(t_max = 20, dt = 0.01, transient = 5)
plot(traj$x, traj$z, type = "l", xlab = "x", ylab = "z",
     main = "Lorenz attractor (x-z projection)")


# \donttest{
# Extreme value analysis on the z-coordinate
traj <- simulate_lorenz(t_max = 500, dt = 0.01, transient = 20)
z <- traj$z
threshold <- quantile(z, 0.95)
theta <- extremal_index_runs(z, threshold, run_length = 5)
theta
#> [1] 0.1292
# }
```
