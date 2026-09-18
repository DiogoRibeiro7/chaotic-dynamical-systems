# Simulate the forced Duffing oscillator

Integrates the periodically forced Duffing oscillator, a damped
nonlinear oscillator that exhibits chaotic motion across well-known
parameter windows. With the default parameters (\\\alpha = -1\\, \\\beta
= 1\\, \\\delta = 0.2\\, \\\gamma = 0.3\\, \\\omega = 1\\) the system
lives in the classical double-well chaotic regime.

## Usage

``` r
simulate_duffing(
  t_max = 100,
  dt = 0.05,
  x0 = 1,
  v0 = 0,
  alpha = -1,
  beta = 1,
  delta = 0.2,
  gamma = 0.3,
  omega = 1,
  transient = 0
)
```

## Arguments

- t_max:

  Numeric. Total integration time after any `transient`.

- dt:

  Numeric. Integration step size.

- x0, v0:

  Numeric. Initial position and velocity.

- alpha, beta:

  Numeric. Linear and cubic stiffness coefficients. The classical
  double-well chaotic regime uses \\\alpha \< 0\\, \\\beta \> 0\\.

- delta:

  Numeric. Damping coefficient.

- gamma, omega:

  Numeric. Forcing amplitude and angular frequency.

- transient:

  Numeric (\\\ge 0\\). Integration time discarded from the start of the
  trajectory.

## Value

Data frame with columns `t`, `x`, `v`.

## Details

The state vector is \\(x, v)\\ with \\v = \dot{x}\\ and dynamics
\$\$\dot{x} = v,\$\$ \$\$\dot{v} = -\delta v - \alpha x - \beta x^3 +
\gamma \cos(\omega t).\$\$

Unlike Lorenz and Rossler the system has explicit time dependence (the
periodic forcing), so the integrator passes `t` into the derivative.

## References

Guckenheimer, J., & Holmes, P. (1983). \*Nonlinear Oscillations,
Dynamical Systems, and Bifurcations of Vector Fields\*. Springer.
[doi:10.1007/978-1-4612-1140-2](https://doi.org/10.1007/978-1-4612-1140-2)

## See also

\[simulate_lorenz()\], \[simulate_rossler()\].

Other simulation functions:
[`simulate_ikeda_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_ikeda_map.md),
[`simulate_logistic_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map.md),
[`simulate_lorenz()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lorenz.md),
[`simulate_mackey_glass()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_mackey_glass.md),
[`simulate_rossler()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_rossler.md),
[`simulate_standard_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_standard_map.md)

## Examples

``` r
traj <- simulate_duffing(t_max = 80, dt = 0.05, transient = 20)
plot(traj$x, traj$v, type = "l", xlab = "x", ylab = "v",
     main = "Duffing oscillator (phase portrait)")

```
