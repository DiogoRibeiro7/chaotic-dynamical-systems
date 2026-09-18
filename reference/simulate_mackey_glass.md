# Simulate the Mackey-Glass delay-differential equation

Integrates the Mackey-Glass equation, a one-dimensional
delay-differential equation widely used as a chaotic benchmark:
\$\$\frac{dx}{dt} = \beta \\\frac{x(t - \tau)}{1 + x(t - \tau)^n} -
\gamma\\ x(t).\$\$

With the conventional parameters (\\\beta = 0.2\\, \\\gamma = 0.1\\, \\n
= 10\\, \\\tau = 17\\) the system is chaotic with a strange attractor of
fractal dimension ~3.

## Usage

``` r
simulate_mackey_glass(
  t_max = 200,
  dt = 0.1,
  x0 = 1.2,
  beta = 0.2,
  gamma = 0.1,
  n = 10,
  tau = 17,
  transient = 0
)
```

## Arguments

- t_max:

  Numeric (\\\> 0\\). Total integration time after the transient.
  Defaults to 200.

- dt:

  Numeric (\\\> 0\\). Integration step size. Defaults to 0.1; smaller
  values give a sharper attractor at higher computational cost.

- x0:

  Numeric. Initial / history value, used for \\t \in \[-\tau, 0\]\\.
  Defaults to 1.2.

- beta, gamma:

  Numeric. Production and decay rates. Defaults to 0.2 and 0.1.

- n:

  Numeric. Hill exponent in the production term. Defaults to 10.

- tau:

  Numeric (\\\> 0\\). Delay. Defaults to 17.

- transient:

  Numeric (\\\ge 0\\). Time discarded from the beginning of the
  trajectory. Defaults to 0; set to ~\`tau \* 4\` if you want to start
  cleanly on the attractor.

## Value

Data frame with columns \`t\` and \`x\`. The first time stamp is 0.

## Details

Discretisation: explicit Euler with a fixed delay buffer of size
\`round(tau / dt)\`. The history segment \\x(t)\\ for \\t \in \[-\tau,
0\]\\ is taken to be the constant \`x0\`. The integrator produces
\`round(t_max / dt) + 1\` post-transient points.

## References

Mackey, M. C., & Glass, L. (1977). Oscillation and chaos in
physiological control systems. \*Science\*, 197(4300), 287-289.
[doi:10.1126/science.267326](https://doi.org/10.1126/science.267326)

## See also

\[simulate_lorenz()\] for the canonical continuous chaotic flow.

Other simulation functions:
[`simulate_duffing()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_duffing.md),
[`simulate_ikeda_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_ikeda_map.md),
[`simulate_logistic_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map.md),
[`simulate_lorenz()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lorenz.md),
[`simulate_rossler()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_rossler.md),
[`simulate_standard_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_standard_map.md)

## Examples

``` r
traj <- simulate_mackey_glass(t_max = 50, dt = 0.1, transient = 30)
plot(traj$t, traj$x, type = "l",
     main = "Mackey-Glass (tau = 17)", xlab = "t", ylab = "x")

```
