# Fast forced Duffing oscillator simulation (C++ implementation)

RK4 integration of the forced Duffing oscillator, mirroring
\[simulate_duffing()\]. Unlike Lorenz and Rossler the forcing introduces
explicit time dependence, so the integrator threads the internal clock
through the derivative evaluation.

## Usage

``` r
simulate_duffing_cpp(
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

  Total integration time after any transient

- dt:

  Integration step size

- x0, v0:

  Initial position and velocity

- alpha, beta, delta, gamma, omega:

  Duffing parameters

- transient:

  Integration time discarded from the start of the trajectory

## Value

DataFrame with columns t, x, v
