# Fast Lorenz system simulation (C++ implementation)

RK4 integration of the classical Lorenz system, mirroring
\[simulate_lorenz()\].

## Usage

``` r
simulate_lorenz_cpp(
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

  Total integration time after any transient

- dt:

  Integration step size

- x0, y0, z0:

  Initial conditions

- sigma, rho, beta:

  Lorenz parameters

- transient:

  Integration time discarded from the start of the trajectory

## Value

DataFrame with columns t, x, y, z
