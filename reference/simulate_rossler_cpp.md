# Fast Rossler system simulation (C++ implementation)

RK4 integration of the Rossler system, mirroring \[simulate_rossler()\].

## Usage

``` r
simulate_rossler_cpp(
  t_max = 200,
  dt = 0.05,
  x0 = 0,
  y0 = 1,
  z0 = 0,
  a = 0.2,
  b = 0.2,
  c = 5.7,
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

- a, b, c:

  Rossler parameters

- transient:

  Integration time discarded from the start of the trajectory

## Value

DataFrame with columns t, x, y, z
