# Fast Mackey-Glass DDE simulation (C++ implementation)

Explicit Euler discretisation with delay buffer, matching the R
reference simulate_mackey_glass(). Cheap O(n_total) sweep; the inner
loop is dominated by the std::pow call for the Hill term.

## Usage

``` r
simulate_mackey_glass_cpp(
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

  Total integration time after the transient

- dt:

  Integration step

- x0:

  Initial / history value

- beta:

  Production rate

- gamma:

  Decay rate

- n:

  Hill exponent

- tau:

  Delay

- transient:

  Time discarded from the start

## Value

DataFrame with columns t and x
