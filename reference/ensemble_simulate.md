# Run a simulator across multiple replicates

Repeats a simulator call \`n_replicates\` times, re-evaluating any
random arguments on each pass, and returns a long-format data frame with
a \`replicate\` column for easy grouped summaries.

## Usage

``` r
ensemble_simulate(expr, n_replicates, seed = NULL)
```

## Arguments

- expr:

  A call to a simulator, captured unevaluated. Random sub-expressions in
  \`expr\` are re-drawn on every replicate.

- n_replicates:

  Integer. Number of replicates to run.

- seed:

  Optional integer seed for \[set.seed()\]. Set this when you need a
  reproducible ensemble across sessions.

## Value

A data frame in long format with a leading \`replicate\` column.

## Details

The first argument is captured as an unevaluated expression (the same
mechanism \[base::replicate()\] uses), so calls such as \`runif(1)\`
inside the simulator's argument list draw a fresh value per replicate.

Results from the simulator are stacked into a single data frame:

\- If each replicate returns a numeric vector (e.g.
\[simulate_logistic_map()\] without noise), the output has columns
\`replicate\`, \`iter\`, \`x\`. - If each replicate returns a data frame
(e.g. \[simulate_henon_map()\], \[simulate_lorenz()\]), the output has
\`replicate\`, \`iter\`, and the simulator's own columns appended.

Set \`seed\` for reproducibility across the whole ensemble.

## See also

the \`simulate\_\*\` family.

## Examples

``` r
# Monte Carlo over initial conditions for the logistic map.
ens <- ensemble_simulate(
  simulate_logistic_map(200, r = 3.8, x0 = runif(1)),
  n_replicates = 5,
  seed = 1
)
head(ens)
#>   replicate iter         x
#> 1         1    1 0.2655087
#> 2         1    2 0.7410525
#> 3         1    3 0.7291961
#> 4         1    4 0.7503828
#> 5         1    5 0.7117720
#> 6         1    6 0.7795799

# Same idea for a 2D map; columns x, y are preserved.
ens2 <- ensemble_simulate(
  simulate_henon_map(100, x0 = runif(1, -0.1, 0.1)),
  n_replicates = 3,
  seed = 1
)
head(ens2)
#>   replicate iter           x           y
#> 1         1    1 -0.04689827  0.00000000
#> 2         1    2  0.99692077 -0.01406948
#> 3         1    3 -0.40546092  0.29907623
#> 4         1    4  1.06891825 -0.12163828
#> 5         1    5 -0.72125899  0.32067548
#> 6         1    6  0.59237512 -0.21637770
```
