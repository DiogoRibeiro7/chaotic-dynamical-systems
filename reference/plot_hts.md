# Plot empirical hitting time survival vs exponential

Plot empirical hitting time survival vs exponential

## Usage

``` r
plot_hts(times, theta)
```

## Arguments

- times:

  Numeric vector of observed hitting times.

- theta:

  Numeric extremal index estimate (rate parameter).

## Value

ggplot object. Requires the \*\*ggplot2\*\* package.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
hts <- hitting_times(x, threshold = 0.9)
theta <- extremal_index_runs(x, threshold = 0.9, run_length = 2)
plot_hts(hts, theta)
```
