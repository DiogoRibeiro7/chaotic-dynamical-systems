# Calculate return levels from GPD fit

Given a series and fitted GPD parameters, compute return levels for
specified return periods.

## Usage

``` r
calculate_return_levels(x, threshold, return_periods)
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold for exceedances.

- return_periods:

  Numeric vector of return periods.

## Value

Numeric vector of return levels corresponding to \`return_periods\`.

## Examples

``` r
x <- rnorm(1000)
rl <- calculate_return_levels(x, 2, c(10, 100))
```
