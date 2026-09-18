# Check D(un) condition

Evaluates a heuristic version of Leadbetter's D(un) mixing condition by
comparing joint exceedance probabilities at lag \`r\` with the product
of marginals for a high threshold \`u_n\`.

## Usage

``` r
d_check(x, threshold, r)
```

## Arguments

- x:

  Numeric vector containing the time series.

- threshold:

  High threshold \`u_n\` for exceedances.

- r:

  Integer lag between exceedances.

## Value

Logical indicating whether the empirical estimate suggests the D(un)
condition holds at lag \`r\`. Returns \`FALSE\` when \`r\` exceeds the
length of the series.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
d_check(x, threshold = 0.9, r = 5)
#> [1] TRUE
```
