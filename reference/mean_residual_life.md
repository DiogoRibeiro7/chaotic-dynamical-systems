# Mean Residual Life (MRL) values

Computes the average excess above a sequence of thresholds.

## Usage

``` r
mean_residual_life(x, thresholds)
```

## Arguments

- x:

  Numeric vector of observations.

- thresholds:

  Numeric vector of thresholds to evaluate.

## Value

Data frame with columns \`threshold\` and \`mean_excess\`. Thresholds
with no exceedances produce \`NA\` in the \`mean_excess\` column.

## Examples

``` r
mrl <- mean_residual_life(rnorm(1000), seq(0, 2, 0.2))
```
