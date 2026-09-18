# Threshold selection diagnostics

Provides tools for assessing appropriate thresholds for POT analysis,
including Mean Residual Life (MRL) and Hill plots.

## Usage

``` r
threshold_diagnostics(x, thresholds, k_values)
```

## Arguments

- x:

  Numeric vector of observations.

- thresholds:

  Numeric vector of candidate thresholds for MRL.

- k_values:

  Integer vector of order statistics counts for the Hill plot.

## Value

List with components \`mrl\` and \`hill\`, containing data frames for
each diagnostic. If some thresholds or \`k_values\` are invalid they are
dropped silently from the output.

## Examples

``` r
set.seed(123)
x <- rpois(1000, lambda = 3)
diag <- threshold_diagnostics(x, seq(0, 6, by = 0.5), 1:50)
mrl_plot(diag$mrl)

hill_plot(diag$hill)
```
