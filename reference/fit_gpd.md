# Fit a Generalized Pareto Distribution (GPD)

Attempts to fit a GPD to the exceedances above \`threshold\`. Uses
\`evd::fpot\` if available, otherwise \`evir::gpd\` or
\`ismev::gpd.fit\` as fallbacks.

## Usage

``` r
fit_gpd(x, threshold)
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold defining exceedances.

## Value

Fitted model object with class \`chaotic_model\` plus the original
backend class. Stops with an error if none of the supporting GPD-fitting
packages (\`evd\`, \`evir\`, \`ismev\`) are installed.

## References

Pickands, J. (1975). Statistical inference using extreme order
statistics. The Annals of Statistics, 3(1), 119-131.

Davison, A. C., and Smith, R. L. (1990). Models for exceedances over
high thresholds. Journal of the Royal Statistical Society: Series B
(Methodological), 52(3), 393-425.

## See also

\[exceedances()\] for extracting exceedances,
\[threshold_diagnostics()\] for threshold selection, \[fit_gev()\] for
alternative block maxima approach

## Examples

``` r
fit_gpd(rnorm(1000), 1.5)
#> <chaotic_model>
#>   Model:  gpd
#>   Method: evd::fpot
#>   Threshold: 1.5
#>   Parameters:
#>     scale     shape 
#>  0.501355 -0.175400 
```
