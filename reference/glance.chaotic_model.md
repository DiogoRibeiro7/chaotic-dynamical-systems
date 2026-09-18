# One-row summary of a chaoticds GEV or GPD fit

Returns the kind of single-row diagnostics summary broom's \`glance()\`
produces for ordinary \`lm\`/\`glm\` fits.

## Usage

``` r
# S3 method for class 'chaotic_model'
glance(x, ...)
```

## Arguments

- x:

  A \`chaotic_model\` returned by \[fit_gev()\] or \[fit_gpd()\].

- ...:

  Unused.

## Value

A data frame with one row and columns \`model\`, \`method\`,
\`threshold\` (NA for GEV), \`nobs\`, \`logLik\`, \`AIC\`, \`BIC\`.

## Examples

``` r
set.seed(1)
fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
generics::glance(fit)
#>   model    method threshold nobs    logLik      AIC      BIC
#> 1   gev evd::fgev        NA  500 -1572.267 3150.534 3163.178
```
