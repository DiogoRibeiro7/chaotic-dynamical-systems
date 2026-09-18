# Augment a chaoticds GEV or GPD fit with fitted CDF / survival columns

Returns the input data alongside model-implied CDF and survival
(exceedance-probability) values at each observation. Useful for PP /
QQ-style diagnostic plots.

## Usage

``` r
# S3 method for class 'chaotic_model'
augment(x, data = NULL, ...)
```

## Arguments

- x:

  A \`chaotic_model\` returned by \[fit_gev()\] or \[fit_gpd()\].

- data:

  Optional numeric vector. If \`NULL\` (default), the data stored in the
  fit are used. For GPD this should be the \*raw\* series (the function
  will subset to exceedances above the threshold).

- ...:

  Unused.

## Value

A data frame with columns \`index\`, \`value\`, \`cdf\`, \`survival\`,
ordered to match the input.

## Examples

``` r
set.seed(1)
fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
head(generics::augment(fit))
#>   index      value        cdf  survival
#> 1     1  0.2847762 0.47088323 0.5291168
#> 2     2 -0.1655205 0.29659608 0.7034039
#> 3     3  2.1241706 0.87586157 0.1241384
#> 4     4  2.1744894 0.88079868 0.1192013
#> 5     5  0.8653699 0.65689283 0.3431072
#> 6     6 -1.0084282 0.04099565 0.9590044
```
