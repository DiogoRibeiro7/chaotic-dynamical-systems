# Tidy a chaoticds GEV or GPD fit

Returns one row per fitted parameter with point estimates and (when the
fit reports them) Wald standard errors.

## Usage

``` r
# S3 method for class 'chaotic_model'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)
```

## Arguments

- x:

  A \`chaotic_model\` returned by \[fit_gev()\] or \[fit_gpd()\].

- conf.int:

  Logical. If \`TRUE\`, attach Wald confidence intervals. Defaults to
  \`FALSE\`. For better-calibrated intervals on the shape parameter use
  \[profile_ci()\] instead.

- conf.level:

  Numeric in (0, 1). Confidence level when \`conf.int\` is \`TRUE\`.
  Defaults to 0.95.

- ...:

  Unused.

## Value

A data frame with columns \`term\`, \`estimate\`, \`std.error\`, and
(when \`conf.int = TRUE\`) \`conf.low\`, \`conf.high\`.

## Examples

``` r
set.seed(1)
fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
generics::tidy(fit)
#>       term   estimate  std.error
#> 1 location 0.01528518 0.04700125
#> 2    scale 0.93647546 0.03541055
#> 3    shape 0.10530769 0.03288932
```
