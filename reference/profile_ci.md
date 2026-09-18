# Profile-likelihood confidence intervals for an extreme-value fit

Convenience wrapper that calls \[profile_likelihood()\] for each
parameter (or a chosen subset) and returns a tidy data frame.

## Usage

``` r
profile_ci(fit, parameter = NULL, level = 0.95, ...)
```

## Arguments

- fit:

  A \`chaotic_model\` returned by \[fit_gev()\], \[fit_gev_rlargest()\],
  \[fit_gpd()\], or \[fit_ppp()\].

- parameter:

  Character vector of parameter names. If \`NULL\` (default), all model
  parameters are profiled.

- level:

  Confidence level. Defaults to 0.95.

- ...:

  Passed to \[profile_likelihood()\] (e.g. \`n_points\`, \`span\`).

## Value

A data frame with columns \`parameter\`, \`estimate\`, \`lower\`,
\`upper\`. Either CI endpoint is \`NA\` if the profile curve did not
bracket the LRT cutoff within the grid span.

## Examples

``` r
set.seed(1)
x  <- evd::rgev(500, loc = 0, scale = 1, shape = 0.1)
fit <- fit_gev(x)
profile_ci(fit)
#>   parameter   estimate       lower     upper
#> 1  location 0.01528518 -0.07556076 0.1087595
#> 2     scale 0.93647546  0.87057305 1.0095436
#> 3     shape 0.10530769  0.04411193 0.1727662
```
