# Profile-likelihood inference for an extreme-value fit

Compute the profile log-likelihood curve for one parameter of a fitted
GEV, r-largest GEV, GPD, or Poisson point-process (PPL) model, plus its
likelihood-ratio confidence interval. Wald (standard-error) intervals on
the shape parameter \\\xi\\ are notoriously asymmetric and
miscalibrated; profile-likelihood intervals are the standard fix.

## Usage

``` r
profile_likelihood(fit, parameter, level = 0.95, n_points = 41L, span = 4)
```

## Arguments

- fit:

  A \`chaotic_model\` returned by \[fit_gev()\], \[fit_gev_rlargest()\],
  \[fit_gpd()\], or \[fit_ppp()\].

- parameter:

  Character. Which parameter to profile. For GEV, r-largest GEV, and
  PPL: \`"location"\` / \`"scale"\` / \`"shape"\` (aliases \`"mu"\`,
  \`"sigma"\`, \`"xi"\`). For GPD: \`"scale"\` / \`"shape"\`.

- level:

  Confidence level for the CI. Defaults to 0.95.

- n_points:

  Integer. Number of grid points across the profile. Defaults to 41,
  which is dense enough for stable linear interpolation on the level set
  while staying cheap to compute.

- span:

  Numeric. Half-width of the grid in units of the parameter's standard
  error around the MLE. Defaults to 4 (a span of 4 standard errors
  typically brackets a 95 percent profile interval comfortably).

## Value

An object of class \`profile_likelihood\`, a list with:

- parameter, model, level:

  The arguments echoed back.

- grid:

  Numeric vector of values at which the parameter was fixed.

- log_lik:

  Profile log-likelihood at each grid point.

- max_log_lik:

  Log-likelihood at the unconstrained MLE.

- threshold_ll:

  The level set used to invert the LRT: \`max_log_lik - qchisq(level, 1)
  / 2\`.

- mle:

  MLE value of the profiled parameter.

- ci:

  Length-2 named numeric vector \`c(lower, upper)\`. Either endpoint is
  \`NA\` when the profile curve does not bracket the level set within
  the grid span – widen \`span\` and rerun if so.

## Details

At each fixed value of the target parameter we maximise the
corresponding model log-likelihood over the remaining parameters via
\[stats::optim()\] (BFGS) and record the profile log-likelihood. The CI
is then \$\$\\\theta : 2(\hat{\ell} - \ell\_{\text{profile}}(\theta))
\le \chi^2\_{1, \alpha}\\\$\$ with the endpoints read off the grid by
linear interpolation. The implementation uses the native likelihood for
each supported model, so \[fit_gev()\], \[fit_gev_rlargest()\],
\[fit_gpd()\], and \[fit_ppp()\] results are profiled on the likelihood
that produced their fitted parameters.

## References

Coles, S. (2001). \*An Introduction to Statistical Modeling of Extreme
Values\*. Springer, sections 2.6.5 and 3.3.3.

## See also

\[profile_ci()\] for a tidy data-frame summary,
\[bootstrap_extremal_index()\] for an alternative uncertainty
quantification approach on the extremal index.

## Examples

``` r
set.seed(1)
x  <- evd::rgev(500, loc = 0, scale = 1, shape = 0.1)
fit <- fit_gev(x)
pl  <- profile_likelihood(fit, "shape")
pl
#> <profile_likelihood>
#>   Model:        gev
#>   Parameter:    shape (xi)
#>   Level:        0.95
#>   MLE:          0.105308
#>   95% profile CI: [0.0441119, 0.172766]
plot(pl)

```
