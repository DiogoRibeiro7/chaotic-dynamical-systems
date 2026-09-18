# Profile-likelihood interval for an extreme-value return level

Returns the profile-likelihood confidence interval for the m-period
return level \`z_m\`. This is the EVT analogue of a calibrated interval
on an extrapolated quantile: where Wald intervals on \`z_m\` are
notoriously skewed (especially for long return periods), the
profile-likelihood interval respects the curvature of the likelihood
surface.

## Usage

``` r
profile_return_level(
  fit,
  m,
  level = 0.95,
  n_per_year = 1,
  exceedance_rate = NULL,
  n_points = 41L,
  span = 0.5
)
```

## Arguments

- fit:

  A \`chaotic_model\` returned by \[fit_gev()\], \[fit_gev_rlargest()\],
  \[fit_gpd()\], or \[fit_ppp()\].

- m:

  Numeric. The return period (in blocks for GEV, in years for GPD when
  \`n_per_year \> 1\`). Must be greater than 1.

- level:

  Confidence level. Defaults to 0.95.

- n_per_year:

  Numeric (\\\> 0\\). Observations per year. Only used for GPD fits;
  ignored for GEV.

- exceedance_rate:

  Numeric in (0, 1). For GPD fits, the empirical probability that the
  underlying process exceeds the threshold. If \`NULL\` (default), the
  function infers it as \`length(exceedances) / length(raw_data)\` –
  which requires that \`fit\$data\` carries the full raw series, not
  just the excesses.

- n_points:

  Integer. Number of grid points across \`z_m\`.

- span:

  Numeric (\\\> 0\\). Half-width of the grid expressed as a fraction of
  \\\|\hat{z}\_m\|\\, with a small absolute fallback when \\\hat{z}\_m\\
  is near zero. Widen if the returned CI endpoints are \`NA\`.

## Value

A \`profile_likelihood\` object (so \[print()\] and \[plot()\] work
unchanged), with \`parameter = "return_level\_\<m\>"\`, \`mle =
z_m_hat\`, and \`ci = c(lower, upper)\`.

## Details

The return level is the value exceeded on average once per \`m\`
periods. For GEV (block-based) with block size implicit in the fit,
\`m\` counts \*blocks\*. For GPD (POT-based) with \`n_per_year\`
observations per year and exceedance rate \\\zeta_u\\ = P(X \> u), \`m\`
counts \*years\* and the function uses \`m \* n_per_year \*
exceedance_rate\` as the effective rate parameter.

Internally we reparametrise the GEV / GPD log-likelihood so that \`z_m\`
is a free parameter and the others are nuisance:

\- GEV: \\\mu(z_m, \sigma, \xi) = z_m + \sigma/\xi \\ (1 - y_m^{-\xi})\\
with \\y_m = -\log(1 - 1/m)\\, falling back to \\\mu = z_m + \sigma \log
y_m\\ for the Gumbel limit \\\xi = 0\\. - GPD: \\\sigma(z_m, \xi) = \xi
(z_m - u) / ((m n_y \zeta_u)^\xi - 1)\\, with the \\\xi = 0\\ limit
handled separately.

At each fixed \`z_m\` the remaining parameters are refit by BFGS, and
the CI is read off the LRT level set just like \[profile_likelihood()\].

## Examples

``` r
set.seed(1)
fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
profile_return_level(fit, m = 100)
#> <profile_likelihood>
#>   Model:        gev_return_level
#>   Parameter:    return_level_100 (z_m)
#>   Level:        0.95
#>   MLE:          5.55771
#>   95% profile CI: [4.78415, 6.67039]
```
