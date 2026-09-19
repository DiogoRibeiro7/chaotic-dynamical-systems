# Extreme value analysis

The package supports several complementary formulations of extreme-value inference.

## Block maxima and GEV

\`block_maxima()\` extracts one maximum per block, and \`fit_gev()\` fits a generalized extreme value model.

## Peaks over threshold and GPD

For a sufficiently high threshold \(u\), the exceedance \(Y=X-u\mid X>u\) is approximated by a generalized Pareto distribution,

\[
H(y)
=
1 -
\left(
1+\xi\frac{y}{\beta}
\right)^{-1/\xi},
\]

on its natural support.

Use \`exceedances()\`, \`fit_gpd()\`, \`threshold_diagnostics()\`, and the threshold-selection helpers.

## Poisson point-process likelihood

\`fit_ppp()\` fits a Poisson point-process formulation parameterized in block-maximum GEV coordinates. The profile-likelihood machinery uses the native point-process likelihood for these fits.

## r-largest order statistics

\`block_r_largest()\` retains the largest \(r\) observations per block, and \`fit_gev_rlargest()\` fits their joint likelihood.

## Non-stationary GEV

\`fit_nonstationary_gev()\` permits systematic variation in GEV parameters. Non-stationarity should follow the data-generating mechanism rather than be added only to improve in-sample fit.

## Return levels and profile likelihood

\`calculate_return_levels()\` computes return levels. For inferential work, use \`profile_likelihood()\`, \`profile_ci()\`, and \`profile_return_level()\`.

For a scalar parameter \(\psi\),

\[
L_p(\psi)
=
\max_{\lambda} L(\psi,\lambda).
\]

This supports likelihood-ratio confidence sets without relying solely on local quadratic approximations.

## Model checking

A fitted tail model should be accompanied by goodness-of-fit, threshold-sensitivity, mixing, and clustering diagnostics.
