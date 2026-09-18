# Fit a Poisson point-process likelihood (PPL) model

Fits the GEV-parameterised Poisson point-process likelihood of Coles
(2001, §7.4) to threshold exceedances. The PPL unifies block-maxima and
peaks-over-threshold inference: both can be derived as marginalisations
of the same Poisson point process. Compared with \[fit_gpd()\], the PPL
returns parameters \\(\mu, \sigma, \xi)\\ directly on the block-maximum
GEV scale, removing the need to back-transform GPD scale to a return
level.

## Usage

``` r
fit_ppp(x, threshold, n_per_block = 365)
```

## Arguments

- x:

  Numeric vector. The raw time series, not just the exceedances.

- threshold:

  Numeric scalar. The high threshold \\u\\.

- n_per_block:

  Numeric (\\\ge 1\\). Observations per notional block. Defaults to 365
  (annual blocks for daily data); set to 1 if you want each observation
  to count as its own block.

## Value

A \`chaotic_model\` with \`model = "ppp"\`, wrapping \`evd::fpot(model =
"pp")\` and carrying the standard \`(loc, scale, shape)\` parameters on
the block-maximum GEV scale.

## Details

For a series of length \`n_y\` observations and threshold \\u\\, with
\`n_per_block\` observations per (notional) block, the log-likelihood is
\$\$\ell(\mu, \sigma, \xi) = -\frac{n_y}{n\_{\text{pb}}} \big(1 +
\xi\\(u - \mu)/\sigma\big)^{-1/\xi} - k \log \sigma - (1 + 1/\xi)
\sum\_{i=1}^{k} \log\big(1 + \xi (x_i - \mu)/\sigma\big),\$\$ where
\\x_1, \ldots, x_k\\ are the observed exceedances and \\n\_{\text{pb}}\\
is the number of observations per block (e.g. 365 for daily series with
annual blocks). The \\\xi = 0\\ (Gumbel) case is handled separately.

The implementation wraps \`evd::fpot(model = "pp")\`, which performs the
maximum-likelihood fit by numerical optimisation. The returned
parameters are the \*annual\* (block-size \`n_per_block\`) GEV
parameters, regardless of how many observations went into the fit.

## References

Coles, S. (2001). \*An Introduction to Statistical Modeling of Extreme
Values\*. Springer, §7.4.

## See also

\[fit_gev()\] for block-maxima inference, \[fit_gpd()\] for the POT
marginal, \[profile_return_level()\] for return-level CIs from a PPL
fit.

## Examples

``` r
set.seed(1)
x <- evd::rgev(2000, loc = 0, scale = 1, shape = 0.1)
u <- quantile(x, 0.9)
fit_ppp(x, threshold = u, n_per_block = 50)
#> <chaotic_model>
#>   Model:  ppp
#>   Method: evd::fpot(model = "pp")
#>   Threshold: 2.550565
#>   Parameters:
#>       loc     scale     shape 
#>  4.722879  1.320833 -0.026787 
```
