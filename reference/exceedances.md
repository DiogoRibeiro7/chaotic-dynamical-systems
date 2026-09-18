# Identify exceedances above a threshold

Identify exceedances above a threshold

## Usage

``` r
exceedances(x, threshold)
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold value.

## Value

Numeric vector of exceedances (values above \`threshold\`). If no values
exceed \`threshold\` an empty vector is returned.

## See also

\[fit_gpd()\] for fitting GPD to exceedances,
\[threshold_exceedances()\] for indices of exceedances,
\[block_maxima()\] for alternative block maxima approach

## Examples

``` r
exceedances(rnorm(100), 1.5)
#> [1] 2.404653 1.757903 2.441365
```
