# Plot Mean Residual Life (MRL)

Plot Mean Residual Life (MRL)

## Usage

``` r
mrl_plot(mrl_df)
```

## Arguments

- mrl_df:

  Data frame as returned by \[mean_residual_life()\].

## Value

ggplot object visualizing the MRL curve. Requires the \*\*ggplot2\*\*
package.

## Examples

``` r
df <- mean_residual_life(rnorm(1000), seq(0, 2, 0.2))
mrl_plot(df)
```
