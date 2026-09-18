# Plot Hill estimates

Plot Hill estimates

## Usage

``` r
hill_plot(hill_df)
```

## Arguments

- hill_df:

  Data frame as returned by \[hill_estimates()\].

## Value

ggplot object visualizing the Hill plot. Requires the \*\*ggplot2\*\*
package.

## Examples

``` r
df <- hill_estimates(rexp(1000), 1:50)
hill_plot(df)
```
