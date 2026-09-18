# Extract the r largest order statistics in each block

Splits \`x\` into \`floor(length(x) / block_size)\` non-overlapping
blocks and returns the top \`r\` values from each, sorted in decreasing
order.

## Usage

``` r
block_r_largest(x, block_size, r)
```

## Arguments

- x:

  Numeric vector. The time series.

- block_size:

  Integer. Length of each block; the last partial block is discarded.

- r:

  Integer (\\\ge 1\\). Number of top values to retain per block. Must be
  \\\le\\ \`block_size\`.

## Value

A numeric matrix with \`floor(length(x) / block_size)\` rows and \`r\`
columns. Each row holds the r largest values in that block, sorted in
decreasing order so that column 1 is the block maximum.

## See also

\[block_maxima()\] for the classical r = 1 case, \[fit_gev_rlargest()\]
to fit the joint likelihood.

## Examples

``` r
x <- simulate_logistic_map(2000, r = 3.8, x0 = 0.2)
rl <- block_r_largest(x, block_size = 50, r = 3)
dim(rl)
#> [1] 40  3
```
