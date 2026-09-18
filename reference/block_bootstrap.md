# Moving block bootstrap resampler

Resamples a time series using overlapping blocks of fixed length.

## Usage

``` r
block_bootstrap(x, block_length)
```

## Arguments

- x:

  Numeric vector of observations.

- block_length:

  Integer length of each block.

## Value

Numeric vector of resampled observations of the same length as \`x\`.

## Examples

``` r
block_bootstrap(1:100, block_length = 10)
#>   [1] 82 83 84 85 86 87 88 89 90 91 61 62 63 64 65 66 67 68 69 70  1  2  3  4  5
#>  [26]  6  7  8  9 10 68 69 70 71 72 73 74 75 76 77 69 70 71 72 73 74 75 76 77 78
#>  [51] 62 63 64 65 66 67 68 69 70 71 36 37 38 39 40 41 42 43 44 45 63 64 65 66 67
#>  [76] 68 69 70 71 72 26 27 28 29 30 31 32 33 34 35 19 20 21 22 23 24 25 26 27 28
```
