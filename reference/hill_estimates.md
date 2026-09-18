# Hill estimator across k values

Computes the Hill estimator for heavy-tail index over a range of order
statistics counts \`k\`.

## Usage

``` r
hill_estimates(x, k_values)
```

## Arguments

- x:

  Numeric vector of observations (positive values).

- k_values:

  Integer vector specifying number of top order statistics.

## Value

Data frame with columns \`k\` and \`hill\` containing the estimates.
Values of \`k\` greater than \`length(x) - 1\` are ignored.

## References

Hill, B. M. (1975). A simple general approach to inference about the
tail of a distribution. The Annals of Statistics, 3(5), 1163-1174.

## Examples

``` r
hill_estimates(rexp(1000), 1:50)
#>     k       hill
#> 1   1 0.08532962
#> 2   2 0.08846097
#> 3   3 0.06738498
#> 4   4 0.16204104
#> 5   5 0.16630567
#> 6   6 0.14977899
#> 7   7 0.17838590
#> 8   8 0.20095436
#> 9   9 0.19814794
#> 10 10 0.18909303
#> 11 11 0.18957995
#> 12 12 0.18894955
#> 13 13 0.17850597
#> 14 14 0.16873169
#> 15 15 0.16824044
#> 16 16 0.15890373
#> 17 17 0.15740891
#> 18 18 0.14974834
#> 19 19 0.14910167
#> 20 20 0.14558561
#> 21 21 0.16920487
#> 22 22 0.17636399
#> 23 23 0.19693345
#> 24 24 0.19838508
#> 25 25 0.19268383
#> 26 26 0.23747759
#> 27 27 0.23114243
#> 28 28 0.22887942
#> 29 29 0.23052311
#> 30 30 0.22508757
#> 31 31 0.22479241
#> 32 32 0.23140004
#> 33 33 0.23475556
#> 34 34 0.23425747
#> 35 35 0.23388647
#> 36 36 0.23581051
#> 37 37 0.23100847
#> 38 38 0.24903441
#> 39 39 0.24394812
#> 40 40 0.25242995
#> 41 41 0.24718095
#> 42 42 0.24417455
#> 43 43 0.25971917
#> 44 44 0.25487486
#> 45 45 0.25362498
#> 46 46 0.25764938
#> 47 47 0.25908335
#> 48 48 0.26534517
#> 49 49 0.26968718
#> 50 50 0.27254194
```
