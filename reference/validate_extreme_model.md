# Validate extreme value model

Performs a basic QQ plot comparison between empirical exceedances and
the fitted GPD model.

## Usage

``` r
validate_extreme_model(x, threshold, method = "qq")
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold for exceedances.

- method:

  Currently only "qq" is implemented.

## Value

List containing QQ plot data.
