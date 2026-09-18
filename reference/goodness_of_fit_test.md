# Goodness-of-fit test for GPD exceedances

Performs a Kolmogorov-Smirnov test comparing the empirical distribution
of exceedances to the fitted GPD model using a method-of-moments
estimate.

## Usage

``` r
goodness_of_fit_test(x, threshold)
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold.

## Value

List with KS statistic and p-value.
