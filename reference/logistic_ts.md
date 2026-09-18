# Logistic Map Time Series

A time series generated from the logistic map with parameter r = 3.8,
demonstrating chaotic behavior. This dataset is useful for extreme value
analysis and testing extremal index estimation methods.

## Usage

``` r
logistic_ts
```

## Format

A numeric vector with 5000 observations

## Source

Generated using `simulate_logistic_map(n = 5000, r = 3.8, x0 = 0.2)`

## Details

Generated using the logistic map equation x\[n+1\] = r \* x\[n\] \* (1 -
x\[n\]). with r = 3.8 and initial condition x_0 = 0.2. This parameter
value produces chaotic dynamics with interesting extreme value
properties.

## Examples

``` r
data(logistic_ts)
plot(logistic_ts[1:500], type = "l", main = "Logistic Map Time Series")

hist(logistic_ts, main = "Distribution of Logistic Map Values")
```
