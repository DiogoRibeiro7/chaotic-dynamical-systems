# Fast Chirikov standard map simulation (C++ implementation)

Efficient C++ implementation of the area-preserving Chirikov-Taylor map
on the torus \[0, 2\*pi)^2.

## Usage

``` r
simulate_standard_map_cpp(n, K = 1.2, p0 = 1, theta0 = 1, noise_sd = 0)
```

## Arguments

- n:

  Number of iterations

- K:

  Kick parameter (default 1.2)

- p0:

  Initial momentum

- theta0:

  Initial angle

- noise_sd:

  Standard deviation of additive Gaussian noise. Defaults to 0.

## Value

DataFrame with columns p and theta
