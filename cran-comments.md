## Test environments

The exact source tarball for this candidate is checked in GitHub Actions on:

- Ubuntu, R release
- Ubuntu, R devel
- Ubuntu, R oldrel-1
- Windows, R release
- macOS, R release
- Ubuntu, R release with the PDF manual and vignettes built

The release workflow records the exact candidate commit, source tarball
filename, byte size, and SHA-256 digest. All platform jobs download and check
that same source artifact rather than rebuilding independently.

## R CMD check results

Final check counts will be recorded after the exact merged 0.2.1 candidate
has completed the release gate.

## CRAN submission notes

This is the first CRAN submission of `chaoticds`.

The package provides simulation and extreme-value analysis tools for chaotic
dynamical systems, including block-maxima and peaks-over-threshold methods,
extremal-index estimation, threshold-exceedance dependence diagnostics,
profile-likelihood inference, recurrence analysis, Lyapunov diagnostics,
symbolic dynamics, and optional C++ fast paths.

The package does not write to the user's home directory, start network
services, or use more than two cores during package checks or examples.
