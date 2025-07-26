# Chaotic Dynamical Systems

This repository collects a set of R scripts for exploring extreme events in simple chaotic maps.  It aims to provide small, self contained utilities for estimating the extremal index, analysing exceedance clusters and performing basic block--maxima and peaks--over--threshold (POT) calculations.

The repository now includes a minimal R package skeleton named `chaoticds` with `DESCRIPTION`,
`NAMESPACE` and an `R/` directory so the functions can be installed using
standard tooling such as **devtools**. All analysis scripts now load these
functions via `library(chaoticds)` rather than sourcing files directly,
so the package can be checked and installed normally.

## Repository Structure

- **`extremal-index/`** – core helpers for estimating the extremal index and hitting-time statistics.
  - `extremal_index.R` implements `threshold_exceedances()`, `cluster_exceedances()`, `extremal_index_runs()`, `extremal_index_intervals()`, `hitting_times()` and `plot_hts()`.
  - `run-extremal-index.R` shows a minimal workflow on simulated AR(1) data.
  - `hitting_evl.R` contains helper functions for hitting-time and extreme-value analyses.
- **`simulations/`** – simple chaotic map simulators.
  - `simulate-logistic-map.R` generates logistic map trajectories.
  - `simulate-henon-map.R` produces two–dimensional Hénon map orbits.
- **`analysis/`** – utilities for extreme-value calculations.
  - `block-maxima.R` extracts block maxima and fits GEV models.
  - `peaks-over-threshold.R` provides exceedance extraction, GPD fitting and the `mrl_plot()` helper.
  - `threshold-selection.R` wraps MRL and Hill diagnostics (re-using `mrl_plot()`).
  - `bootstrap-ci.R` computes bootstrap confidence intervals for the extremal index.
    - `cluster-statistics.R` summarizes cluster sizes and plots histograms.
    - `mixing-diagnostics.R` estimates ACF decay and simple mixing coefficients.
  - **`run-demo-chaos.R`** – small wrapper calling `run_demo()` from the package to run an end-to-end example and optionally render a PDF report.
- **`vignettes/`** – R Markdown tutorials: `estimating-theta-logistic.Rmd` and `block-maxima-vs-pot-henon.Rmd`.
- **`roadmap.md`** – overview of the development plan (all current items are implemented).

## Installation

1. Install R (>= 4.0).
2. Install the **renv** package to manage reproducible environments:
   ```R
   install.packages("renv")
   ```
3. Restore the pinned package versions using the provided lockfile:
   ```R
   renv::restore()
   ```
4. Install **devtools** if not already available:
   ```R
   install.packages("devtools")
   ```
5. Install the package and its dependencies from this repository:
   ```R
   devtools::install_local(".")
   library(chaoticds)
```
   The package depends on several CRAN libraries such as `assertthat`,
   `ggplot2`, `evd`, `evir`, `ismev` and `rmarkdown`, which will be pulled in
   automatically. Versions are pinned in `renv.lock` so running
   `renv::restore()` ensures a reproducible environment.

If you also plan to use the Python utilities, run `./setup-all.sh` after cloning
the repository. This script calls `renv::restore()` and `poetry install` in one
step so both toolchains are ready for testing.

Alternatively, you can install the required packages manually:
   ```R
   install.packages(c(
     "assertthat", "ggplot2",  # base utilities and plotting
     "evd", "evir", "ismev",  # extreme-value distribution fitting
     "rmarkdown"             # for PDF report generation
   ))
   ```

## Usage

Most scripts can be run directly with `Rscript`.  For example

```bash
# simulate a logistic map trajectory
Rscript simulations/simulate-logistic-map.R

# compute block maxima and fit a GEV model
Rscript analysis/block-maxima.R

# run the full demo workflow
Rscript run-demo-chaos.R
```

You can also load the package in an interactive R session:

```R
library(chaoticds)
series <- simulate_logistic_map(1000, 3.8, 0.2)
bm <- block_maxima(series, 50)
```

The extremal-index demo in `extremal-index/run-extremal-index.R` prints example estimates and plots the empirical hitting-time survival curve.

## Recurrence Plots

Recurrence plots visualize when a system revisits similar states in reconstructed phase space. They can expose:
- periodicity through diagonal lines
- regime switching in square blocks
- chaotic and unpredictable behavior as scattered points
Use `recurrence_analysis()` to compute summary measures such as recurrence rate and determinism.
An example script `analysis/recurrence-plots.R` demonstrates how to generate a
recurrence plot for the built-in `logistic_ts` dataset and extract basic
recurrence statistics.

## Example Datasets

Three small datasets ship with the package:

- `logistic_ts` – 5,000 observations from the chaotic logistic map
- `henon_ts` – 3,000 rows giving a two-dimensional Hénon map trajectory
- `ar1_ts` – 4,000 values from a stationary AR(1) process

Load them with `data(logistic_ts)` (or `henon_ts`, `ar1_ts`) to try the
analysis examples immediately.

## Python Interface

For users who prefer Python, a minimal module is provided under
`chaoticds/` with the same recurrence utilities. Dependencies are managed
with [Poetry](https://python-poetry.org/) via `pyproject.toml`.
Install the package in an isolated environment with:

```bash
poetry install
```

Then you can import the functions just like in R:

```python
from chaoticds import recurrence_analysis
props = recurrence_analysis([0.1, 0.5, 0.2, 0.9])
```

## Testing and Continuous Integration

All unit tests live under `tests/` and use the **testthat** framework. After installing the package, run them with:

```R
devtools::test()
```

A GitHub Actions workflow automatically installs dependencies, performs `R CMD check`, and executes the test suite on every push and pull request.


## Roadmap Highlights

`roadmap.md` details future additions such as:

- Simulation of logistic and Hénon maps
- Block maxima and peaks-over-threshold methods
- Threshold selection diagnostics
- Bootstrap confidence intervals for the extremal index
- Cluster statistics and mixing diagnostics
- End-to-end demo scripts and vignettes (see files under `vignettes/`)

These items will expand the repository into a comprehensive toolkit for extreme-value analysis.

## License

This project is released under the MIT License. See [LICENSE](LICENSE) for details.

## Author

Diogo Ribeiro
ESMAD – Instituto Politécnico do Porto
[ORCID](https://orcid.org/0009-0001-2022-7072)
GitHub: [DiogoRibeiro7](https://github.com/DiogoRibeiro7)
Email: dfr@esmad.ipp.pt | diogo.debastos.ribeiro@gmail.com

