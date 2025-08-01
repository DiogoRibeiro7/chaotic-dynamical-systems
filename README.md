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
  - `simulate-tent-map.R` generates tent map trajectories.
  - `simulate-lozi-map.R` produces Lozi map orbits.
  - `simulate-cat-map.R` simulates the Arnold cat map.
  - `logistic-bifurcation.R` creates bifurcation diagram data for the logistic map.
- **`analysis/`** – utilities for extreme-value calculations.
  - `block-maxima.R` extracts block maxima and fits GEV models.
  - `peaks-over-threshold.R` provides exceedance extraction, GPD fitting and the `mrl_plot()` helper.
  - `threshold-selection.R` wraps MRL and Hill diagnostics (re-using `mrl_plot()`).
  - `bootstrap-ci.R` computes bootstrap confidence intervals for the extremal index.
    - `cluster-statistics.R` summarizes cluster sizes and plots histograms.
    - `mixing-diagnostics.R` estimates ACF decay and simple mixing coefficients.
    - `utils.R` provides helpers like `clean_extreme_data()`, `compute_autocorrelation()`
      and `with_logging()` for error logging. `with_logging()` accepts an
      optional `msg` argument so you can annotate log files when running
      scripts. The repository also includes `estimate_correlation_dimension()`
      which estimates the fractal dimension of a series using a
      Grassberger–Procaccia approach.
  - **`run-demo-chaos.R`** – small wrapper calling `run_demo()` from the package to run an end-to-end example and optionally render a PDF report.
- **`vignettes/`** – R Markdown tutorials: `estimating-theta-logistic.Rmd` and `block-maxima-vs-pot-henon.Rmd`.
- **`roadmap.md`** – overview of the development plan (all current items are implemented).

## Installation

Run the setup script to install all dependencies and initialise `renv`:

```bash
./setup.sh --minimal  # omit heavy optional packages
```

The script logs all output to `setup.log`, which you can inspect if
something goes wrong during installation.

This will install the required R packages, create the basic directory
structure and set up testing infrastructure.  You can also restore the
environment manually and install the package from source:

```R
install.packages("renv")
renv::restore()
devtools::install_local(".")
```

For the optional Python utilities, execute `./setup-all.sh`.

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
# bifurcation diagram data
r_vals <- seq(2.5, 4, length.out = 200)
bif <- logistic_bifurcation(r_vals, n_iter = 200, discard = 100)
plot(bif$r, bif$x, pch = '.', cex = 0.5)
# estimate correlation dimension
cd <- estimate_correlation_dimension(series)
cd$dimension
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

All unit tests live under `tests/` and use the **testthat** framework. After installing the package, you can run them (if `devtools` is available) with:

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
- Advanced utilities: bivariate extremal index, adaptive thresholds,
  non-stationary GEV fitting, tail dependence, spectral analysis of extremes,
  return level estimation and model validation
- End-to-end demo scripts and vignettes (see files under `vignettes/`)

These items will expand the repository into a comprehensive toolkit for extreme-value analysis.

## Citation

If you use this software, please cite it as described in
[CITATION.cff](CITATION.cff).  An R-readable citation entry is also
provided in [inst/CITATION](inst/CITATION) and can be obtained with
`citation("chaoticds")`.

## License

This project is released under the MIT License. See [LICENSE](LICENSE) for details.

## Author

Diogo Ribeiro
ESMAD – Instituto Politécnico do Porto
[ORCID](https://orcid.org/0009-0001-2022-7072)
GitHub: [DiogoRibeiro7](https://github.com/DiogoRibeiro7)
Email: dfr@esmad.ipp.pt | diogo.debastos.ribeiro@gmail.com

