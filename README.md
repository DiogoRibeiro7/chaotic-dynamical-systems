# Chaotic Dynamical Systems

This repository collects a set of R scripts for exploring extreme events in simple chaotic maps.  It aims to provide small, self contained utilities for estimating the extremal index, analysing exceedance clusters and performing basic block--maxima and peaks--over--threshold (POT) calculations.

The repository now includes a minimal R package skeleton named `chaoticds` with `DESCRIPTION`,
`NAMESPACE` and an `R/` directory so the functions can be installed using
standard tooling such as **devtools**.

## Repository Structure

- **`extremal-index/`** – core helpers for estimating the extremal index and hitting-time statistics.
  - `extremal_index.R` implements `threshold_exceedances()`, `cluster_exceedances()`, `extremal_index_runs()`, `extremal_index_intervals()`, `hitting_times()` and `plot_hts()`.
  - `run-extremal-index.R` shows a minimal workflow on simulated AR(1) data.
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
- **`run-demo-chaos.R`** – orchestrates the above tools into a single workflow and optionally renders a short PDF report.
- **`vignettes/`** – R Markdown tutorials: `estimating-theta-logistic.Rmd` and `block-maxima-vs-pot-henon.Rmd`.
- **`roadmap.md`** – overview of the development plan (all current items are implemented).

## Installation

1. Install R (>= 4.0).
2. Install devtools if not already available:
   ```R
   install.packages("devtools")
   ```
3. Install the package and its dependencies from this repository:
   ```R
   devtools::install_local(".")
   library(chaoticds)
```
   The package depends on several CRAN libraries such as `assertthat`,
   `ggplot2`, `evd`, `evir`, `ismev` and `rmarkdown`, which will be pulled in
   automatically.

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

The extremal-index demo in `extremal-index/run-extremal-index.R` prints example estimates and plots the empirical hitting-time survival curve.

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

