# Chaotic Dynamical Systems

This repository hosts R scripts for analyzing extreme events in chaotic dynamical systems. The goal is to provide tools for estimating the extremal index, studying hitting/return times, and ultimately building a full extreme-value statistics toolbox.

## Current Contents

- `extremal-index/` – functions to estimate the extremal index and hitting time statistics, along with a demo script.
  - `extremal_index.R` defines helper functions such as `threshold_exceedances`, `cluster_exceedances`, `extremal_index_runs`, `extremal_index_intervals`, `hitting_times` and `plot_hts`. Each function is documented with roxygen comments.
  - `run-extremal-index.R` installs dependencies, generates example AR(1) data and demonstrates the estimators.
- `simulations/` – basic chaotic map simulators.
  - `simulate-logistic-map.R` produces orbits of the logistic map.
  - `simulate-henon-map.R` generates trajectories of the two-dimensional Hénon map.
- `analysis/` – extreme-value analysis utilities.
  - `block-maxima.R` computes block maxima and fits a GEV distribution.
  - `peaks-over-threshold.R` fits a Generalized Pareto distribution and provides
    mean residual life diagnostics.
  - `threshold-selection.R` offers mean residual life and Hill plots to help
    choose a threshold for POT analyses.
  - `bootstrap-ci.R` estimates extremal index confidence intervals via a block
    bootstrap.
  - `cluster-statistics.R` analyzes exceedance clusters with summaries and
    histograms.
  - `mixing-diagnostics.R` computes auto-correlation decay curves and simple
    mixing coefficient estimates.
- `run-demo-chaos.R` demonstrates the full pipeline and can produce a PDF
  report when `rmarkdown` is installed.
- `vignettes/` contains R Markdown tutorials, including
  `estimating-theta-logistic.Rmd` and
  `block-maxima-vs-pot-henon.Rmd`.
- `roadmap.md` – outlines planned modules including map simulations, block-maxima and peaks-over-threshold analysis, cluster statistics and more.

## Installation

1. Install R (>= 4.0).
2. Install the required packages:
   ```R
   install.packages(c(
     "assertthat", "ggplot2",  # base utilities and plotting
     "evd", "evir", "ismev",  # extreme-value distribution fitting
     "rmarkdown"             # for PDF report generation
   ))
   ```

## Usage

Run the demonstration script:
   ```bash
   Rscript extremal-index/run-extremal-index.R
   ```
   The script prints extremal index estimates and produces a plot comparing empirical hitting time survival with the exponential distribution.

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

