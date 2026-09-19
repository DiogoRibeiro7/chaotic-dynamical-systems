# chaoticds

**chaoticds** is an R toolkit for simulating chaotic dynamical systems and studying their extremes.

The package combines deterministic dynamics, extreme value theory, clustering diagnostics, recurrence analysis, and high-performance C++ implementations. The R implementation remains the reference specification, while compute-heavy routines can expose matching \`_cpp\` fast paths.

## What the package covers

- canonical discrete maps including logistic, Hénon, tent, Lozi, cat, standard, and Ikeda maps;
- continuous systems including Lorenz, Rössler, and Duffing dynamics, plus Mackey–Glass delay dynamics;
- coupled systems including coupled logistic-map lattices and Kuramoto oscillators;
- block-maxima, peaks-over-threshold, point-process, and r-largest inference;
- extremal-index estimation and declustering;
- threshold, mixing, recurrence, Lyapunov, and dependence diagnostics;
- profile-likelihood inference and return-level estimation;
- R/C++ parity for computationally expensive simulation paths.

## A minimal workflow

\`\`\`r
library(chaoticds)

x <- simulate_logistic_map(
  n = 5000,
  r = 3.8,
  x0 = 0.2
)

u <- quantile(x, 0.95)
theta <- extremal_index_runs(x, threshold = u, run_length = 2)
gpd <- fit_gpd(x, threshold = u)

theta
summary(gpd)
\`\`\`

The important distinction is that the extreme observations are not assumed to have appeared independently. They arise from a dynamical system, so temporal clustering and dependence are part of the statistical problem.

## Documentation map

Start with [Getting started](getting-started.md) for installation and a compact end-to-end example.

For the mathematical and methodological parts, see:

- [Dynamical systems](dynamical-systems.md)
- [Extreme value analysis](extreme-value-analysis.md)
- [Extremal index](extremal-index.md)
- [Diagnostics](diagnostics.md)

For implementation details and the public surface:

- [Performance](performance.md)
- [Function reference](reference.md)
- [Project roadmap](project-roadmap.md)

## Source and issues

The source code, tests, vignettes, and issue tracker live in the [GitHub repository](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems).
